(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Caqti_describe
open Caqti_errors
open Caqti_metadata
open Caqti_prereq
open Caqti_query
open Caqti_sigs
open Postgresql
open Printf

module CL = CalendarLib
let with_utc f = CL.Time_Zone.(on f UTC ())

let float_prec =
  try int_of_string (Sys.getenv "CAQTUS_POSTGRESQL_FLOAT_PRECISION")
  with Not_found -> 17

let typedesc_of_ftype = function
  | BOOL -> `Bool
  | INT2 | INT4 | INT8 -> `Int
  | FLOAT4 | FLOAT8 | NUMERIC -> `Float
  | CHAR | VARCHAR | TEXT -> `String
  | BYTEA -> `Bytes
  | DATE -> `Date
  | TIMESTAMP | TIMESTAMPTZ | ABSTIME -> `Utc
  | ft -> `Other (string_of_ftype ft)

let utc_of_timestamp s =
  let n = String.length s in
  let fmt, s =
    if n <= 19 then
      ("%F %T%:::z", s ^ "+00") else
    if s.[19] <> '.' then
      ((if n = 22 then "%F %T%:::z" else "%F %T%z"), s) else
    let s0 = String.sub s 0 19 in
    if s.[n - 5] = '+' || s.[n - 5] = '-' then
      ("%F %T%z", s0 ^ String.sub s (n - 5) 5) else
    if s.[n - 3] = '+' || s.[n - 3] = '-' then
      ("%F %T%:::z", s0 ^ String.sub s (n - 3) 3) else
    ("%F %T%:::z", s0 ^ "+00") in
  with_utc (fun () -> CL.Printer.Calendar.from_fstring fmt s)

module Q = struct
  let start = Caqti_query.prepare_sql "BEGIN"
  let commit = Caqti_query.prepare_sql "COMMIT"
  let rollback = Caqti_query.prepare_sql "ROLLBACK"
end

module Param = struct
  type t = string
  let null = null
  let option f = function None -> null | Some x -> f x
  let bool x = string_of_bool x
  let int x = string_of_int x
  let int32 x = Int32.to_string x
  let int64 x = Int64.to_string x
  let float x = sprintf "%.*g" float_prec x
  let string s = s
  let bytes s = Bytes.to_string s
  let sub_bytes s i n = Bytes.sub_string s i n
  let date_string s = s
  let date_tuple = iso8601_of_datetuple
  let date_cl t = CL.Printer.Date.sprint "%F" t
  let utc_float t =
    let t' = CL.Calendar.from_unixfloat t in
    with_utc (fun () -> CL.Printer.Calendar.sprint "%F %T%z" t')
  let utc_string = string
  let utc_cl t = with_utc (fun () -> CL.Printer.Calendar.sprint "%F %T%z" t)

  (* deprecated *)
  let date = date_cl
  let utc = utc_cl
  let other s = s
end

module Tuple = struct

  type t = int * Postgresql.result

  let length (_, r) = r#nfields

  let raw j (i, r) =
    try r#getvalue i j with Error msg ->
    raise (Invalid_argument (string_of_error msg))

  let is_null j (i, r) =
    try r#getisnull i j with Error msg ->
    raise (Invalid_argument (string_of_error msg))
  let option f j (i, r) =
    if is_null j (i, r) then None else Some (f j (i, r))

  let bool j t =
    match raw j t with
    | "t" -> true
    | "f" -> false
    | _ -> failwith "bool_of_pgbool: Expecting \"t\" or \"f\"."
  let int j t = int_of_string (raw j t)
  let int32 j t = Int32.of_string (raw j t)
  let int64 j t = Int64.of_string (raw j t)
  let float j t = float_of_string (raw j t)
  let string j t = raw j t
  let bytes j t = Bytes.of_string (raw j t)
  let date_string j t = raw j t
  let date_tuple j t = datetuple_of_iso8601 (raw j t)
  let date_cl j t = CL.Printer.Date.from_fstring "%F" (raw j t)
  let utc_float j t = CL.Calendar.to_unixfloat (utc_of_timestamp (raw j t))
  let utc_string = raw
  let utc_cl j t = utc_of_timestamp (raw j t)

  (* deprecated *)
  let date = date_cl
  let utc = utc_cl
  let other = raw
end

(*
module Report = struct
  type t = Postgresql.result
  let returned_count = Some (fun (r : t) -> r#ntuples)
  let affected_count = Some (fun (r : t) -> int_of_string r#cmd_tuples)
end
*)

let escaped_connvalue s =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun ch -> if ch = '\\' || ch = '\'' then Buffer.add_char buf '\\';
               Buffer.add_char buf ch)
    s;
  Buffer.contents buf

let debug_quote s =
  "\"" ^ String.escaped s ^ "\""

let param_info p = Array.to_list (Array.map debug_quote p)

let tuple_info (i, r) =
  let rec loop j acc = if j < 0 then acc else
                       loop (j - 1) (debug_quote (r#getvalue i j) :: acc) in
  loop (r#nfields - 1) []

module Caqtus_functor (System : Caqti_system_sig.S) = struct

  open System

  let backend_info =
    create_backend_info
      ~uri_scheme:"postgresql" ~dialect_tag:`Pgsql
      ~parameter_style:(`Indexed (fun i -> "$" ^ string_of_int (succ i)))
      ~describe_has_typed_parameters:true
      ~describe_has_typed_fields:true
      ~has_transactions:true ()

  let query_info = make_query_info backend_info

  let prepare_failed uri q msg =
    fail (Prepare_failed (uri, query_info q, msg))
  let execute_failed uri q msg =
    fail (Execute_failed (uri, query_info q, msg))
  let miscommunication uri q fmt =
    ksprintf (fun s -> fail (Miscommunication (uri, query_info q, s))) fmt

  class connection uri =
    (* Connection URIs were introduced in version 9.2, so deconstruct URIs of
     * the form postgresql:/?query-string to provide a way to pass a connection
     * string which is valid for earlier versions. *)
    let conninfo =
      if Uri.host uri <> None then Uri.to_string uri else
      let mkparam k v = k ^ " = '" ^ escaped_connvalue v ^ "'" in
      String.concat " "
        (List.flatten (List.map (fun (k, vs) -> List.map (mkparam k) vs)
                                (Uri.query uri))) in
  object (self)
    inherit Postgresql.connection ~conninfo ()

    initializer self#set_nonblocking true

    val prepared_queries = Hashtbl.create 11

    (* Private Methods for Fetching Results *)

    method private wait_for_result =
      Unix.wrap_fd
        begin fun socket_fd ->
          let rec hold () =
            self#consume_input;
            if self#is_busy
              then Unix.poll ~read:true socket_fd >|= (fun _ -> ()) >>= hold
              else return () in
          hold ()
        end
        (Obj.magic self#socket)

    method private fetch_result_io =
      self#wait_for_result >>= fun () ->
      return self#get_result

    method private fetch_single_result_io qi =
      self#fetch_result_io >>= function
      | None -> fail (Miscommunication (uri, qi, "Missing response."))
      | Some r ->
        self#fetch_result_io >>=
        begin function
        | None -> return r
        | Some _ ->
          fail (Miscommunication (uri, qi, "Unexpected multirow response."))
        end

    (* Connection *)

    method private poll_loop_io step =
      let on_fd fd =
        let rec next = function
          | Polling_reading ->
            Unix.poll ~read:true fd  >>= fun _ -> next (step ())
          | Polling_writing ->
            Unix.poll ~write:true fd >>= fun _ -> next (step ())
          | Polling_failed | Polling_ok -> return () in
        next Polling_writing in
      Unix.wrap_fd on_fd (Obj.magic self#socket)

    method finish_connecting_io =
      self#poll_loop_io (fun () -> self#connect_poll)

    method reset_io =
      if self#reset_start then begin
        Hashtbl.clear prepared_queries;
        self#poll_loop_io (fun () -> self#reset_poll) >>= fun () ->
        return (self#status = Ok)
      end else
        return false

    method try_reset_io =
      if self#status = Ok then return true else self#reset_io

    method retry_on_failure : 'a. (unit -> 'a io) -> 'a io = fun f ->
      (try f () with
        | Postgresql.Error (Connection_failure _ as err) as exc
            when self#status <> Ok ->
          Log.warning_f "Reconnecting due to connection failure:\n%s"
                        (Postgresql.string_of_error err) >>= fun () ->
          self#reset_io >>= function true -> f () | false -> raise exc)

    method wrap_send f = self#retry_on_failure (fun () -> f (); return ())

    (* Direct Execution *)

    method exec_oneshot_io ?params ?binary_params qs =
      try
        self#wrap_send (fun () -> self#send_query ?params ?binary_params qs)
          >>= fun () ->
        self#fetch_single_result_io (`Oneshot qs)
      with
      | Postgresql.Error err ->
        let msg = Postgresql.string_of_error err in
        fail (Execute_failed (uri, `Oneshot qs, msg))
      | xc -> fail xc

    (* Prepared Execution *)

    method prepare_io q name sql =
      begin try
        self#wrap_send (fun () -> self#send_prepare name sql) >>= fun () ->
        self#fetch_single_result_io (query_info q)
      with
      | Missing_query_string ->
        prepare_failed uri q "PostgreSQL query strings are missing."
      | Postgresql.Error err ->
        prepare_failed uri q (Postgresql.string_of_error err)
      | xc -> fail xc
      end >>= fun r ->
      match r#status with
      | Command_ok -> return ()
      | Bad_response | Nonfatal_error | Fatal_error ->
        prepare_failed uri q r#error
      | _ ->
        miscommunication uri q
          "Expected Command_ok or an error as response to prepare."

    method describe_io qi name =
      self#wrap_send (fun () -> self#send_describe_prepared name) >>= fun () ->
      self#fetch_single_result_io qi >>= fun r ->
      let describe_param i =
        try typedesc_of_ftype (r#paramtype i)
        with Oid oid -> `Other ("oid" ^ string_of_int oid) in
      let describe_field i =
        let t = try typedesc_of_ftype (r#ftype i)
                with Oid oid -> `Other ("oid" ^ string_of_int oid) in
        r#fname i, t in
      let binary_params =
        let n = r#ntuples in
        let is_binary i = r#paramtype i = BYTEA in
        let rec has_binary i =
          i < n && (is_binary i || has_binary (i + 1)) in
        if has_binary 0 then Some (Array.init r#ntuples is_binary)
                        else None in
      let querydesc =
        { querydesc_params = Array.init r#nparams describe_param;
          querydesc_fields = Array.init r#nfields describe_field } in
      return (binary_params, querydesc)

    method cached_prepare_io ({pq_index; pq_name; pq_encode} as pq) =
      try return (Hashtbl.find prepared_queries pq_index)
      with Not_found ->
        let qs = pq_encode backend_info in
        self#prepare_io (Prepared pq) pq_name qs >>= fun () ->
        self#describe_io (`Prepared (pq_name, qs)) pq_name >>= fun pqinfo ->
        Hashtbl.add prepared_queries pq_index pqinfo;
        return pqinfo

    method exec_prepared_io ?params ({pq_name; _} as pq) =
      try
        self#retry_on_failure
          (fun () ->
            self#cached_prepare_io pq >|= fun (binary_params, _) ->
            self#send_query_prepared ?params ?binary_params pq_name)
          >>= fun () ->
        self#fetch_single_result_io (query_info (Prepared pq))
      with
      | Postgresql.Error err ->
        execute_failed uri (Prepared pq) (Postgresql.string_of_error err)
      | xc -> fail xc
  end

  module type CONNECTION = CONNECTION with type 'a io = 'a System.io

  let connect uri =

    (* Establish a single connection. *)
    catch
      (fun () ->
        try
          let conn = new connection uri in
          conn#finish_connecting_io >>= fun () ->
          return conn
        with Error e ->
          fail (Error e))
      (function
        | Error e ->
          fail (Connect_failed (uri, Postgresql.string_of_error e))
        | xc -> fail xc) >>=
    fun conn ->

    (* Basic check that the client doesn't use the same unpooled connection
     * from parallel cooperative threads. *)
    let in_use = ref false in
    let use f =
      assert (not !in_use);
      f conn >>= fun r ->
      in_use := false;
      return r in

    return (module struct
      type 'a io = 'a System.io
      module Param = Param
      module Tuple = Tuple

      let uri = uri
      let backend_info = backend_info

      let disconnect () = use @@ fun c -> c#finish; return ()
      let validate () = conn#try_reset_io
      let check f = f (conn#status = Ok)

      let check_command_ok q r =
        match r#status with
        | Command_ok -> return ()
        | Bad_response | Nonfatal_error | Fatal_error ->
          execute_failed uri q r#error
        | _ ->
          miscommunication uri q "Expected Command_ok or an error response."

      let check_tuples_ok q r =
        match r#status with
        | Tuples_ok -> return ()
        | Bad_response | Nonfatal_error | Fatal_error ->
          execute_failed uri q r#error
        | _ ->
          miscommunication uri q "Expected Tuples_ok or an error response."

      let describe =
        Some (fun q -> use begin fun c ->
          match q with
          | Prepared pq ->
            c#retry_on_failure (fun () -> c#cached_prepare_io pq)
              >>= fun (_, r) -> return r
          | Oneshot qsf ->
            let qs = qsf backend_info in
            c#prepare_io q "_desc_tmp" qs >>= fun () ->
            c#describe_io (`Oneshot qs) "_desc_tmp" >>= fun (_, r) ->
            c#exec_oneshot_io "DEALLOCATE _desc_tmp" >>= fun _ -> return r
        end)

      let exec_prepared params q =
        (if Log.debug_query_enabled ()
         then Log.debug_query (query_info q) (param_info params)
         else return ()) >>= fun () ->
        use begin fun c ->
          match q with
          | Oneshot qsf -> c#exec_oneshot_io ~params (qsf backend_info)
          | Prepared pp -> c#exec_prepared_io ~params pp
        end

      let exec q params =
        exec_prepared params q >>= check_command_ok q

      let find q f params =
        exec_prepared params q >>= fun r ->
        check_tuples_ok q r >>= fun () ->
        if r#ntuples = 1 then begin
          (if Log.debug_tuple_enabled ()
           then Log.debug_tuple (tuple_info (0, r))
           else return ()) >>=
          fun () -> return (f (0, r))
        end else
        miscommunication uri q "Received %d tuples, expected one." r#ntuples

      let find_opt q f params =
        exec_prepared params q >>= fun r ->
        check_tuples_ok q r >>= fun () ->
        if r#ntuples = 0 then return None else
        if r#ntuples = 1 then begin
          (if Log.debug_tuple_enabled ()
           then Log.debug_tuple (tuple_info (0, r))
           else return ()) >>=
          fun () -> return (Some (f (0, r)))
        end else
        miscommunication uri q
                         "Received %d tuples, expected at most one." r#ntuples

      let fold q f params acc =
        exec_prepared params q >>= fun r ->
        check_tuples_ok q r >>= fun () ->
        let n = r#ntuples in
        if Log.debug_tuple_enabled () then
          let rec loop i acc =
            if i = n then return acc else
            Log.debug_tuple (tuple_info (i, r)) >>= fun () ->
            loop (i + 1) (f (i, r) acc) in
          loop 0 acc
        else
          let rec loop i acc =
            if i = n then acc else
            loop (i + 1) (f (i, r) acc) in
          return (loop 0 acc)

      let fold_s q f params acc =
        exec_prepared params q >>= fun r ->
        check_tuples_ok q r >>= fun () ->
        let n = r#ntuples in
        if Log.debug_tuple_enabled () then
          let rec loop i acc =
            if i = n then return acc else
            Log.debug_tuple (tuple_info (i, r)) >>= fun () ->
            f (i, r) acc >>= loop (i + 1) in
          loop 0 acc
        else
          let rec loop i acc =
            if i = n then return acc else
            f (i, r) acc >>= loop (i + 1) in
          loop 0 acc

      let iter_s q f params =
        exec_prepared params q >>= fun r ->
        check_tuples_ok q r >>= fun () ->
        let a = r#get_all in
        let n = Array.length a in
        if Log.debug_tuple_enabled () then
          let rec loop i =
            if i = n then return () else
            Log.debug_tuple (tuple_info (i, r)) >>= fun () ->
            f (i, r) >>= fun () -> loop (i + 1) in
          loop 0
        else
          let rec loop i =
            if i = n then return () else
            f (i, r) >>= fun () -> loop (i + 1) in
          loop 0

      let iter_p q f params =
        if Log.debug_tuple_enabled () then iter_s q f params else
        exec_prepared params q >>= fun r ->
        check_tuples_ok q r >>= fun () ->
        let rec loop i acc =
          if i = 0 then acc else
          loop (i - 1) (f (i, r) :: acc) in
        join (loop r#ntuples [])

      let start () = exec Q.start [||]
      let commit () = exec Q.commit [||]
      let rollback () = exec Q.rollback [||]
    end : CONNECTION)

end

let () = Caqti_connect.register_scheme "postgresql" (module Caqtus_functor)
