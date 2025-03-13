(* Copyright (C) 2017--2025  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

[@@@alert "-caqti_private"]

open Caqti_platform
open Printf

let dialect =
  let open Caqti_template in
  let server_version =
    Version.of_string_unsafe (Sqlite3.sqlite_version_info ())
  in
  Dialect.create_sqlite ~server_version ()

let driver_info = Caqti_driver_info.of_dialect dialect

type Caqti_connection_sig.driver_connection += Driver_connection of Sqlite3.db

let get_uri_bool uri name =
  (match Uri.get_query_param uri name with
   | Some ("true" | "yes") -> Some true
   | Some ("false" | "no") -> Some false
   | Some _ ->
      ksprintf invalid_arg "Boolean expected for URI parameter %s." name
   | None -> None)

let get_uri_int uri name =
  (match Uri.get_query_param uri name with
   | Some s ->
      (try Some (int_of_string s) with
       | Failure _ ->
          ksprintf invalid_arg "Integer expected for URI parameter %s." name)
   | None -> None)

type Caqti_error.msg += Error_msg of {
  errcode: Sqlite3.Rc.t;
  extended_errcode: int option;
  errmsg: string option;
}

let cause_of_rc : Sqlite3.Rc.t * int option -> _ = function
 | CONSTRAINT, Some 1299 -> `Not_null_violation
 | CONSTRAINT, Some  787 -> `Foreign_key_violation
 | CONSTRAINT, Some 2067 -> `Unique_violation
 | CONSTRAINT, Some  275 -> `Check_violation
 | CONSTRAINT,         _ -> `Integrity_constraint_violation__don't_match
 | NOMEM,              _ -> `Out_of_memory
 | FULL,               _ -> `Disk_full
 | _,                  _ -> `Unspecified__don't_match

let () =
  let pp ppf = function
   | Error_msg {errcode; errmsg; extended_errcode; _} ->
      Format.pp_print_string ppf
        (match errmsg with
         | None -> Sqlite3.Rc.to_string errcode
         | Some errmsg -> errmsg);
      (match extended_errcode with
       | None -> Format.pp_print_char ppf '.'
       | Some erc -> Format.fprintf ppf " (ERC#%d)." erc)
   | _ -> assert false
  in
  let cause = function
   | Error_msg {errcode; extended_errcode; _} ->
      cause_of_rc (errcode, extended_errcode)
   | _ -> assert false
  in
  Caqti_error.define_msg ~pp ~cause [%extension_constructor Error_msg]

let wrap_rc ?db errcode =
  let errmsg = Option.map Sqlite3.errmsg db in
  let extended_errcode = Option.map Sqlite3.extended_errcode_int db in
  Error_msg {errcode; errmsg; extended_errcode}

let data_of_value : type a. a Caqti_type.Field.t -> a -> Sqlite3.Data.t =
  fun field_type x ->
  (match field_type with
   | Bool   -> Sqlite3.Data.INT (if x then 1L else 0L)
   | Int    -> Sqlite3.Data.INT (Int64.of_int x)
   | Int16  -> Sqlite3.Data.INT (Int64.of_int x)
   | Int32  -> Sqlite3.Data.INT (Int64.of_int32 x)
   | Int64  -> Sqlite3.Data.INT x
   | Float  -> Sqlite3.Data.FLOAT x
   | String -> Sqlite3.Data.TEXT x
   | Enum _ -> Sqlite3.Data.TEXT x
   | Octets -> Sqlite3.Data.BLOB x
   | Pdate -> Sqlite3.Data.TEXT (Conv.iso8601_of_pdate x)
   | Ptime ->
      (* This is the suggested time representation according to
         https://sqlite.org/lang_datefunc.html, and is consistent with
         current_timestamp.  Three subsecond digits are significant. *)
      let s = Ptime.to_rfc3339 ~space:true ~frac_s:3 ~tz_offset_s:0 x in
      Sqlite3.Data.TEXT (String.sub s 0 23)
   | Ptime_span ->
      Sqlite3.Data.FLOAT (Ptime.Span.to_float_s x))

(* TODO: Check integer ranges? The Int64.to_* functions don't raise. *)
let value_of_data
    : type a. uri: Uri.t ->
      a Caqti_type.Field.t -> Sqlite3.Data.t -> a =
  fun ~uri field_type data ->
  let to_ptime_span x =
    (match Ptime.Span.of_float_s x with
     | Some t -> t
     | None ->
        let msg = Caqti_error.Msg "Interval out of range for Ptime.span." in
        let typ = Caqti_type.field field_type in
        Request_utils.raise_decode_rejected ~uri ~typ msg)
  in
  let cannot_convert_to ft =
    let msg =
      Printf.sprintf "Cannot convert %s to %s."
        (Sqlite3.Data.to_string_debug data) ft
    in
    let typ = Caqti_type.field field_type in
    Request_utils.raise_decode_rejected ~uri ~typ (Caqti_error.Msg msg)
  in
  (match field_type, data with
   | Bool, Sqlite3.Data.INT y -> y <> 0L
   | Bool, _ -> cannot_convert_to "bool"
   | Int, Sqlite3.Data.INT y -> Int64.to_int y
   | Int, _ -> cannot_convert_to "int"
   | Int16, Sqlite3.Data.INT y -> Int64.to_int y
   | Int16, _ -> cannot_convert_to "int"
   | Int32, Sqlite3.Data.INT y -> Int64.to_int32 y
   | Int32, _ -> cannot_convert_to "int32"
   | Int64, Sqlite3.Data.INT y -> y
   | Int64, _ -> cannot_convert_to "int64"
   | Float, Sqlite3.Data.FLOAT y -> y
   | Float, Sqlite3.Data.INT y -> Int64.to_float y
   | Float, _ -> cannot_convert_to "float"
   | String, Sqlite3.Data.TEXT y -> y
   | String, _ -> cannot_convert_to "string"
   | Enum _, Sqlite3.Data.TEXT y -> y
   | Enum _, _ -> cannot_convert_to "enum"
   | Octets, Sqlite3.Data.BLOB y -> y
   | Octets, _ -> cannot_convert_to "octets"
   | Pdate as field_type, Sqlite3.Data.TEXT y ->
      (match Conv.pdate_of_iso8601 y with
       | Ok y -> y
       | Error msg ->
          let msg = Caqti_error.Msg msg in
          let typ = Caqti_type.field field_type in
          Request_utils.raise_decode_rejected ~uri ~typ msg)
   | Pdate, _ -> cannot_convert_to "date"
   | Ptime as field_type, Sqlite3.Data.TEXT y ->
      (* TODO: Improve parsing. *)
      (match Conv.ptime_of_rfc3339_utc y with
       | Ok y -> y
       | Error msg ->
          let msg = Caqti_error.Msg msg in
          let typ = Caqti_type.field field_type in
          Request_utils.raise_decode_rejected ~uri ~typ msg)
   | Ptime, _ -> cannot_convert_to "time"
   | Ptime_span, Sqlite3.Data.FLOAT x ->
      to_ptime_span x
   | Ptime_span, Sqlite3.Data.INT x ->
      to_ptime_span (Int64.to_float x)
   | Ptime_span, _ -> cannot_convert_to "time span")

let query_quotes q =
  let open Caqti_template in
  let rec loop : Query.t -> _ = function
   | L _ | P _ | E _ -> Fun.id
   | V (t, v) -> List.cons (data_of_value t v)
   | Q s -> List.cons (Sqlite3.Data.TEXT s)
   | S qs -> List_ext.fold loop qs
  in
  List.rev (loop q [])

let query_string q =
  let open Caqti_template in
  let quotes = query_quotes q in
  let buf = Buffer.create 64 in
  let iQ = ref 1 in
  let iP0 = List.length quotes + 1 in
  let rec loop : Query.t -> _ = function
   | L s -> Buffer.add_string buf s
   | V _ -> bprintf buf "?%d" !iQ; incr iQ
   | Q _ -> bprintf buf "?%d" !iQ; incr iQ
   | P i -> bprintf buf "?%d" (iP0 + i)
   | E _ -> assert false
   | S qs -> List.iter loop qs
  in
  loop q;
  (quotes, Buffer.contents buf)

let bind_quotes ~uri ~db stmt oq =
  let aux j x =
    (match Sqlite3.bind stmt (j + 1) x with
     | Sqlite3.Rc.OK -> Ok ()
     | rc ->
        let typ = Caqti_type.string in
        Error (Caqti_error.encode_failed ~uri ~typ (wrap_rc ~db rc)))
  in
  List_ext.iteri_r aux oq

let encode_null_field ~uri ~db stmt field_type iP =
  (match Sqlite3.bind stmt (iP + 1) Sqlite3.Data.NULL with
   | Sqlite3.Rc.OK -> ()
   | rc ->
      let typ = Caqti_type.field field_type in
      Request_utils.raise_encode_failed ~uri ~typ (wrap_rc ~db rc))

let encode_field ~uri ~db stmt field_type field_value iP =
  let d = data_of_value field_type field_value in
  (match Sqlite3.bind stmt (iP + 1) d with
   | Sqlite3.Rc.OK -> ()
   | rc ->
      let typ = Caqti_type.field field_type in
      Request_utils.raise_encode_failed ~uri ~typ (wrap_rc ~db rc))

let encode_param ~uri ~db stmt t =
  let write_value ~uri ft fv iP = encode_field ~uri ~db stmt ft fv iP; iP + 1 in
  let write_null ~uri ft iP = encode_null_field ~uri ~db stmt ft iP; iP + 1 in
  let encode = Request_utils.encode_param ~uri {write_value; write_null} t in
  fun x iP ->
    try Ok (encode x iP) with
     | Caqti_error.Exn (#Caqti_error.call as err) -> Error err

let decode_row ~uri ~query row_type =
  let read_value ~uri ft (stmt, j) =
    let fv = value_of_data ~uri ft (Sqlite3.column stmt j) in
    (fv, (stmt, j + 1))
  in
  let skip_null n (stmt, j) =
    let j' = j + n in
    let rec check k =
      k = j' || Sqlite3.column stmt k = Sqlite3.Data.NULL && check (k + 1)
    in
    if check j then Some (stmt, j') else None
  in
  let field_decoder = {Request_utils.read_value; skip_null} in
  let decode = Request_utils.decode_row ~uri field_decoder row_type in
  fun stmt ->
    let (y, (_, n)) = decode (stmt, 0) in
    let n' = Sqlite3.data_count stmt in
    if n = n' then Some y else
    let msg = sprintf "Decoded only %d of %d fields." n n' in
    let msg = Caqti_error.Msg msg in
    Request_utils.raise_response_rejected ~uri ~query msg

module Q = struct
  open Caqti_template.Create

  let start = static T.(unit -->. unit) "BEGIN"
  let commit = static T.(unit -->. unit) "COMMIT"
  let rollback = static T.(unit -->. unit) "ROLLBACK"
end

module Connect_functor
  (System : Caqti_platform.System_sig.S)
  (System_unix : Caqti_platform_unix.System_sig.S
    with type 'a fiber := 'a System.Fiber.t
     and type stdenv := System.stdenv) =
struct
  open System
  open System_unix
  open System.Fiber.Infix
  open System_utils.Monad_syntax (System.Fiber)
  module H = Connection_utils.Make_helpers (System)

  let driver_info = driver_info

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a fiber := 'a Fiber.t
     and type ('a, 'err) stream := ('a, 'err) Stream.t

  module Make_connection_base
    (Connection_arg : sig
      val subst : Caqti_template.Query.subst
      val uri : Uri.t
      val db : Sqlite3.db
      val dynamic_capacity : int
    end) =
  struct
    open Connection_arg

    let using_db_ref = ref false
    let using_db f =
      H.assert_single_use ~what:"SQLite connection" using_db_ref f

    module Response = struct

      type ('b, 'm) t = {
        stmt: Sqlite3.stmt;
        row_type: 'b Caqti_type.t;
        query: string;
        mutable affected_count: int;
        mutable has_been_executed: bool;
      }

      let returned_count _ = Fiber.return (Error `Unsupported)

      let affected_count {affected_count; has_been_executed; query; _} =
        if has_been_executed then Fiber.return (Ok affected_count) else
        let msg =
          Caqti_error.Msg
            "Statement not executed yet, affected_count unavailable."
        in
        Fiber.return (Error (Caqti_error.response_rejected ~uri ~query msg))

      let run_step response =
        let ret = Sqlite3.step response.stmt in
        if not response.has_been_executed then
          begin
            response.has_been_executed <- true;
            response.affected_count <- Sqlite3.changes db
          end;
        ret

      let fetch_row ({stmt; row_type; query; _} as response) =
        let decode = decode_row ~uri ~query row_type in
        fun () ->
          (match run_step response with
           | Sqlite3.Rc.DONE -> None
           | Sqlite3.Rc.ROW -> decode stmt
           | rc ->
              Request_utils.raise_response_failed ~uri ~query (wrap_rc ~db rc))

      let exec ({row_type; query; _} as response) =
        assert (Caqti_type.unify row_type Caqti_type.unit <> None);
        let retrieve () =
          (match run_step response with
           | Sqlite3.Rc.DONE -> Ok ()
           | Sqlite3.Rc.ROW ->
              let msg = Caqti_error.Msg "Received unexpected row for exec." in
              Error (Caqti_error.response_rejected ~uri ~query msg)
           | rc ->
              Error (Caqti_error.response_failed ~uri ~query (wrap_rc ~db rc)))
        in
        Preemptive.detach retrieve ()

      let find resp =
        let retrieve () =
          try
            (match fetch_row resp () with
             | None ->
                let msg = Caqti_error.Msg "Received no rows for find." in
                Error (Caqti_error.response_rejected ~uri ~query:resp.query msg)
             | Some y ->
                (match fetch_row resp () with
                 | None -> Ok y
                 | Some _ ->
                    let msg = "Received multiple rows for find." in
                    let msg = Caqti_error.Msg msg in
                    let query = resp.query in
                    Error (Caqti_error.response_rejected ~uri ~query msg)))
           with Caqti_error.Exn (#Caqti_error.retrieve as err) -> Error err
        in
        Preemptive.detach retrieve ()

      let find_opt resp =
        let retrieve () =
          try
            (match fetch_row resp () with
             | None -> Ok None
             | Some y ->
                (match fetch_row resp () with
                 | None -> Ok (Some y)
                 | Some _ ->
                    let msg = "Received multiple rows for find_opt." in
                    let msg = Caqti_error.Msg msg in
                    let query = resp.query in
                    Error (Caqti_error.response_rejected ~uri ~query msg)))
          with Caqti_error.Exn (#Caqti_error.retrieve as err) -> Error err
        in
        Preemptive.detach retrieve ()

      let fold f resp acc =
        let fetch = fetch_row resp in
        let retrieve acc =
          let rec loop acc =
            (match fetch () with
             | None -> Ok acc
             | Some y -> loop (f y acc))
          in
          try loop acc with
           | Caqti_error.Exn (#Caqti_error.retrieve as err) -> Error err
        in
        Preemptive.detach retrieve acc

      let fold_s f resp acc =
        let fetch = fetch_row resp in
        let retrieve acc =
          let rec loop acc =
            (match fetch () with
             | None -> Ok acc
             | Some y ->
                (match Preemptive.run_in_main (fun () -> f y acc) with
                 | Ok acc -> loop acc
                 | Error _ as r -> r))
          in
          try loop acc with
           | Caqti_error.Exn (#Caqti_error.retrieve as err) -> Error err
        in
        Preemptive.detach retrieve acc

      let iter_s f resp =
        let fetch = fetch_row resp in
        let retrieve () =
          let rec loop () =
            (match fetch () with
             | None -> Ok ()
             | Some y ->
                (match Preemptive.run_in_main (fun () -> f y) with
                 | Ok () -> loop ()
                 | Error _ as r -> r))
          in
          try loop () with
           | Caqti_error.Exn (#Caqti_error.retrieve as err) -> Error err
        in
        Preemptive.detach retrieve ()

      let to_stream resp =
        let fetch = fetch_row resp in
        let rec seq () =
          (match fetch () with
           | None -> Stream.Nil
           | Some y -> Stream.Cons (y, Preemptive.detach seq)
           | exception Caqti_error.Exn (#Caqti_error.retrieve as err) ->
              Stream.Error err)
        in
        Preemptive.detach seq
    end

    module Pcache = Request_cache.Make (struct
      type t = Sqlite3.stmt * Sqlite3.Data.t list * string
      let weight _ = 1
    end)

    let pcache = Pcache.create ~dynamic_capacity dialect

    let prepare req =
      let prepare_helper query =
        try
          let stmt = Sqlite3.prepare db query in
          (match Sqlite3.prepare_tail stmt with
           | None -> Ok stmt
           | Some stmt -> Ok stmt)
        with Sqlite3.Error msg ->
          let msg = Caqti_error.Msg msg in
          Error (Caqti_error.request_failed ~uri ~query msg)
      in

      let templ = Caqti_request.query req driver_info in
      let templ = Caqti_template.Query.expand subst templ in
      let quotes, query = query_string templ in
      Preemptive.detach prepare_helper query >|=? fun stmt ->
      (stmt, quotes, query)

    let pp_request_with_param ppf =
      Caqti_template.Request.make_pp_with_param ~subst ~dialect () ppf

    let free_prepared (stmt, _, query) =
      (match Sqlite3.finalize stmt with
       | Sqlite3.Rc.OK -> Ok ()
       | rc ->
          let query = sprintf "DEALLOCATE (%s)" query in
          Error (Caqti_error.request_failed ~uri ~query (wrap_rc ~db rc))
       | exception Sqlite3.Error msg ->
          let query = sprintf "DEALLOCATE (%s)" query in
          let msg = Caqti_error.Msg msg in
          Error (Caqti_error.request_failed ~uri ~query msg))

    let deallocate req = using_db @@ fun () ->
      (match Caqti_template.Request.prepare_policy req with
       | Dynamic | Static ->
          (match Pcache.deallocate pcache req with
           | None -> Fiber.return (Ok ())
           | Some (entry, commit) ->
              Preemptive.detach free_prepared entry >|=? commit)
       | Direct -> failwith "deallocate called on oneshot request")

    let deallocate_some () =
      let rec loop = function
       | [] -> Ok ()
       | pcache_entry :: orphans ->
          (match free_prepared pcache_entry with
           | Ok () -> loop orphans
           | r -> r)
      in
      let orphans, commit = Pcache.trim pcache in
      Preemptive.detach loop orphans >|= Result.map commit

    let call ~f req param = using_db @@ fun () ->
      deallocate_some () >>=? fun () ->
      Log.debug ~src:Logging.request_log_src (fun f ->
        f "Sending %a" pp_request_with_param (req, param))
        >>= fun () ->

      let param_type = Caqti_template.Request.param_type req in
      let row_type = Caqti_template.Request.row_type req in

      (match Caqti_template.Request.prepare_policy req with
       | Direct -> prepare req
       | Dynamic | Static ->
          (match Pcache.find_and_promote pcache req with
           | Some pcache_entry -> Fiber.return (Ok pcache_entry)
           | None ->
              prepare req >|=? fun pcache_entry ->
              Pcache.add pcache req pcache_entry;
              pcache_entry))
      >>=? fun (stmt, quotes, query) ->

      (* CHECKME: Does binding involve IO? *)
      Fiber.return (bind_quotes ~uri ~db stmt quotes) >>=? fun () ->
      let nQ = List.length quotes in
      (match encode_param ~uri ~db stmt param_type param nQ with
       | Ok nQP ->
          let nP = Caqti_type.length param_type in
          if nQP > nQ + nP then
            failwith "Too many arguments passed to query; \
                      check that the parameter type is correct."
          else
          if nQP < nQ + nP then
            failwith "Too few arguments passed to query; \
                      check that the parameter type is correct."
          else
          let resp = Response.{
            stmt; query; row_type; has_been_executed=false; affected_count = -1;
          } in
          Fiber.return (Ok resp)
       | Error _ as r -> Fiber.return r)
      >>=? fun resp ->

      (* CHECKME: Does finalize or reset involve IO? *)
      let cleanup () =
        (match Caqti_template.Request.prepare_policy req with
         | Direct ->
            (match Sqlite3.finalize stmt with
             | Sqlite3.Rc.OK -> Fiber.return ()
             | rc ->
                Log.warn (fun p ->
                  p "Ignoring error %s when finalizing statement."
                    (Sqlite3.Rc.to_string rc)))
         | Dynamic | Static ->
            (match Sqlite3.reset stmt with
             | Sqlite3.Rc.OK -> Fiber.return ()
             | _ ->
                Log.warn (fun p ->
                  p "Dropping cache statement due to error.") >|= fun () ->
                Pcache.remove_and_discard pcache req))
      in
      Fiber.finally (fun () -> f resp) cleanup

    let disconnect () = using_db @@ fun () ->
      let finalize_error_count = ref 0 in
      let not_busy = ref false in
      Preemptive.detach begin fun () ->
        let uncache (stmt, _, _) =
          (match Sqlite3.finalize stmt with
           | Sqlite3.Rc.OK -> ()
           | _ -> finalize_error_count := !finalize_error_count + 1) in
        Pcache.iter uncache pcache;
        not_busy := Sqlite3.db_close db
        (* If this reports busy, it means we missed an Sqlite3.finalize or other
         * cleanup action, so this should not happen. *)
      end () >>= fun () ->
      (if !finalize_error_count = 0 then Fiber.return () else
        Log.warn (fun p ->
          p "Finalization of %d during disconnect return error."
            !finalize_error_count)) >>= fun () ->
      (if !not_busy then Fiber.return () else
        Log.warn (fun p -> p "Sqlite reported still busy when closing handle."))

    let validate () = Fiber.return true
    let check f = f true

    let exec q p = call ~f:Response.exec q p
    let start () = exec Q.start ()
    let commit () = exec Q.commit ()
    let rollback () = exec Q.rollback ()
    let set_statement_timeout _ = Fiber.return (Ok ())
  end

  let setup ~config db =
    let tweaks_version = Caqti_connect_config.(get tweaks_version) config in
    if tweaks_version < (1, 8) then Fiber.return () else
    Preemptive.detach (Sqlite3.exec db) "PRAGMA foreign_keys = ON"
      >>= fun rc ->
    if rc = Sqlite3.Rc.OK then Fiber.return () else
    Log.warn (fun f ->
      f "Could not turn on foreign key support: %s" (Sqlite3.Rc.to_string rc))

  let connect ~sw:_ ~stdenv:_ ~subst ~config uri =
    try
      (* Check URI and extract parameters. *)
      assert (Uri.scheme uri = Some "sqlite3");
      (match Uri.userinfo uri, Uri.host uri with
       | None, (None | Some "") -> ()
       | _ -> invalid_arg "Sqlite URI cannot contain user or host components.");
      let mode =
        (match get_uri_bool uri "write", get_uri_bool uri "create" with
         | Some false, Some true -> invalid_arg "Create mode presumes write."
         | (Some false), (Some false | None)      -> Some `READONLY
         | (Some true | None), (Some true | None) -> None
         | (Some true | None), (Some false)       -> Some `NO_CREATE) in
      let busy_timeout = get_uri_int uri "busy_timeout" in

      (* Connect, configure, wrap. *)
      Preemptive.detach
        (fun () ->
          Sqlite3.db_open ~mutex:`FULL ?mode (Uri.path uri |> Uri.pct_decode))
        () >>= fun db ->
      setup ~config db >|= fun () ->
      (match busy_timeout with
       | None -> ()
       | Some timeout -> Sqlite3.busy_timeout db timeout);
      let module Arg = struct
        let subst = subst dialect
        let uri = uri
        let db = db
        let dynamic_capacity =
          Caqti_connect_config.(get dynamic_prepare_capacity) config
      end in
      let module Connection_base = Make_connection_base (Arg) in
      let module Connection = struct
        let driver_info = driver_info
        let dialect = dialect
        let driver_connection = Some (Driver_connection db)
        include Connection_base
        include Connection_utils.Make_convenience (System) (Connection_base)
        include Connection_utils.Make_populate (System) (Connection_base)
      end in
      Ok (module Connection : CONNECTION)
    with
     | Invalid_argument msg ->
        Fiber.return
          (Error (Caqti_error.connect_rejected ~uri (Caqti_error.Msg msg)))
     | Sqlite3.Error msg ->
        Fiber.return
          (Error (Caqti_error.connect_failed ~uri (Caqti_error.Msg msg)))
end

let () =
  Caqti_platform_unix.Driver_loader.register "sqlite3" (module Connect_functor)
