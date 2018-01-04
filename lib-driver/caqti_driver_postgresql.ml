(* Copyright (C) 2017--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_prereq
open Printf
module Pg = Postgresql

let start_req = Caqti_request.exec Caqti_type.unit "BEGIN"
let commit_req = Caqti_request.exec Caqti_type.unit "COMMIT"
let rollback_req = Caqti_request.exec Caqti_type.unit "ROLLBACK"

type Caqti_error.msg += Pg_msg of Pg.error
let () =
  let pp ppf = function
   | Pg_msg error -> Format.pp_print_string ppf (Pg.string_of_error error)
   | _ -> assert false in
  Caqti_error.define_msg ~pp [%extension_constructor Pg_msg]

let driver_info =
  Caqti_driver_info.create
    ~uri_scheme:"postgresql"
    ~dialect_tag:`Pgsql
    ~parameter_style:(`Indexed (fun i -> "$" ^ string_of_int (succ i)))
    ~can_pool:true
    ~can_concur:true
    ~can_transact:true
    ~describe_has_typed_params:true
    ~describe_has_typed_fields:true
    ()

module Pg_ext = struct

  let bool_of_string = function
   | "t" -> true
   | "f" -> false
   | _ -> invalid_arg "Pg_ext.bool_of_string"

  let string_of_bool = function true -> "t" | false -> "f"

  let query_string templ =
    let buf = Buffer.create 64 in
    let rec loop = function
     | Caqti_request.L s -> Buffer.add_string buf s
     | Caqti_request.P i -> bprintf buf "$%d" (i + 1)
     | Caqti_request.S frags -> List.iter loop frags in
    loop templ;
    Buffer.contents buf

  let escaped_connvalue s =
    let buf = Buffer.create (String.length s) in
    let aux = function
     | '\\' -> Buffer.add_string buf {|\\|}
     | '\'' -> Buffer.add_string buf {|\'|}
     | ch -> Buffer.add_char buf ch in
    String.iter aux s;
    Buffer.contents buf

  let conninfo_of_uri uri =
    if Uri.host uri <> None then Uri.to_string uri else
    let mkparam k v = k ^ " = '" ^ escaped_connvalue v ^ "'" in
    let mkparams (k, vs) = List.map (mkparam k) vs in
    String.concat " " (List.flatten (List.map mkparams (Uri.query uri)))
end

let is_binary_param : type a. a Caqti_type.field -> bool = function
 | Caqti_type.Octets -> true
 | _ -> false

let rec set_binary_params
    : type a. bool array -> a Caqti_type.t -> int -> int = fun bp -> function
 | Caqti_type.Unit -> fun i -> i
 | Caqti_type.Field ft -> fun i -> bp.(i) <- is_binary_param ft; i + 1
 | Caqti_type.Option t -> set_binary_params bp t
 | Caqti_type.Tup2 (t0, t1) ->
    set_binary_params bp t0 %> set_binary_params bp t1
 | Caqti_type.Tup3 (t0, t1, t2) ->
    set_binary_params bp t0 %> set_binary_params bp t1 %>
    set_binary_params bp t2
 | Caqti_type.Tup4 (t0, t1, t2, t3) ->
    set_binary_params bp t0 %> set_binary_params bp t1 %>
    set_binary_params bp t2 %> set_binary_params bp t3
 | Caqti_type.Custom {rep; _} -> set_binary_params bp rep

let rec encode_field
    : type a. uri: Uri.t -> a Caqti_type.field -> a -> (string, _) result =
  fun ~uri field_type x ->
  (match field_type with
   | Caqti_type.Bool -> Ok (Pg_ext.string_of_bool x)
   | Caqti_type.Int -> Ok (string_of_int x)
   | Caqti_type.Int32 -> Ok (Int32.to_string x)
   | Caqti_type.Int64 -> Ok (Int64.to_string x)
   | Caqti_type.Float -> Ok (sprintf "%.17g" x)
   | Caqti_type.String -> Ok x
   | Caqti_type.Octets -> Ok x
   | Caqti_type.Pdate -> Ok (iso8601_of_pdate x)
   | Caqti_type.Ptime ->
      Ok (Ptime.to_rfc3339 ~space:true x)
   | _ ->
      (match Caqti_type.Field.coding driver_info field_type with
       | None -> Error (Caqti_error.encode_missing ~uri ~field_type ())
       | Some (Caqti_type.Field.Coding {rep; encode; _}) ->
          (match encode x with
           | Ok y -> encode_field ~uri rep y
           | Error msg ->
              let msg = Caqti_error.Msg msg in
              let typ = Caqti_type.field field_type in
              Error (Caqti_error.encode_rejected ~uri ~typ msg))))

let rec decode_field
    : type a. uri: Uri.t -> a Caqti_type.field -> string -> (a, _) result =
  fun ~uri field_type s ->
  let conv f s =
    try Ok (f s) with _ ->
    let msg = Caqti_error.Msg (sprintf "Invalid value %S." s) in
    let typ = Caqti_type.field field_type in
    Error (Caqti_error.decode_rejected ~uri ~typ msg) in
  (match field_type with
   | Caqti_type.Bool -> conv Pg_ext.bool_of_string s
   | Caqti_type.Int -> conv int_of_string s
   | Caqti_type.Int32 -> conv Int32.of_string s
   | Caqti_type.Int64 -> conv Int64.of_string s
   | Caqti_type.Float -> conv float_of_string s
   | Caqti_type.String -> Ok s
   | Caqti_type.Octets -> Ok s
   | Caqti_type.Pdate ->
     (match pdate_of_iso8601 s with
       | Ok _ as r -> r
       | Error msg ->
          let msg = Caqti_error.Msg msg in
          let typ = Caqti_type.field field_type in
          Error (Caqti_error.decode_rejected ~uri ~typ msg))
   | Caqti_type.Ptime ->
      (* TODO: Improve parsing. *)
      (match ptime_of_rfc3339_utc s with
       | Ok _ as r -> r
       | Error msg ->
          let msg = Caqti_error.Msg msg in
          let typ = Caqti_type.field field_type in
          Error (Caqti_error.decode_rejected ~uri ~typ msg))
   | _ ->
      (match Caqti_type.Field.coding driver_info field_type with
       | None -> Error (Caqti_error.decode_missing ~uri ~field_type ())
       | Some (Caqti_type.Field.Coding {rep; decode; _}) ->
          (match decode_field ~uri rep s with
           | Ok y ->
              (match decode y with
               | Ok _ as r -> r
               | Error msg ->
                  let msg = Caqti_error.Msg msg in
                  let typ = Caqti_type.field field_type in
                  Error (Caqti_error.decode_rejected ~uri ~typ msg))
           | Error _ as r -> r)))

let rec encode_param
    : type a. uri: Uri.t -> string array ->
      a Caqti_type.t -> a -> int -> (int, _) result =
  fun ~uri params t x ->
  (match t, x with
   | Caqti_type.Unit, () -> fun i -> Ok i
   | Caqti_type.Field ft, fv -> fun i ->
      (match encode_field ~uri ft fv with
       | Ok s -> params.(i) <- s; Ok (i + 1)
       | Error _ as r -> r)
   | Caqti_type.Option t, None -> fun i -> Ok (i + Caqti_type.length t)
   | Caqti_type.Option t, Some x -> encode_param ~uri params t x
   | Caqti_type.Tup2 (t0, t1), (x0, x1) ->
      encode_param ~uri params t0 x0 %>?  encode_param ~uri params t1 x1
   | Caqti_type.Tup3 (t0, t1, t2), (x0, x1, x2) ->
      encode_param ~uri params t0 x0 %>?  encode_param ~uri params t1 x1 %>?
      encode_param ~uri params t2 x2
   | Caqti_type.Tup4 (t0, t1, t2, t3), (x0, x1, x2, x3) ->
      encode_param ~uri params t0 x0 %>?  encode_param ~uri params t1 x1 %>?
      encode_param ~uri params t2 x2 %>?  encode_param ~uri params t3 x3
   | Caqti_type.Custom {rep; encode; _}, x -> fun i ->
      (match encode x with
       | Ok y -> encode_param ~uri params rep y i
       | Error msg ->
          let msg = Caqti_error.Msg msg in
          Error (Caqti_error.encode_rejected ~uri ~typ:t msg)))

let rec decode_row'
    : type b. uri: Uri.t -> Pg.result * int ->
      b Caqti_type.t -> int -> (int * b, _) result =
  fun ~uri ((resp, i) as row) t ->
  (match t with
   | Caqti_type.Unit -> fun j -> Ok (j, ())
   | Caqti_type.Field ft -> fun j ->
      (match decode_field ~uri ft (resp#getvalue i j) with
       | Ok y -> Ok (j + 1, y)
       | Error _ as r -> r)
   | Caqti_type.Option t -> fun j ->
      let j' = j + Caqti_type.length t in
      let rec null_only k = k = j' ||
        (resp#getisnull i j && null_only (k + 1)) in
      if null_only j then Ok (j', None) else
      (match decode_row' ~uri row t j with
       | Ok (j'', y) -> assert (j' = j''); Ok (j'', Some y)
       | Error _ as r -> r)
   | Caqti_type.Tup2 (t0, t1) -> fun j ->
      decode_row' ~uri row t0 j |>? fun (j, y0) ->
      decode_row' ~uri row t1 j |>? fun (j, y1) ->
      Ok (j, (y0, y1))
   | Caqti_type.Tup3 (t0, t1, t2) -> fun j ->
      decode_row' ~uri row t0 j |>? fun (j, y0) ->
      decode_row' ~uri row t1 j |>? fun (j, y1) ->
      decode_row' ~uri row t2 j |>? fun (j, y2) ->
      Ok (j, (y0, y1, y2))
   | Caqti_type.Tup4 (t0, t1, t2, t3) -> fun j ->
      decode_row' ~uri row t0 j |>? fun (j, y0) ->
      decode_row' ~uri row t1 j |>? fun (j, y1) ->
      decode_row' ~uri row t2 j |>? fun (j, y2) ->
      decode_row' ~uri row t3 j |>? fun (j, y3) ->
      Ok (j, (y0, y1, y2, y3))
   | Caqti_type.Custom {rep; decode; _} as typ -> fun j ->
      (match decode_row' ~uri row rep j with
       | Ok (j, y) ->
          (match decode y with
           | Ok z -> Ok (j, z)
           | Error msg ->
              let msg = Caqti_error.Msg msg in
              Error (Caqti_error.decode_rejected ~uri ~typ msg))
       | Error _ as r -> r))

let decode_row ~uri resp row_type =
  (match decode_row' ~uri resp row_type 0 with
   | Ok (j, y) -> assert (j = Caqti_type.length row_type); Ok y
   | Error _ as r -> r)

module Connect_functor (System : Caqti_system_sig.S) = struct
  open System

  let (>>=?) m mf = m >>= (function Ok x -> mf x | Error _ as r -> return r)
  let (>|=?) m f = m >|= (function Ok x -> f x | Error _ as r -> r)

  let driver_info = driver_info

  module Pg_io = struct

    let communicate db step =
      let aux fd =
        let rec loop = function
         | Pg.Polling_reading ->
            Unix.poll ~read:true fd >>= fun _ -> loop (step ())
         | Pg.Polling_writing ->
            Unix.poll ~write:true fd >>= fun _ -> loop (step ())
         | Pg.Polling_failed | Pg.Polling_ok ->
            return ()
        in
        loop Pg.Polling_writing
      in
      Unix.wrap_fd aux (Obj.magic db#socket)

    let get_next_result ~uri ~query db =
      let aux fd =
        let rec hold () =
          db#consume_input;
          if db#is_busy then
            Unix.poll ~read:true fd >|= (fun _ -> ()) >>= hold
          else
            return (Ok db#get_result) in
        (try hold () with
         | Pg.Error msg ->
            let msg = Pg_msg msg in
            return (Error (Caqti_error.request_failed ~uri ~query msg))) in
      Unix.wrap_fd aux (Obj.magic db#socket)

    let get_result ~uri ~query db =
      get_next_result ~uri ~query db >>=
      (function
       | Ok None ->
          let msg = Caqti_error.Msg "No response received after send." in
          return (Error (Caqti_error.request_failed ~uri ~query msg))
       | Ok (Some result) ->
          get_next_result ~uri ~query db >>=
          (function
           | Ok None -> return (Ok result)
           | Ok (Some _) ->
              let msg = Caqti_error.Msg "More than one response received." in
              return (Error (Caqti_error.response_rejected ~uri ~query msg))
           | Error _ as r -> return r)
       | Error _ as r -> return r)

    let check_query_result ~uri ~query result mult =
      let reject msg =
        let msg = Caqti_error.Msg msg in
        Error (Caqti_error.response_rejected ~uri ~query msg) in
      let fail msg =
        let msg = Caqti_error.Msg msg in
        Error (Caqti_error.request_failed ~uri ~query msg) in
      (match result#status with
       | Pg.Command_ok ->
          (match Caqti_mult.expose mult with
           | `Zero -> Ok ()
           | (`One | `Zero_or_one | `Zero_or_more) ->
              reject "Tuples expected for this query.")
       | Pg.Tuples_ok ->
          (match Caqti_mult.expose mult with
           | `Zero ->
              if result#ntuples = 0 then Ok () else
              reject "No tuples expected for this query."
           | `One ->
              if result#ntuples = 1 then Ok () else
              ksprintf reject "Received %d tuples, expected one."
                       result#ntuples
           | `Zero_or_one ->
              if result#ntuples <= 1 then Ok () else
              ksprintf reject "Received %d tuples, expected at most one."
                       result#ntuples
           | `Zero_or_more -> Ok ())
       | Pg.Empty_query -> fail "The query was empty."
       | Pg.Bad_response -> reject result#error
       | Pg.Fatal_error -> fail result#error
       | Pg.Nonfatal_error -> Ok () (* TODO: Log *)
       | Pg.Copy_out | Pg.Copy_in | Pg.Copy_both ->
          reject "Received unexpected copy response."
       | Pg.Single_tuple ->
          reject "Received unexpected single-tuple response.")

    let check_command_result ~uri ~query result =
      check_query_result ~uri ~query result Caqti_mult.zero
  end

  (* Driver Interface *)

  module type CONNECTION =
    Caqti_connection_sig.Base with type 'a future := 'a System.future

  module Connection (Db : sig val uri : Uri.t val db : Pg.connection end)
    : CONNECTION =
  struct
    open Db

    type pcache_entry = {
      query: string;
      param_length: int;
      binary_params: bool array;
    }
    let pcache : (int, pcache_entry) Hashtbl.t = Hashtbl.create 19

    let reset () =
      try
        if db#reset_start then begin
          Hashtbl.clear pcache;
          Pg_io.communicate db (fun () -> db#reset_poll) >|= fun () ->
          db#status = Pg.Ok
        end else
          return false
      with Pg.Error _ -> return false

    let rec retry ~query ?(n = 1) f =
      try return (Ok (f ()))
      with Postgresql.Error (Connection_failure _ as err) ->
        if n > 0 then
          reset () >>= fun reset_ok ->
          if reset_ok then
            retry ~query ~n:(n - 1) f
          else
            return (Error (Caqti_error.request_failed ~uri ~query (Pg_msg err)))
        else
          return (Error (Caqti_error.request_failed ~uri ~query (Pg_msg err)))

    let prepare query_name query =
      retry ~query (fun () -> db#send_prepare query_name query) >>=
      (function
       | Error _ as r -> return r
       | Ok () ->
          Pg_io.get_result ~uri ~query db >|=
          (function
           | Ok result -> Pg_io.check_command_result ~uri ~query result
           | Error _ as r -> r))

    let query_oneshot ?params ?binary_params query =
      retry ~query (fun () -> db#send_query ?params ?binary_params query) >>=
      (function
       | Error _ as r -> return r
       | Ok () -> Pg_io.get_result ~uri ~query db)

    let query_prepared ~query ?params ?binary_params query_name =
      retry ~query
        (fun () -> db#send_query_prepared ?params ?binary_params query_name) >>=
      (function
       | Error _ as r -> return r
       | Ok () -> Pg_io.get_result ~uri ~query db)

    module Response = struct
      type ('b, 'm) t = {row_type: 'b Caqti_type.t; result: Pg.result}

      let returned_count {result; _} =
        return (Ok result#ntuples)

      let affected_count {result; _} =
        return (Ok (int_of_string result#cmd_tuples))

      let exec _ = return (Ok ())

      let find {row_type; result} =
        return (decode_row ~uri (result, 0) row_type)

      let find_opt {row_type; result} =
        return begin
          if result#ntuples = 0 then Ok None else
          (match decode_row ~uri (result, 0) row_type with
           | Ok y -> Ok (Some y)
           | Error _ as r -> r)
        end

      let fold f {row_type; result} acc =
        let n = result#ntuples in
        let rec loop i acc =
          if i = n then Ok acc else
          (match decode_row ~uri (result, i) row_type with
           | Ok y -> loop (i + 1) (f y acc)
           | Error _ as r -> r) in
        return (loop 0 acc)

      let fold_s f {row_type; result} acc =
        let n = result#ntuples in
        let rec loop i acc =
          if i = n then return (Ok acc) else
          (match decode_row ~uri (result, i) row_type with
           | Ok y -> f y acc >>=? loop (i + 1)
           | Error _ as r -> return r) in
        loop 0 acc

      let iter_s f {row_type; result} =
        let n = result#ntuples in
        let rec loop i () =
          if i = n then return (Ok ()) else
          (match decode_row ~uri (result, i) row_type with
           | Ok y -> f y >>=? loop (i + 1)
           | Error _ as r -> return r) in
        loop 0 ()
    end

    let call ~f req param =

      (* Send the query. *)
      let param_type = Caqti_request.param_type req in
      (match Caqti_request.query_id req with
       | None ->
          let templ = Caqti_request.query_template req driver_info in
          let query = Pg_ext.query_string templ in
          let param_length = Caqti_type.length param_type in
          let binary_params = Array.make param_length false in
          let nbp = set_binary_params binary_params param_type 0 in
          assert (nbp = param_length);
          let params = Array.make param_length Pg.null in
          (match encode_param ~uri params param_type param 0 with
           | Ok n ->
              assert (n = param_length);
              query_oneshot ~params ~binary_params query >|=? fun result ->
              Ok (query, result)
           | Error _ as r -> return r)
       | Some query_id ->
          let query_name = sprintf "_caq%d" query_id in
          (try return (Ok (Hashtbl.find pcache query_id)) with
           | Not_found ->
              let templ = Caqti_request.query_template req driver_info in
              let query = Pg_ext.query_string templ in
              prepare query_name query >|=? fun () ->
              let param_length = Caqti_type.length param_type in
              let binary_params = Array.make param_length false in
              let nbp = set_binary_params binary_params param_type 0 in
              assert (nbp = param_length);
              let pcache_entry = {query; param_length; binary_params} in
              Hashtbl.add pcache query_id pcache_entry;
              Ok pcache_entry)
            >>=? fun {query; param_length; binary_params} ->
          let params = Array.make param_length Pg.null in
          (match encode_param ~uri params param_type param 0 with
           | Ok n ->
              assert (n = param_length);
              query_prepared ~query ~params ~binary_params query_name
                >|=? fun result ->
              Ok (query, result)
           | Error _ as r -> return r))
        >>=? fun (query, result) ->

      (* Fetch and process the result. *)
      let row_type = Caqti_request.row_type req in
      let row_mult = Caqti_request.row_mult req in
      (match Pg_io.check_query_result ~uri ~query result row_mult with
       | Ok () -> f Response.{row_type; result}
       | Error _ as r -> return r)

    let disconnect () = db#finish; return ()
    let validate () = if db#status = Pg.Ok then return true else reset ()
    let check f = f (try db#status = Pg.Ok with Pg.Error _ -> false)

    let exec q p = call ~f:Response.exec q p
    let start () = exec start_req ()
    let commit () = exec commit_req ()
    let rollback () = exec rollback_req ()
  end

  let connect uri =
    let conninfo = Pg_ext.conninfo_of_uri uri in
    try
      let db = new Pg.connection ~conninfo () in
      Pg_io.communicate db (fun () -> db#connect_poll) >|= fun () ->
      if db#status <> Pg.Ok then
        let msg = db#error_message in
        Error (Caqti_error.connect_failed ~uri (Caqti_error.Msg msg))
      else
        let module Connection =
          Connection (struct let uri = uri let db = db end) in
        Ok (module Connection : CONNECTION)
    with Pg.Error error ->
      return (Error (Caqti_error.connect_failed ~uri (Pg_msg error)))
end

let () = Caqti_connect.define_driver "postgresql" (module Connect_functor)
