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
open Caqti_driver_lib
open Printf

let driver_info =
  Caqti_driver_info.create
    ~uri_scheme:"sqlite3"
    ~dialect_tag:`Sqlite
    ~parameter_style:(`Linear "?")
    ~can_pool:false
    ~can_concur:false
    ~can_transact:true
    ~describe_has_typed_params:false
    ~describe_has_typed_fields:true
    ()

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

type Caqti_error.msg += Rc : Sqlite3.Rc.t -> Caqti_error.msg
let () =
  let pp ppf = function
   | Rc rc -> Format.pp_print_string ppf (Sqlite3.Rc.to_string rc)
   | _ -> assert false in
  Caqti_error.define_msg ~pp [%extension_constructor Rc]

let rec data_of_value
    : type a. uri: Uri.t -> a Caqti_type.field -> a ->
      (Sqlite3.Data.t, _) result =
  fun ~uri field_type x ->
  (match field_type with
   | Caqti_type.Bool   -> Ok (Sqlite3.Data.INT (if x then 1L else 0L))
   | Caqti_type.Int    -> Ok (Sqlite3.Data.INT (Int64.of_int x))
   | Caqti_type.Int32  -> Ok (Sqlite3.Data.INT (Int64.of_int32 x))
   | Caqti_type.Int64  -> Ok (Sqlite3.Data.INT x)
   | Caqti_type.Float  -> Ok (Sqlite3.Data.FLOAT x)
   | Caqti_type.String -> Ok (Sqlite3.Data.TEXT x)
   | Caqti_type.Octets -> Ok (Sqlite3.Data.BLOB x)
   | Caqti_type.Pdate -> Ok (Sqlite3.Data.TEXT (iso8601_of_pdate x))
   | Caqti_type.Ptime ->
      (* This is the suggested time representation according to
         https://sqlite.org/lang_datefunc.html, and is consistent with
         current_timestamp.  Three subsecond digits are significant. *)
      let s = Ptime.to_rfc3339 ~space:true ~frac_s:3 ~tz_offset_s:0 x in
      Ok (Sqlite3.Data.TEXT (String.sub s 0 23))
   | Caqti_type.Ptime_span ->
      Ok (Sqlite3.Data.FLOAT (Ptime.Span.to_float_s x))
   | _ ->
      (match Caqti_type.Field.coding driver_info field_type with
       | None ->
          Error (Caqti_error.encode_missing ~uri ~field_type ())
       | Some (Caqti_type.Field.Coding {rep; encode; _}) ->
          (match encode x with
           | Ok y -> data_of_value ~uri rep y
           | Error msg ->
              let msg = Caqti_error.Msg msg in
              let typ = Caqti_type.field field_type in
              Error (Caqti_error.encode_rejected ~uri ~typ msg))))

(* TODO: Check integer ranges? The Int64.to_* functions don't raise. *)
let rec value_of_data
    : type a. uri: Uri.t ->
      a Caqti_type.field -> Sqlite3.Data.t -> (a, _) result =
  fun ~uri field_type data ->
  let to_ptime_span x =
    (match Ptime.Span.of_float_s x with
     | Some t -> Ok t
     | None ->
        let msg = Caqti_error.Msg "Interval out of range for Ptime.span." in
        let typ = Caqti_type.field field_type in
        Error (Caqti_error.decode_rejected ~uri ~typ msg)) in
  (match field_type, data with
   | Caqti_type.Bool, Sqlite3.Data.INT y -> Ok (y <> 0L)
   | Caqti_type.Int, Sqlite3.Data.INT y -> Ok (Int64.to_int y)
   | Caqti_type.Int32, Sqlite3.Data.INT y -> Ok (Int64.to_int32 y)
   | Caqti_type.Int64, Sqlite3.Data.INT y -> Ok y
   | Caqti_type.Float, Sqlite3.Data.FLOAT y -> Ok y
   | Caqti_type.Float, Sqlite3.Data.INT y -> Ok (Int64.to_float y)
   | Caqti_type.String, Sqlite3.Data.TEXT y -> Ok y
   | Caqti_type.Octets, Sqlite3.Data.BLOB y -> Ok y
   | Caqti_type.Pdate as field_type, Sqlite3.Data.TEXT y ->
      (match pdate_of_iso8601 y with
       | Ok _ as r -> r
       | Error msg ->
          let msg = Caqti_error.Msg msg in
          let typ = Caqti_type.field field_type in
          Error (Caqti_error.decode_rejected ~uri ~typ msg))
   | Caqti_type.Ptime as field_type, Sqlite3.Data.TEXT y ->
      (* TODO: Improve parsing. *)
      (match ptime_of_rfc3339_utc y with
       | Ok _ as r -> r
       | Error msg ->
          let msg = Caqti_error.Msg msg in
          let typ = Caqti_type.field field_type in
          Error (Caqti_error.decode_rejected ~uri ~typ msg))
   | Caqti_type.Ptime_span, Sqlite3.Data.FLOAT x ->
      to_ptime_span x
   | Caqti_type.Ptime_span, Sqlite3.Data.INT x ->
      to_ptime_span (Int64.to_float x)
   | field_type, d ->
      (match Caqti_type.Field.coding driver_info field_type with
       | None -> Error (Caqti_error.decode_missing ~uri ~field_type ())
       | Some (Caqti_type.Field.Coding {rep; decode; _}) ->
          (match value_of_data ~uri rep d with
           | Ok y ->
              (match decode y with
               | Ok _ as r -> r
               | Error msg ->
                  let msg = Caqti_error.Msg msg in
                  let typ = Caqti_type.field field_type in
                  Error (Caqti_error.decode_rejected ~uri ~typ msg))
           | Error _ as r -> r)))

let encode_null_field ~uri stmt field_type o =
  let aux i =
    (match Sqlite3.bind stmt (i + 1) Sqlite3.Data.NULL with
     | Sqlite3.Rc.OK -> Ok ()
     | rc ->
        let typ = Caqti_type.field field_type in
        Error (Caqti_error.encode_failed ~uri ~typ (Rc rc))) in
  List.iter_r aux o

let encode_field ~uri stmt field_type field_value o =
  (match data_of_value ~uri field_type field_value with
   | Ok d ->
      let aux i =
        (match Sqlite3.bind stmt (i + 1) d with
         | Sqlite3.Rc.OK -> Ok ()
         | rc ->
            let typ = Caqti_type.field field_type in
            Error (Caqti_error.encode_failed ~uri ~typ (Rc rc))) in
      List.iter_r aux o
   | Error _ as r -> r)

let rec encode_null_param
    : type a. uri: Uri.t -> Sqlite3.stmt -> a Caqti_type.t ->
      int list list -> (int list list, _) result =
  fun ~uri stmt ->
  (function
   | Caqti_type.Unit -> fun os -> Ok os
   | Caqti_type.Field ft -> fun os ->
      assert (os <> []);
      (match encode_null_field ~uri stmt ft (List.hd os) with
       | Ok () -> Ok (List.tl os)
       | Error _ as r -> r)
   | Caqti_type.Option t ->
      encode_null_param ~uri stmt t
   | Caqti_type.Tup2 (t0, t1) ->
      encode_null_param ~uri stmt t0 %>? encode_null_param ~uri stmt t1
   | Caqti_type.Tup3 (t0, t1, t2) ->
      encode_null_param ~uri stmt t0 %>? encode_null_param ~uri stmt t1 %>?
      encode_null_param ~uri stmt t2
   | Caqti_type.Tup4 (t0, t1, t2, t3) ->
      encode_null_param ~uri stmt t0 %>? encode_null_param ~uri stmt t1 %>?
      encode_null_param ~uri stmt t2 %>? encode_null_param ~uri stmt t3
   | Caqti_type.Custom {rep; _} ->
      encode_null_param ~uri stmt rep)

let rec encode_param
    : type a. uri: Uri.t -> Sqlite3.stmt -> a Caqti_type.t -> a ->
      int list list -> (int list list, _) result =
  fun ~uri stmt t x ->
  (match t, x with
   | Caqti_type.Unit, () -> fun os -> Ok os
   | Caqti_type.Field ft, fv -> fun os ->
      assert (os <> []);
      (match encode_field ~uri stmt ft fv (List.hd os) with
       | Ok () -> Ok (List.tl os)
       | Error _ as r -> r)
   | Caqti_type.Option t, None ->
      encode_null_param ~uri stmt t
   | Caqti_type.Option t, Some x ->
      encode_param ~uri stmt t x
   | Caqti_type.Tup2 (t0, t1), (x0, x1) ->
      encode_param ~uri stmt t0 x0 %>? encode_param ~uri stmt t1 x1
   | Caqti_type.Tup3 (t0, t1, t2), (x0, x1, x2) ->
      encode_param ~uri stmt t0 x0 %>? encode_param ~uri stmt t1 x1 %>?
      encode_param ~uri stmt t2 x2
   | Caqti_type.Tup4 (t0, t1, t2, t3), (x0, x1, x2, x3) ->
      encode_param ~uri stmt t0 x0 %>? encode_param ~uri stmt t1 x1 %>?
      encode_param ~uri stmt t2 x2 %>? encode_param ~uri stmt t3 x3
   | Caqti_type.Custom {rep; encode; _}, x -> fun i ->
      (match encode x with
       | Ok y -> encode_param ~uri stmt rep y i
       | Error msg ->
          let msg = Caqti_error.Msg msg in
          Error (Caqti_error.encode_rejected ~uri ~typ:t msg)))

let rec decode_row
    : type b. uri: Uri.t -> Sqlite3.stmt -> int -> b Caqti_type.t ->
      (int * b, _) result =
  fun ~uri stmt i ->
  (function
   | Caqti_type.Unit -> Ok (i, ())
   | Caqti_type.Field ft ->
      (match value_of_data ~uri ft (Sqlite3.column stmt i) with
       | Ok fv -> Ok (i + 1, fv)
       | Error _ as r -> r)
   | Caqti_type.Option t ->
      let j = i + Caqti_type.length t in
      let rec null_only k = k = j ||
        (Sqlite3.column stmt k = Sqlite3.Data.NULL && null_only (i + 1)) in
      if null_only i then Ok (j, None) else
      (match decode_row ~uri stmt i t with
       | Ok (j, y) -> Ok (j, Some y)
       | Error _ as r -> r)
   | Caqti_type.Tup2 (t0, t1) ->
      decode_row ~uri stmt i t0 |>? fun (i, y0) ->
      decode_row ~uri stmt i t1 |>? fun (i, y1) ->
      Ok (i, (y0, y1))
   | Caqti_type.Tup3 (t0, t1, t2) ->
      decode_row ~uri stmt i t0 |>? fun (i, y0) ->
      decode_row ~uri stmt i t1 |>? fun (i, y1) ->
      decode_row ~uri stmt i t2 |>? fun (i, y2) ->
      Ok (i, (y0, y1, y2))
   | Caqti_type.Tup4 (t0, t1, t2, t3) ->
      decode_row ~uri stmt i t0 |>? fun (i, y0) ->
      decode_row ~uri stmt i t1 |>? fun (i, y1) ->
      decode_row ~uri stmt i t2 |>? fun (i, y2) ->
      decode_row ~uri stmt i t3 |>? fun (i, y3) ->
      Ok (i, (y0, y1, y2, y3))
   | Caqti_type.Custom {rep; decode; _} as typ ->
      (match decode_row ~uri stmt i rep with
       | Ok (j, y) ->
          (match decode y with
           | Ok z -> Ok (j, z)
           | Error msg ->
              let msg = Caqti_error.Msg msg in
              Error (Caqti_error.decode_rejected ~uri ~typ msg))
       | Error _ as r -> r))

module Q = struct
  let start = Caqti_request.exec Caqti_type.unit "BEGIN"
  let commit = Caqti_request.exec Caqti_type.unit "COMMIT"
  let rollback = Caqti_request.exec Caqti_type.unit "ROLLBACK"
end

module Connect_functor (System : Caqti_driver_sig.System_unix) = struct
  open System

  let (>>=?) m mf = m >>= (function Ok x -> mf x | Error _ as r -> return r)
  let (>|=?) m f = m >|= (function Ok x -> f x | Error _ as r -> r)

  let driver_info = driver_info

  module type CONNECTION = Caqti_connection_sig.Base
    with type 'a future := 'a System.future
     and type ('a, 'err) Response.stream := ('a, 'err) System.Stream.t

  module Connection (Db : sig val uri : Uri.t val db : Sqlite3.db end)
    : CONNECTION =
  struct
    open Db

    module Response = struct

      type ('b, 'm) t = {
        stmt: Sqlite3.stmt;
        row_type: 'b Caqti_type.t;
        query: string;
      }

      let returned_count _ = return (Error `Unsupported)

      let affected_count {stmt; _} = return (Error `Unsupported)
      (* Might be implemented with Sqlite3.changes, but it needs to be queried
       * at the correct time. *)

      let fetch_row {stmt; row_type; query} =
        (match Sqlite3.step stmt with
         | Sqlite3.Rc.DONE -> Ok None
         | Sqlite3.Rc.ROW ->
            (match decode_row ~uri stmt 0 row_type with
             | Ok (n, y) ->
                let n' = Sqlite3.data_count stmt in
                if n = n' then
                  Ok (Some y)
                else
                  let msg = sprintf "Decoded only %d of %d fields." n n' in
                  let msg = Caqti_error.Msg msg in
                  Error (Caqti_error.response_rejected ~uri ~query msg)
             | Error _ as r -> r)
         | rc ->
            Error (Caqti_error.response_failed ~uri ~query (Rc rc)))

      let exec {stmt; row_type; query} =
        assert (row_type = Caqti_type.unit);
        let retrieve () =
          (match Sqlite3.step stmt with
           | Sqlite3.Rc.DONE -> Ok ()
           | Sqlite3.Rc.ROW ->
              let msg = Caqti_error.Msg "Received unexpected row for exec." in
              Error (Caqti_error.response_rejected ~uri ~query msg)
           | rc ->
              Error (Caqti_error.response_failed ~uri ~query (Rc rc))) in
        Preemptive.detach retrieve ()

      let find resp =
        let retrieve () =
          (match fetch_row resp with
           | Ok None ->
              let msg = Caqti_error.Msg "Received no rows for find." in
              Error (Caqti_error.response_rejected ~uri ~query:resp.query msg)
           | Ok (Some y) ->
              (match fetch_row resp with
               | Ok None -> Ok y
               | Ok (Some _) ->
                  let msg = "Received multiple rows for find." in
                  let msg = Caqti_error.Msg msg in
                  let query = resp.query in
                  Error (Caqti_error.response_rejected ~uri ~query msg)
               | Error _ as r -> r)
           | Error _ as r -> r) in
        Preemptive.detach retrieve ()

      let find_opt resp = Preemptive.detach fetch_row resp

      let fold f resp acc =
        let rec retrieve acc =
          (match fetch_row resp with
           | Ok None -> Ok acc
           | Ok (Some y) -> retrieve (f y acc)
           | Error _ as r -> r) in
        Preemptive.detach retrieve acc

      let fold_s f resp acc =
        let rec retrieve acc =
          (match fetch_row resp with
           | Ok None -> Ok acc
           | Ok (Some y) ->
              (match Preemptive.run_in_main (fun () -> f y acc) with
               | Ok acc -> retrieve acc
               | Error _ as r -> r)
           | Error _ as r -> r) in
        Preemptive.detach retrieve acc

      let iter_s f resp =
        let rec retrieve () =
          (match fetch_row resp with
           | Ok None -> Ok ()
           | Ok (Some y) ->
              (match Preemptive.run_in_main (fun () -> f y) with
               | Ok () -> retrieve ()
               | Error _ as r -> r)
           | Error _ as r -> r) in
        Preemptive.detach retrieve ()

      let rec to_stream resp () =
        let open Stream in
        match fetch_row resp with
        | Ok None -> return Nil
        | Error err -> return (Err err)
        | Ok (Some y) -> return (Cons (y, to_stream resp))
    end

    let pcache = Hashtbl.create 19

    let call ~f req param =
      let param_type = Caqti_request.param_type req in
      let row_type = Caqti_request.row_type req in

      let prepare_helper query =
        try
          let stmt = Sqlite3.prepare db query in
          (match Sqlite3.prepare_tail stmt with
           | None -> Ok stmt
           | Some stmt -> Ok stmt)
        with Sqlite3.Error msg ->
          let msg = Caqti_error.Msg msg in
          Error (Caqti_error.request_failed ~uri ~query msg) in

      let prepare () =
        let templ = Caqti_request.query req driver_info in
        let query = linear_query_string templ in
        let os = linear_param_order templ in
        Preemptive.detach prepare_helper query >|=? fun stmt ->
        Ok (stmt, os, query) in

      (match Caqti_request.query_id req with
       | None -> prepare ()
       | Some id ->
          (try return (Ok (Hashtbl.find pcache id)) with
           | Not_found ->
              prepare () >|=? fun pcache_entry ->
              Hashtbl.add pcache id pcache_entry;
              Ok pcache_entry))
      >>=? fun (stmt, os, query) ->

      (* CHECKME: Does binding involve IO? *)
      (match encode_param ~uri stmt param_type param os with
       | Ok os ->
          assert (os = []);
          return (Ok Response.{stmt; query; row_type})
       | Error _ as r -> return r)
      >>=? fun resp ->

      (* CHECKME: Does finalize or reset involve IO? *)
      let cleanup () =
        (match Caqti_request.query_id req with
         | None ->
            (match Sqlite3.finalize stmt with
             | Sqlite3.Rc.OK -> return ()
             | _ ->
                Log.warn (fun p ->
                  p "Ignoring error when finalizing statement."))
         | Some id ->
            (match Sqlite3.reset stmt with
             | Sqlite3.Rc.OK -> return ()
             | _ ->
                Log.warn (fun p ->
                  p "Dropping cache statement due to error.") >|= fun () ->
                Hashtbl.remove pcache id)) in

      (try f resp >>= fun r -> cleanup () >|= fun () -> r
       with exn -> cleanup () >|= fun () -> raise exn (* should not happen *))

    let disconnect () =
      let finalize_error_count = ref 0 in
      let not_busy = ref false in
      Preemptive.detach begin fun () ->
        let uncache _ (stmt, _, _) =
          (match Sqlite3.finalize stmt with
           | Sqlite3.Rc.OK -> ()
           | _ -> finalize_error_count := !finalize_error_count + 1) in
        Hashtbl.iter uncache pcache;
        not_busy := Sqlite3.db_close db
        (* If this reports busy, it means we missed an Sqlite3.finalize or other
         * cleanup action, so this should not happen. *)
      end () >>= fun () ->
      (if !finalize_error_count = 0 then return () else
        Log.warn (fun p ->
          p "Finalization of %d during disconnect return error."
            !finalize_error_count)) >>= fun () ->
      (if !not_busy then return () else
        Log.warn (fun p -> p "Sqlite reported still busy when closing handle."))

    let validate () = return true
    let check f = f true

    let exec q p = call ~f:Response.exec q p
    let start () = exec Q.start ()
    let commit () = exec Q.commit ()
    let rollback () = exec Q.rollback ()
  end

  let connect uri =
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
        () >|= fun db ->
      (match busy_timeout with
       | None -> ()
       | Some timeout -> Sqlite3.busy_timeout db timeout);
      let module Arg = struct let uri = uri let db = db end in
      let module Db = Connection (Arg) in
      Ok (module Db : CONNECTION)
    with
     | Invalid_argument msg ->
        return (Error (Caqti_error.connect_rejected ~uri (Caqti_error.Msg msg)))
     | Sqlite3.Error msg ->
        return (Error (Caqti_error.connect_failed ~uri (Caqti_error.Msg msg)))
end

let () = Caqti_connect.define_unix_driver "sqlite3" (module Connect_functor)
