(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_driver_lib
open Caqti_common_priv
open Printf

let cause_of_errno = function
 | 1022 -> `Unique_violation
 | 1048 -> `Not_null_violation
 | 1052 -> `Integrity_constraint_violation__don't_match
 | 1062 -> `Unique_violation
 | 1169 -> `Unique_violation
 | 1216 -> `Foreign_key_violation
 | 1217 -> `Foreign_key_violation
 | 1451 -> `Foreign_key_violation
 | 1452 -> `Foreign_key_violation
 | 1557 -> `Integrity_constraint_violation__don't_match
 | 1586 -> `Unique_violation
 | 1761 -> `Integrity_constraint_violation__don't_match
 | 1762 -> `Integrity_constraint_violation__don't_match
 | 1859 -> `Integrity_constraint_violation__don't_match
 | 4025 -> `Check_violation
 | _ -> `Unspecified__don't_match

type Caqti_error.msg += Error_msg of {errno: int; error: string}
let () =
  let pp ppf = function
   | Error_msg {errno; error} ->
      Format.fprintf ppf "Error %d, %s." errno error
   | _ ->
      assert false
  in
  let cause = function
   | Error_msg {errno; _} -> cause_of_errno errno
   | _ -> assert false
  in
  Caqti_error.define_msg ~pp ~cause [%extension_constructor Error_msg]

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std
  let set_utc =
    (unit ->. unit) ~oneshot:true "SET time_zone = '+00:00'"
  let set_statement_timeout =
    (float ->. unit) ~oneshot:true "SET max_statement_time = ?"
end

let no_env _ _ = raise Not_found

module Connect_functor (System : Caqti_driver_sig.System_unix) = struct
  open System
  module H = Caqti_connection.Make_helpers (System)

  let (>>=?) m mf = m >>= (function Ok x -> mf x | Error _ as r -> return r)

  module Mdb = Mariadb.Nonblocking.Make
    (struct
      open System

      module IO = struct
        type 'a future = 'a System.future
        let (>>=) = System.(>>=)
        let return = System.return
      end

      let wait db status =
        Mariadb.Nonblocking.fd db |> System.Unix.wrap_fd @@ fun fd ->
        System.Unix.poll
          ~read:(Mariadb.Nonblocking.Status.read status)
          ~write:(Mariadb.Nonblocking.Status.write status) fd
          >|= (fun (read, write, timeout) ->
        Mariadb.Nonblocking.Status.create ~read ~write ~timeout ())
    end)

  module Mdb_ext = struct

    module Field = struct

      let typename field =
        (match Mdb.Field.value field with
         | `Null -> "null"
         | `Int _ -> "int"
         | `Float _ -> "float"
         | `String _ -> "string"
         | `Bytes _ -> "bytes"
         | `Time _ -> "time")

      let bool field =
        (match Mdb.Field.value field with
         | `Int i -> i <> 0
         | _ -> failwith "Mdb_ext.Field.float")

      (* Lax conversion from float and string since:
       * - Arithmetic involving integers like `1 + ?` with at least one
       *   parameter comes back as float.
       * - An expression like `sum(1)` comes back as a string here, presumably
       *   as decimal from MariaDB. *)
      let int field =
        (match Mdb.Field.value field with
         | `Int i -> i
         | `Float x when fst (modf x) = 0.0 -> int_of_float x
         | `String s -> int_of_string s
         | _ -> failwith "Mdb_ext.Field.int")

      let int32 field =
        (match Mdb.Field.value field with
         | `Int i -> Int32.of_int i
         | `Float x -> Int32.of_float x   (* cf. int *)
         | `String s -> Int32.of_string s (* cf. int *)
         | _ -> failwith "Mdb_ext.Field.int32")

      let int64 field =
        (match Mdb.Field.value field with
         | `Int i -> Int64.of_int i
         | `Float x -> Int64.of_float x   (* cf. int *)
         | `String s -> Int64.of_string s (* cf. int *)
         | _ -> failwith "Mdb_ext.Field.int64")

      let float field =
        (match Mdb.Field.value field with
         | `Int i -> float_of_int i
         | `Float x -> x
         | _ -> failwith "Mdb_ext.Field.float")

      let string field =
        (match Mdb.Field.value field with
         | `String s -> s
         | `Bytes s -> Bytes.to_string s
         | _ -> failwith "Mdb_ext.Field.string")
    end

  end

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a future := 'a System.future
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t

  let driver_info =
    Caqti_driver_info.create
      ~uri_scheme:"mariadb"
      ~dialect_tag:`Mysql
      ~parameter_style:(`Linear "?")
      ~can_pool:true
      ~can_concur:true
      ~can_transact:true
      ~describe_has_typed_params:false (* TODO *)
      ~describe_has_typed_fields:false (* TODO *)
      ()

  let rec encode_field
      : type a. uri: Uri.t ->
        a Caqti_type.Field.t -> a -> (Mdb.Field.value, _) result =
    fun ~uri field_type x ->
    (match field_type with
     | Caqti_type.Bool -> Ok (`Int (if x then 1 else 0))
     | Caqti_type.Int -> Ok (`Int x)
     | Caqti_type.Int16 -> Ok (`Int x)
     | Caqti_type.Int32 -> Ok (`Int (Int32.to_int x))
     | Caqti_type.Int64 -> Ok (`Int (Int64.to_int x))
     | Caqti_type.Float -> Ok (`Float x)
     | Caqti_type.String -> Ok (`String x)
     | Caqti_type.Enum _ -> Ok (`String x)
     | Caqti_type.Octets -> Ok (`Bytes (Bytes.of_string x))
     | Caqti_type.Pdate ->
        let year, month, day = Ptime.to_date x in
        Ok (`Time (Mdb.Time.date ~year ~month ~day ()))
     | Caqti_type.Ptime ->
        let (year, month, day), ((hour, minute, second), _) =
          Ptime.to_date_time ~tz_offset_s:0 x in
        let ps = snd (Ptime.Span.to_d_ps (Ptime.to_span x)) in
        let tss = Int64.rem ps 1_000_000_000_000L in
        let microsecond = Int64.to_int (Int64.div tss 1_000_000L) in
        Ok (`Time (Mdb.Time.datetime ~year ~month ~day
                                     ~hour ~minute ~second ~microsecond ()))
     | Caqti_type.Ptime_span ->
        Ok (`Float (Ptime.Span.to_float_s x))
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
      : type b. uri: Uri.t -> b Caqti_type.Field.t ->
        Mdb.Field.t -> (b, _) result =
    fun ~uri field_type field ->
    try match field_type with
     | Caqti_type.Bool -> Ok (Mdb_ext.Field.bool field)
     | Caqti_type.Int -> Ok (Mdb_ext.Field.int field)
     | Caqti_type.Int16 -> Ok (Mdb_ext.Field.int field)
     | Caqti_type.Int32 -> Ok (Mdb_ext.Field.int32 field)
     | Caqti_type.Int64 -> Ok (Mdb_ext.Field.int64 field)
     | Caqti_type.Float -> Ok (Mdb.Field.float field)
     | Caqti_type.String -> Ok (Mdb_ext.Field.string field)
     | Caqti_type.Enum _ -> Ok (Mdb_ext.Field.string field)
     | Caqti_type.Octets -> Ok (Mdb_ext.Field.string field)
     | Caqti_type.Pdate ->
        let t = Mdb.Field.time field in
        let date = Mdb.Time.(year t, month t, day t) in
        (match Ptime.of_date date with
         | None -> failwith "Ptime.of_date"
         | Some t -> Ok t)
     | Caqti_type.Ptime ->
        let t = Mdb.Field.time field in
        let date = Mdb.Time.(year t, month t, day t) in
        let time = Mdb.Time.(hour t, minute t, second t) in
        let us = Mdb.Time.microsecond t in
        (match Ptime.of_date_time (date, (time, 0)),
               Ptime.Span.of_d_ps (0, Int64.(mul (of_int us) 1_000_000L)) with
         | None, _ | _, None -> failwith "Ptime.of_date_time"
         | Some t, Some tss ->
            (match Ptime.add_span t tss with
             | None -> failwith "Ptime.of_date_time: Overflow."
             | Some t' -> Ok t'))
     | Caqti_type.Ptime_span ->
        let t = Mdb_ext.Field.float field in
        (match Ptime.Span.of_float_s t with
         | None -> failwith "Ptime.Span.of_float_s"
         | Some t -> Ok t)
     | field_type ->
        (match Caqti_type.Field.coding driver_info field_type with
         | None -> Error (Caqti_error.decode_missing ~uri ~field_type ())
         | Some (Caqti_type.Field.Coding {rep; decode; _}) ->
            (match decode_field ~uri rep field with
             | Ok y ->
                (match decode y with
                 | Ok _ as r -> r
                 | Error msg ->
                    let msg = Caqti_error.Msg msg in
                    let typ = Caqti_type.field field_type in
                    Error (Caqti_error.decode_rejected ~uri ~typ msg))
             | Error _ as r -> r))
    with
     | Failure _ ->
        let typename = Mdb_ext.Field.typename field in
        let msg = Caqti_error.Msg ("Received " ^ typename ^ ".") in
        let typ = Caqti_type.field field_type in
        Error (Caqti_error.decode_rejected ~uri ~typ msg)

  let encode_param ~uri params t v acc =
    let write_value ~uri ft fv os =
      assert (os <> []);
      (match encode_field ~uri ft fv with
       | Ok v ->
          List.iter (fun j -> params.(j) <- v) (List.hd os);
          Ok (List.tl os)
       | Error _ as r -> r)
    in
    let write_null ~uri:_ _ os = Ok (List.tl os) in
    Caqti_driver_lib.encode_param ~uri {write_value; write_null} t v acc

  let decode_row ~uri row_type row =
    let read_value ~uri ft j =
      (match decode_field ~uri ft row.(j) with
       | Ok fv -> Ok (fv, j + 1)
       | Error _ as r -> r)
    in
    let skip_null n j =
      let j' = j + n in
      let rec check k =
        k = j' || Mdb.Field.null_value row.(k) && check (k + 1)
      in
      if check j then Some j' else None
    in
    let extract (y, j) = assert (j = Caqti_type.length row_type); Some y in
    Caqti_driver_lib.decode_row ~uri {read_value; skip_null} row_type 0
      |> Result.map extract

  module Make_connection_base
    (Connection_arg : sig
      val env : Caqti_driver_info.t -> string -> Caqti_query.t
      val uri : Uri.t
      val db : Mdb.t
    end) =
  struct
    open Connection_arg

    let env' = env driver_info

    let using_db_ref = ref false
    let using_db f =
      H.assert_single_use ~what:"MariaDB connection" using_db_ref f

    let request_failed ~query (errno, error) =
      Error (Caqti_error.request_failed ~uri ~query (Error_msg {errno; error}))

    let response_failed ~query (errno, error) =
      Error (Caqti_error.response_failed ~uri ~query (Error_msg {errno; error}))

    let response_rejected ~query msg =
      Error (Caqti_error.response_rejected ~uri ~query (Caqti_error.Msg msg))

    module Response = struct
      type ('b, +'m) t = {
        query: string;
        res: Mdb.Res.t;
        row_type: 'b Caqti_type.t;
      }

      let reject_f ~query fmt = ksprintf (response_rejected ~query) fmt

      let affected_count {res; _} = return (Ok (Mdb.Res.affected_rows res))
      let returned_count {res; _} = return (Ok (Mdb.Res.num_rows res))

      let decode_next_row ~query res row_type =
        Mdb.Res.fetch (module Mdb.Row.Array) res >|=
        (function
         | Ok None as r -> r
         | Ok (Some row) -> decode_row ~uri row_type row
         | Error err -> response_failed ~query err)

      let exec {res; query; _} =
        (match Mdb.Res.num_rows res with
         | 0 -> return (Ok ())
         | n -> return (reject_f ~query "Received %d tuples for exec." n))

      let find {query; res; row_type} =
        (match Mdb.Res.num_rows res with
         | 1 ->
            decode_next_row ~query res row_type >|=
            (function
             | Ok None -> assert false
             | Ok (Some y) -> Ok y
             | Error _ as r -> r)
         | n -> return (reject_f ~query "Received %d tuples for find." n))

      let find_opt {query; res; row_type} =
        (match Mdb.Res.num_rows res with
         | 0 -> return (Ok None)
         | 1 -> decode_next_row ~query res row_type
         | n -> return (reject_f ~query "Received %d tuples for find_opt." n))

      let fold f {query; res; row_type} =
        let rec loop acc =
          decode_next_row ~query res row_type >>=
          (function
           | Ok None -> return (Ok acc)
           | Ok (Some y) -> loop (f y acc)
           | Error _ as r -> return r) in
        loop

      let fold_s f {query; res; row_type} =
        let rec loop acc =
          decode_next_row ~query res row_type >>=
          (function
           | Ok None -> return (Ok acc)
           | Ok (Some y) -> f y acc >>=? loop
           | Error _ as r -> return r) in
        loop

      let iter_s f {query; res; row_type} =
        let rec loop () =
          decode_next_row ~query res row_type >>=
          (function
           | Ok None -> return (Ok ())
           | Ok (Some y) -> f y >>=? loop
           | Error _ as r -> return r) in
        loop ()

      let rec to_stream ({query; res; row_type} as resp) () =
        decode_next_row ~query res row_type >>=
        (function
          | Ok None -> return Stream.Nil
          | Error err -> return (Stream.Error err)
          | Ok (Some y) -> return (Stream.Cons (y, to_stream resp)))
    end

    type pcache_entry = {
      query: string;
      stmt: Mdb.Stmt.t;
      param_length: int;
      param_order: int list list;
      quotes: (int * string) list;
    }

    let pcache : (int, pcache_entry) Hashtbl.t = Hashtbl.create 23

    let pp_request_with_param ppf =
      Caqti_request.make_pp_with_param ~env ~driver_info () ppf

    let call ~f req param = using_db @@ fun () ->
      Log.debug ~src:request_log_src (fun f ->
        f "Sending %a" pp_request_with_param (req, param))
        >>= fun () ->

      let process {query; stmt; param_length; param_order; quotes} =
        let param_type = Caqti_request.param_type req in
        let row_type = Caqti_request.row_type req in
        let params = Array.make param_length `Null in
        List.iter (fun (j, s) -> params.(j) <- `String s) quotes;
        (match encode_param ~uri params param_type param param_order with
         | Error _ as r -> return r
         | Ok [] ->
            Mdb.Stmt.execute stmt params >>=
            (function
             | Error err -> return (request_failed ~query err)
             | Ok res -> f Response.{query; res; row_type})
         | Ok (_ :: _) -> assert false) in

      (match Caqti_request.query_id req with
       | None ->
          let templ = Caqti_request.query req driver_info in
          let templ = Caqti_query.expand ~final:true env' templ in
          let query = linear_query_string templ in
          Mdb.prepare db query >>=
          (function
           | Error err -> return (request_failed ~query err)
           | Ok stmt ->
              let param_length = linear_param_length templ in
              let param_order, quotes = linear_param_order templ in
              let pcache_entry =
                {query; stmt; param_length; param_order; quotes} in
              process pcache_entry >>= fun process_result ->
              Mdb.Stmt.close stmt >>=
              (function
               | Error (code, msg) ->
                  Log.warn (fun p ->
                    p "Ignoring error while closing statement: %d %s" code msg)
               | Ok () -> return ()) >|= fun () ->
              process_result)
       | Some id ->
          (try return (Ok (Hashtbl.find pcache id)) with
           | Not_found ->
              let templ = Caqti_request.query req driver_info in
              let templ = Caqti_query.expand ~final:true env' templ in
              let query = linear_query_string templ in
              Mdb.prepare db query >|=
              (function
               | Error err -> request_failed ~query err
               | Ok stmt ->
                  let param_length = linear_param_length templ in
                  let param_order, quotes = linear_param_order templ in
                  let pcache_entry =
                    {query; stmt; param_length; param_order; quotes} in
                  Hashtbl.add pcache id pcache_entry;
                  Ok pcache_entry))
            >>=? fun pcache_entry ->
          process pcache_entry >>= fun process_result ->
          Mdb.Stmt.reset pcache_entry.stmt >>= fun reset_result ->
          (match reset_result with
           | Ok () -> return ()
           | Error (code, msg) ->
              Log.warn (fun p ->
                p "Removing statement from cache due to failed reset: %d %s"
                  code msg) >|= fun () ->
              Hashtbl.remove pcache id) >|= fun () ->
          process_result)

    let deallocate req =
      (match Caqti_request.query_id req with
       | Some query_id ->
          (match Hashtbl.find pcache query_id with
           | exception Not_found -> return (Ok ())
           | pcache_entry ->
              Mdb.Stmt.close pcache_entry.stmt >>=
              (function
               | Ok () ->
                  Hashtbl.remove pcache query_id;
                  return (Ok ())
               | Error err ->
                  let query = sprintf "DEALLOCATE %d" query_id in
                  return (request_failed ~query err)))
       | None ->
          failwith "deallocate called on oneshot request")

    let disconnect () = using_db @@ fun () ->
      let close_stmt _ pcache_entry prologue =
        prologue >>= fun () ->
        Mdb.Stmt.close pcache_entry.stmt >>=
        (function
         | Ok () -> return ()
         | Error (code, msg) ->
            Log.warn (fun p ->
              p "Ignoring failure during disconnect: %d %s" code msg)) in
      Hashtbl.fold close_stmt pcache (return ()) >>= fun () ->
      Mdb.close db

    let validate () = using_db @@ fun () ->
      Mdb.ping db >|= function Ok () -> true | Error _ -> false

    let check f = f true (* FIXME *)

    let transaction_failed query (errno, error) =
      let msg = Error_msg {errno; error} in
      return (Error (Caqti_error.request_failed ~uri ~query msg))

    let start () = using_db @@ fun () ->
      Mdb.autocommit db false >>=
      (function
       | Ok () -> return (Ok ())
       | Error err -> transaction_failed "# SET autocommit = 0" err)

    let commit () = using_db @@ fun () ->
      Mdb.commit db >>= fun commit_result ->
      Mdb.autocommit db true >>= fun autocommit_result ->
      (match commit_result, autocommit_result with
       | Ok (), Ok () -> return (Ok ())
       | Error err, _ -> transaction_failed "# COMMIT" err
       | Ok (), Error err -> transaction_failed "# SET autocommit = 1" err)

    let rollback () = using_db @@ fun () ->
      Mdb.rollback db >>=
      (function
       | Ok () -> return (Ok ())
       | Error err -> transaction_failed "# ROLLBACK" err)

    let set_statement_timeout t =
      call ~f:Response.exec Q.set_statement_timeout
        (match t with
         | None -> 0.0
         | Some t -> max 0.000001 t)
  end

  type conninfo = {
    host: string option;
    user: string option;
    pass: string option;
    port: int option;
    db: string option;
    flags: Mdb.flag list option;
    config_group: string option;
  }

  let parse_uri uri =
    let host = Uri.host uri in
    let user = Uri.user uri in
    let pass = Uri.password uri in
    let port = Uri.port uri in
    let config_group = Uri.get_query_param uri "config-group" in
    (match Uri.path uri with
     | "" | "/" -> Ok None
     | path ->
        if Filename.dirname path <> "/" then
          let msg = Caqti_error.Msg "Bad URI path." in
          Error (Caqti_error.connect_rejected ~uri msg)
        else
          Ok (Some (Filename.basename path))) |>? fun db ->
    Ok {host; user; pass; port; db; flags = None; config_group}

  let connect_prim ~env ~tweaks_version:_ ~uri
        {host; user; pass; port; db; flags; config_group} =
    let config_group = match config_group with Some g -> g | None -> "caqti" in
    let socket = Uri.get_query_param uri "socket" in
    let options = [Mdb.Read_default_group config_group] in
    Mdb.connect ?host ?user ?pass ?db ?port ?socket ?flags ~options () >>=
    (function
     | Ok db ->
        let module B = Make_connection_base
          (struct
            let env = env
            let uri = uri
            let db = db
          end)
        in
        let module C = struct
          let driver_info = driver_info
          let driver_connection = None
          include B
          include Caqti_connection.Make_convenience (System) (B)
          include Caqti_connection.Make_populate (System) (B)
        end in
        Mdb.set_character_set db "utf8mb4" >>=
          (function
           | Ok () -> return ()
           | Error (err_no, err_msg) ->
              Log.warn (fun f ->
                f "Could not enable full Unicode coverage for this MariaDB \
                   connection. UTF-8 strings are likely limited to the BMP. \
                   set_character_set says %S (%d)" err_msg err_no))
          >>= fun () ->
        (* MariaDB returns local times but without time zone, so change it to
         * UTC for Caqti sessions. *)
        C.call ~f:(fun resp -> C.Response.exec resp) Q.set_utc () >|=
          (function
           | Ok () -> Ok (module C : CONNECTION)
           | Error err -> Error (`Post_connect err))
     | Error (errno, error) ->
        return @@
          Error (Caqti_error.connect_failed ~uri (Error_msg {errno; error})))

  let connect ?(env = no_env) ~tweaks_version uri =
    (match parse_uri uri with
     | Error _ as r -> return r
     | Ok conninfo -> connect_prim ~tweaks_version ~env ~uri conninfo)
end

let () = Caqti_connect.define_unix_driver "mariadb" (module Connect_functor)
