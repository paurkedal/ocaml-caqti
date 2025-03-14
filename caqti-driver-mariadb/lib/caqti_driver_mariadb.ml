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

open Caqti_template
open Caqti_platform
open Printf

let (|>?) = Result.bind

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
  open Caqti_template.Create
  let set_utc =
    direct T.(unit -->. unit) "SET time_zone = '+00:00'"
  let set_statement_timeout =
    direct T.(float -->. unit) "SET max_statement_time = ?"
end

module Connect_functor
  (System : Caqti_platform.System_sig.S)
  (System_unix : Caqti_platform_unix.System_sig.S
    with type 'a fiber := 'a System.Fiber.t
     and type stdenv := System.stdenv) =
struct
  open System
  open System.Fiber.Infix
  open System_utils.Monad_syntax (System.Fiber)
  module H = Connection_utils.Make_helpers (System)

  let (>>=?) m f = m >>= function Ok x -> f x | Error _ as r -> Fiber.return r

  let rec fold_s_list f =
    (function
     | [] -> Fiber.return ()
     | x :: xs -> f x >>= fun () -> fold_s_list f xs)

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a fiber := 'a Fiber.t
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t

  let dialect =
    Dialect.create_mysql ~server_version:(Version.of_string_unsafe "") ()
  let driver_info =
    Caqti_driver_info.of_dialect dialect

  (* We need to pass stdenv into the below wait, in order to implement
   * timout for EIO, since it uses stdenv#clock.  This means that our Mdb
   * instance becomes dependent on stdenv and will therefore be
   * instantiated for each connection. *)
  module Pass_stdenv
    (Connect_env : sig val stdenv : stdenv end) =
  struct
    open Connect_env

    module Mdb = Mariadb.Nonblocking.Make
      (struct
        open System

        module IO = struct
          type 'a future = 'a Fiber.t
          let (>>=) = Fiber.Infix.(>>=)
          let return = Fiber.return
        end

        let wait db status =
          Mariadb.Nonblocking.fd db |> System_unix.Unix.wrap_fd @@ fun fd ->
          System_unix.Unix.poll
            ~stdenv
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

    let encode_field : type a. a Field_type.t -> a -> Mdb.Field.value =
      fun field_type x ->
      (match field_type with
       | Bool -> `Int (if x then 1 else 0)
       | Int -> `Int x
       | Int16 -> `Int x
       | Int32 -> `Int (Int32.to_int x)
       | Int64 -> `Int (Int64.to_int x)
       | Float -> `Float x
       | String -> `String x
       | Enum _ -> `String x
       | Octets -> `Bytes (Bytes.of_string x)
       | Pdate ->
          let year, month, day = Ptime.to_date x in
          `Time (Mdb.Time.date ~year ~month ~day ())
       | Ptime ->
          let (year, month, day), ((hour, minute, second), _) =
            Ptime.to_date_time ~tz_offset_s:0 x in
          let ps = snd (Ptime.Span.to_d_ps (Ptime.to_span x)) in
          let tss = Int64.rem ps 1_000_000_000_000L in
          let microsecond = Int64.to_int (Int64.div tss 1_000_000L) in
          `Time (Mdb.Time.datetime
                  ~year ~month ~day ~hour ~minute ~second ~microsecond ())
       | Ptime_span ->
          `Float (Ptime.Span.to_float_s x))

    let decode_field
        : type b. uri: Uri.t -> b Field_type.t -> Mdb.Field.t -> b =
      fun ~uri field_type field ->
      try match field_type with
       | Bool -> Mdb_ext.Field.bool field
       | Int -> Mdb_ext.Field.int field
       | Int16 -> Mdb_ext.Field.int field
       | Int32 -> Mdb_ext.Field.int32 field
       | Int64 -> Mdb_ext.Field.int64 field
       | Float -> Mdb.Field.float field
       | String -> Mdb_ext.Field.string field
       | Enum _ -> Mdb_ext.Field.string field
       | Octets -> Mdb_ext.Field.string field
       | Pdate ->
          let t = Mdb.Field.time field in
          let date = Mdb.Time.(year t, month t, day t) in
          (match Ptime.of_date date with
           | None -> failwith "Ptime.of_date"
           | Some t -> t)
       | Ptime ->
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
               | Some t' -> t'))
       | Ptime_span ->
          let t = Mdb_ext.Field.float field in
          (match Ptime.Span.of_float_s t with
           | None -> failwith "Ptime.Span.of_float_s"
           | Some t -> t)
      with
       | Failure _ ->
          let typename = Mdb_ext.Field.typename field in
          let msg = Caqti_error.Msg ("Received " ^ typename ^ ".") in
          let typ = Row_type.field field_type in
          Request_utils.raise_decode_rejected ~uri ~typ msg

    let encode_param ~uri params t v acc =
      let write_value ~uri:_ ft fv os =
        assert (os <> []);
        let v = encode_field ft fv in
        List.iter (fun j -> params.(j) <- v) (List.hd os);
        List.tl os
      in
      let write_null ~uri:_ _ os = List.tl os in
      try
        Ok (Request_utils.encode_param ~uri {write_value; write_null} t v acc)
      with Caqti_error.Exn (#Caqti_error.call as err) ->
        Error err

    let decode_row ~uri row_type =
      let read_value ~uri ft (row, j) =
        let fv = decode_field ~uri ft row.(j) in
        (fv, (row, j + 1))
      in
      let skip_null n (row, j) =
        let j' = j + n in
        let rec check k =
          k = j' || Mdb.Field.null_value row.(k) && check (k + 1)
        in
        if check j then Some (row, j') else None
      in
      let decode =
        Request_utils.decode_row ~uri {read_value; skip_null} row_type
      in
      fun row ->
        try
          let (y, (_, j)) = decode (row, 0) in
          assert (j = Row_type.length row_type);
          Ok (Some y)
        with Caqti_error.Exn (#Caqti_error.retrieve as err) ->
          Error err

    module Make_connection_base
      (Connection_arg : sig
        val subst : Query.subst
        val uri : Uri.t
        val db : Mdb.t
        val dynamic_capacity : int
      end) =
    struct
      open Connection_arg

      let dialect = dialect

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
          row_type: 'b Row_type.t;
        }

        let reject_f ~query fmt = ksprintf (response_rejected ~query) fmt

        let affected_count {res; _} = Fiber.return (Ok (Mdb.Res.affected_rows res))
        let returned_count {res; _} = Fiber.return (Ok (Mdb.Res.num_rows res))

        let decode_next_row ~query row_type =
          let decode = decode_row ~uri row_type in
          fun res ->
            Mdb.Res.fetch (module Mdb.Row.Array) res >|= function
             | Ok None as r -> r
             | Ok (Some row) -> decode row
             | Error err -> response_failed ~query err

        let exec {res; query; _} =
          (match Mdb.Res.num_rows res with
           | 0 -> Fiber.return (Ok ())
           | n -> Fiber.return (reject_f ~query "Received %d tuples for exec." n))

        let find {query; res; row_type} =
          (match Mdb.Res.num_rows res with
           | 1 ->
              decode_next_row ~query row_type res >|=
              (function
               | Ok None -> assert false
               | Ok (Some y) -> Ok y
               | Error _ as r -> r)
           | n -> Fiber.return (reject_f ~query "Received %d tuples for find." n))

        let find_opt {query; res; row_type} =
          (match Mdb.Res.num_rows res with
           | 0 -> Fiber.return (Ok None)
           | 1 -> decode_next_row ~query row_type res
           | n -> Fiber.return (reject_f ~query "Received %d tuples for find_opt." n))

        let fold f {query; res; row_type} =
          let decode = decode_next_row ~query row_type in
          let rec loop acc =
            decode res >>= function
             | Ok None -> Fiber.return (Ok acc)
             | Ok (Some y) -> loop (f y acc)
             | Error _ as r -> Fiber.return r
          in
          loop

        let fold_s f {query; res; row_type} =
          let decode = decode_next_row ~query row_type in
          let rec loop acc =
            decode res >>= function
             | Ok None -> Fiber.return (Ok acc)
             | Ok (Some y) -> f y acc >>=? loop
             | Error _ as r -> Fiber.return r
          in
          loop

        let iter_s f {query; res; row_type} =
          let decode = decode_next_row ~query row_type in
          let rec loop () =
            decode res >>= function
             | Ok None -> Fiber.return (Ok ())
             | Ok (Some y) -> f y >>=? loop
             | Error _ as r -> Fiber.return r
          in
          loop ()

        let to_stream {query; res; row_type} =
          let decode = decode_next_row ~query row_type in
          let rec loop () =
            decode res >>= function
             | Ok None -> Fiber.return Stream.Nil
             | Error err -> Fiber.return (Stream.Error err)
             | Ok (Some y) -> Fiber.return (Stream.Cons (y, loop))
          in
          loop
      end

      type prepared = {
        query: string;
        stmt: Mdb.Stmt.t;
        param_length: int;
        param_order: int list list;
        quotes: Request_utils.linear_param list;
      }

      module Pcache =
        Request_cache.Make (struct type t = prepared let weight _ = 1 end)

      let pcache : Pcache.t = Pcache.create ~dynamic_capacity dialect

      let pp_request_with_param ppf =
        Request.make_pp_with_param ~subst ~dialect () ppf

      let free_prepared prepared =
        let rewrite_error = function
         | Ok () -> Ok ()
         | Error err ->
            let query = sprintf "DEALLOCATE (%s)" prepared.query in
            request_failed ~query err
        in
        Mdb.Stmt.close prepared.stmt >|= rewrite_error

      let deallocate req =
        (match Request.prepare_policy req with
         | Dynamic | Static ->
            (match Pcache.deallocate pcache req with
             | None -> Fiber.return (Ok ())
             | Some (prepared, commit) -> free_prepared prepared >|=? commit)
         | Direct ->
            failwith "deallocate called on oneshot request")

      let deallocate_some () =
        let rec loop = function
         | [] -> Fiber.return (Ok ())
         | prepared :: orphans ->
            let*? () = free_prepared prepared in
            loop orphans
        in
        let orphans, commit = Pcache.trim pcache in
        loop orphans >|=? commit

      let prepare request =
        deallocate_some () >>=? fun () ->
        (match Pcache.find_and_promote pcache request with
         | Some prepared ->
            Fiber.return (Ok prepared)
         | None ->
            let templ = Request.query request dialect in
            let templ = Query.expand ~final:true subst templ in
            let query = Request_utils.linear_query_string templ in
            Mdb.prepare db query >|= function
             | Error err -> request_failed ~query err
             | Ok stmt ->
                let param_length = Request_utils.linear_param_length templ in
                let param_order, quotes =
                  Request_utils.linear_param_order templ in
                let prepared =
                  {query; stmt; param_length; param_order; quotes} in
                Pcache.add pcache request prepared;
                Ok prepared)

      let call ~f req param = using_db @@ fun () ->
        Log.debug ~src:Logging.request_log_src (fun f ->
          f "Sending %a" pp_request_with_param (req, param))
          >>= fun () ->

        let process {query; stmt; param_length; param_order; quotes} =
          let param_type = Request.param_type req in
          let row_type = Request.row_type req in
          let params = Array.make param_length `Null in
          List.iter
            (fun (Request_utils.Linear_param (j, t, v)) ->
              params.(j) <- encode_field t v)
            quotes;
          (match encode_param ~uri params param_type param param_order with
           | Error _ as r -> Fiber.return r
           | Ok [] ->
              Mdb.Stmt.execute stmt params >>=
              (function
               | Error err -> Fiber.return (request_failed ~query err)
               | Ok res -> f Response.{query; res; row_type})
           | Ok (_ :: _) -> assert false) in

        (match Request.prepare_policy req with
         | Direct ->
            let templ = Request.query req dialect in
            let templ = Query.expand ~final:true subst templ in
            let query = Request_utils.linear_query_string templ in
            Mdb.prepare db query >>=
            (function
             | Error err -> Fiber.return (request_failed ~query err)
             | Ok stmt ->
                let param_length = Request_utils.linear_param_length templ in
                let param_order, quotes =
                  Request_utils.linear_param_order templ in
                let prepared =
                  {query; stmt; param_length; param_order; quotes} in
                process prepared >>= fun process_result ->
                Mdb.Stmt.close stmt >>=
                (function
                 | Error (code, msg) ->
                    Log.warn (fun p ->
                      p "Ignoring error while closing statement: %d %s" code msg)
                 | Ok () -> Fiber.return ()) >|= fun () ->
                process_result)
         | Dynamic | Static ->
            let*? prepared = prepare req in
            let* process_result = process prepared in
            let+ () =
              Mdb.Stmt.reset prepared.stmt >>=
              (function
               | Ok () -> Fiber.return ()
               | Error (code, msg) ->
                  Log.warn (fun p ->
                    p "Removing statement from cache due to failed reset: %d %s"
                      code msg) >|= fun () ->
                  Pcache.remove_and_discard pcache req)
            in
            process_result)

      let disconnect () = using_db @@ fun () ->
        let close_stmt prepared =
          Mdb.Stmt.close prepared.stmt >>=
          (function
           | Ok () -> Fiber.return ()
           | Error (code, msg) ->
              Log.warn (fun p ->
                p "Ignoring failure during disconnect: %d %s" code msg))
        in
        fold_s_list close_stmt (Pcache.elements pcache) >>= fun () ->
        Pcache.clear_and_discard pcache;
        Mdb.close db

      let validate () = using_db @@ fun () ->
        Mdb.ping db >|= function Ok () -> true | Error _ -> false

      let check f = f true (* FIXME *)

      let transaction_failed query (errno, error) =
        let msg = Error_msg {errno; error} in
        Fiber.return (Error (Caqti_error.request_failed ~uri ~query msg))

      let start () = using_db @@ fun () ->
        Mdb.autocommit db false >>=
        (function
         | Ok () -> Fiber.return (Ok ())
         | Error err -> transaction_failed "# SET autocommit = 0" err)

      let commit () = using_db @@ fun () ->
        Mdb.commit db >>= fun commit_result ->
        Mdb.autocommit db true >>= fun autocommit_result ->
        (match commit_result, autocommit_result with
         | Ok (), Ok () -> Fiber.return (Ok ())
         | Error err, _ -> transaction_failed "# COMMIT" err
         | Ok (), Error err -> transaction_failed "# SET autocommit = 1" err)

      let rollback () = using_db @@ fun () ->
        Mdb.rollback db >>=
        (function
         | Ok () -> Fiber.return (Ok ())
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
  end

  let connect ~sw:_ ~stdenv ~subst ~config uri =
    let module With_stdenv = Pass_stdenv (struct let stdenv = stdenv end) in
    let open With_stdenv in

    Fiber.return (parse_uri uri)
      >>=? fun {host; user; pass; port; db; flags; config_group} ->

    let config_group = match config_group with Some g -> g | None -> "caqti" in
    let socket = Uri.get_query_param uri "socket" in
    let options = [Mdb.Read_default_group config_group] in
    Mdb.connect ?host ?user ?pass ?db ?port ?socket ?flags ~options () >>=
    (function
     | Ok db ->
        let module B = Make_connection_base
          (struct
            let subst = subst dialect
            let uri = uri
            let db = db
            let dynamic_capacity =
              Caqti_connect_config.(get dynamic_prepare_capacity config)
          end)
        in
        let module C = struct
          let driver_info = driver_info
          let driver_connection = None
          include B
          include Connection_utils.Make_convenience (System) (B)
          include Connection_utils.Make_populate (System) (B)
        end in
        Mdb.set_character_set db "utf8mb4" >>=
          (function
           | Ok () -> Fiber.return ()
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
        Fiber.return @@
          Error (Caqti_error.connect_failed ~uri (Error_msg {errno; error})))
end

let () =
  Caqti_platform_unix.Driver_loader.register "mariadb" (module Connect_functor)
