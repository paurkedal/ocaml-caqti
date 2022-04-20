(* Copyright (C) 2021--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_common_priv
open Postgresql_conv
open Printf

type Caqti_error.msg += Pgx_msg of string * Pgx.Error_response.t
let () =
  let pp ppf = function
   | Pgx_msg (msg, _) -> Format.pp_print_string ppf msg
   | _ -> assert false
  in
  let cause = function
   | Pgx_msg (_, error_response) ->
      cause_of_sqlstate error_response.Pgx.Error_response.code
   | _ ->
      assert false
  in
  Caqti_error.define_msg ~pp ~cause [%extension_constructor Pgx_msg]

let (|>?) r f = match r with Ok x -> f x | Error e -> Error e

let no_env _ _ = raise Not_found

let driver_info =
  Caqti_driver_info.create
    ~uri_scheme:"pgx"
    ~dialect_tag:`Pgsql
    ~parameter_style:(`Indexed (fun i -> "$" ^ string_of_int (succ i)))
    ~can_pool:true
    ~can_concur:true
    ~can_transact:true
    ~describe_has_typed_params:true
    ~describe_has_typed_fields:true
    ()

let pg_type_name : type a. a Caqti_type.field -> string = function
 | Caqti_type.Bool -> "bool"
 | Caqti_type.Int -> "int8"
 | Caqti_type.Int16 -> "int2"
 | Caqti_type.Int32 -> "int4"
 | Caqti_type.Int64 -> "int8"
 | Caqti_type.Float -> "float8"
 | Caqti_type.String -> "text"
 | Caqti_type.Octets -> "bytea"
 | Caqti_type.Pdate -> "date"
 | Caqti_type.Ptime -> "timestamptz"
 | Caqti_type.Ptime_span -> "interval"
 | Caqti_type.Enum name -> name
 | _ -> "unknown"

let query_string ~env templ =
  let templ = Caqti_query.expand ~final:true env templ in

  let rec extract_quotes = function
   | Caqti_query.Q s -> fun (n, acc) -> (n + 1, Pgx.Value.of_string s :: acc)
   | Caqti_query.L _ | Caqti_query.P _ -> Fun.id
   | Caqti_query.E _ -> fun _ -> assert false
   | Caqti_query.S qs -> List.fold extract_quotes qs
  in
  let nQ, rev_quotes = extract_quotes templ (0, []) in

  let buf = Buffer.create 64 in
  let rec write_query_string = function
   | Caqti_query.L s -> fun jQ -> Buffer.add_string buf s; jQ
   | Caqti_query.Q _ -> fun jQ -> bprintf buf "$%d" jQ; jQ + 1
   | Caqti_query.P j -> fun jQ -> bprintf buf "$%d" (nQ + 1 + j); jQ
   | Caqti_query.E _ -> assert false
   | Caqti_query.S qs -> List.fold write_query_string qs
  in
  let _jQ = write_query_string templ 1 in
  (Buffer.contents buf, rev_quotes)

let rec encode_field
    : type a. uri: Uri.t -> a Caqti_type.field -> a -> (Pgx.Value.t, _) result
    = fun ~uri field_type x ->
  (match field_type with
   | Caqti_type.Bool -> Ok (Pgx.Value.of_bool x)
   | Caqti_type.Int -> Ok (Pgx.Value.of_int x)
   | Caqti_type.Int16 -> Ok (Pgx.Value.of_int x)
   | Caqti_type.Int32 -> Ok (Pgx.Value.of_int32 x)
   | Caqti_type.Int64 -> Ok (Pgx.Value.of_int64 x)
   | Caqti_type.Float -> Ok (Pgx.Value.of_float x)
   | Caqti_type.String -> Ok (Pgx.Value.of_string x)
   | Caqti_type.Enum _ -> Ok (Pgx.Value.of_string x)
   | Caqti_type.Octets -> Ok (Pgx.Value.of_binary x)
   | Caqti_type.Pdate -> Ok (Pgx.Value.of_string (iso8601_of_pdate x))
   | Caqti_type.Ptime -> Ok (Pgx.Value.of_string (pgstring_of_pdate x))
   | Caqti_type.Ptime_span ->
      Ok (Pgx.Value.of_string (pgstring_of_ptime_span x))
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

let encode_param ~uri t param =
  let write_value ~uri ft fv acc =
    (match encode_field ~uri ft fv with
     | Ok y -> Ok (y :: acc)
     | Error _ as r -> r)
  in
  let write_null ~uri:_ _ acc =
    Ok (Pgx.Value.null :: acc)
  in
  Caqti_driver_lib.encode_param ~uri {write_value; write_null} t param []
    |> Result.map List.rev

let rec decode_field
    : type a. uri: Uri.t -> a Caqti_type.field -> Pgx.Value.t -> (a, _) result
    = fun ~uri field_type v ->
  let wrap_conv_exn f s =
    (match f s with
     | y -> Ok y
     | exception Pgx.Value.Conversion_failure msg_str ->
        let msg = Caqti_error.Msg msg_str in
        let typ = Caqti_type.field field_type in
        Error (Caqti_error.decode_rejected ~uri ~typ msg))
  in
  let wrap_conv_res f s =
    (match f s with
     | Ok _ as r -> r
     | Error msg_str ->
        let msg = Caqti_error.Msg msg_str in
        let typ = Caqti_type.field field_type in
        Error (Caqti_error.decode_rejected ~uri ~typ msg))
  in
  (match field_type with
   | Caqti_type.Bool -> wrap_conv_exn Pgx.Value.to_bool_exn v
   | Caqti_type.Int -> wrap_conv_exn Pgx.Value.to_int_exn v
   | Caqti_type.Int16 -> wrap_conv_exn Pgx.Value.to_int_exn v
   | Caqti_type.Int32 -> wrap_conv_exn Pgx.Value.to_int32_exn v
   | Caqti_type.Int64 -> wrap_conv_exn Pgx.Value.to_int64_exn v
   | Caqti_type.Float -> wrap_conv_exn Pgx.Value.to_float_exn v
   | Caqti_type.String -> wrap_conv_exn Pgx.Value.to_string_exn v
   | Caqti_type.Enum _ -> wrap_conv_exn Pgx.Value.to_string_exn v
   | Caqti_type.Octets -> wrap_conv_exn Pgx.Value.to_binary_exn v
   | Caqti_type.Pdate ->
      v |>  wrap_conv_exn Pgx.Value.to_string_exn
        |>? wrap_conv_res pdate_of_iso8601
   | Caqti_type.Ptime ->
      v |>  wrap_conv_exn Pgx.Value.to_string_exn
        |>? wrap_conv_res ptime_of_rfc3339_utc
   | Caqti_type.Ptime_span ->
      v |>  wrap_conv_exn Pgx.Value.to_string_exn
        |>? wrap_conv_res ptime_span_of_pgstring
   | _ ->
      (match Caqti_type.Field.coding driver_info field_type with
       | None -> Error (Caqti_error.decode_missing ~uri ~field_type ())
       | Some (Caqti_type.Field.Coding {rep; decode; _}) ->
          (match decode_field ~uri rep v with
           | Ok y ->
              (match decode y with
               | Ok _ as r -> r
               | Error msg ->
                  let msg = Caqti_error.Msg msg in
                  let typ = Caqti_type.field field_type in
                  Error (Caqti_error.decode_rejected ~uri ~typ msg))
           | Error _ as r -> r)))

let decode_row ~uri row_type fields =
  let read_value ~uri ft = function
   | [] -> assert false
   | field :: fields ->
      (match decode_field ~uri ft field with
       | Ok y -> Ok (y, fields)
       | Error _ as r -> r)
  in
  let rec skip_null n xs =
    if n = 0 then Some xs else
    (match xs with
     | [] -> assert false
     | x :: xs' when Pgx.Value.(compare null) x = 0 -> skip_null (n - 1) xs'
     | _ :: _ -> None)
  in
  Caqti_driver_lib.decode_row ~uri {read_value; skip_null} row_type fields
    |> Result.map (fun (y, fields) -> assert (fields = []); y)

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let select_type_oid = "SELECT oid FROM pg_catalog.pg_type WHERE typname = $1"

  let set_timezone_to_utc =
    (unit ->. unit) ~oneshot:true "SET TimeZone TO 'UTC'"
end

type connect_arg = {
  host: string option;
  port: int option;
  user: string option;
  password: string option;
  database: string option;
  unix_domain_socket_dir: string option;
}

let (let/?) m f = match m with Ok x -> f x | Error _ as r -> r

let parse_uri uri =
  let reject msg =
    let msg = Caqti_error.Msg msg in
    Error (Caqti_error.connect_rejected ~uri msg)
  in
  let/? host_or_unix_domain_socket_dir =
    (match Uri.host uri, Uri.get_query_param uri "host" with
     | None, None -> Ok None
     | Some s, None | None, Some s -> Ok (Some s)
     | Some _, Some _ -> reject "Conflicting host in URI.")
  in
  let host, unix_domain_socket_dir =
    (match host_or_unix_domain_socket_dir with
     | None | Some "" -> (None, None)
     | Some s -> if s.[0] = '/' then (None, Some s) else (Some s, None))
  in
  let/? port =
    (match Uri.port uri, Uri.get_query_param uri "port" with
     | None, None -> Ok None
     | Some i, None -> Ok (Some i)
     | None, Some s ->
        (try Ok (Some (int_of_string s)) with
         | Failure _ -> reject "Non-integer port number in URI.")
     | Some _, Some _ -> reject "Conflicting host in URI.")
  in
  let/? user =
    (match Uri.user uri, Uri.get_query_param uri "user" with
     | None, None -> Ok None
     | Some s, None | None, Some s -> Ok (Some s)
     | Some _, Some _ -> reject "Conflicting user in URI.")
  in
  let/? password =
    (match Uri.password uri, Uri.get_query_param uri "password" with
     | None, None -> Ok None
     | Some s, None | None, Some s -> Ok (Some s)
     | Some _, Some _ -> reject "Conflicting password in URI.")
  in
  let/? database =
    (match String.split_on_char '/' (Uri.path uri),
           Uri.get_query_param uri "dbname" with
     | [], _ -> assert false
     | [""], dbname -> Ok dbname
     | [""; dbname], None -> Ok (Some dbname)
     | [""; _], Some _ -> reject "Conflicting dbname in URI."
     | _ -> reject "Too many path components in URI.")
  in
  Ok {host; port; user; password; database; unix_domain_socket_dir}

module Connect_functor (System : Caqti_platform_net.Sig.System) = struct
  open System

  let ( let* ) = ( >>= )
  let ( let+ ) = ( >|= )
  let ( let*? ) m f = m >>= function Ok x -> f x | Error _ as r -> return r
  let ( let+? ) m f = m >|= function Ok x -> Ok (f x) | Error _ as r -> r
  let ( >>=? ) = ( let*? )
  let ( >|=? ) = ( let+? )

  let intercept h f =
    catch
      (fun () -> f () >|= fun y -> Ok y)
      (function
       | Pgx.PostgreSQL_Error (msg, err) ->
          return (Error (h (Pgx_msg (msg, err))))
       | exn ->
          raise exn)

  let intercept_request_failed ~uri ~query =
    intercept (Caqti_error.request_failed ~uri ~query)
  let intercept_connect_failed ~uri =
    intercept (Caqti_error.connect_failed ~uri)

  module Pgx_with_io = Pgx.Make (struct
    type 'a t = 'a future
    let return = return
    let ( >>= ) = ( >>= )
    let catch = catch

    include Networking

    (* TODO *)
    type ssl_config
    let upgrade_ssl = `Not_supported

    let output_binary_int oc x =
      let buf = Bytes.create 4 in
      Bytes.set_int32_be buf 0 (Int32.of_int x);
      output_string oc (Bytes.to_string buf)

    let input_binary_int ic =
      let buf = Bytes.create 4 in
      really_input ic buf 0 4 >|= fun () ->
      Int32.to_int (Bytes.get_int32_be buf 0)

    let getlogin () = failwith "The DB user must be provided."

    let debug msg = Log.debug (fun f -> f "%s" msg)

    let protect f ~finally = System.finally f finally

    module Sequencer = struct
      type 'a monad = 'a future
      include Sequencer
    end
  end)

  module Make_connection_base
    (Connection_arg : sig
      val env : Caqti_driver_info.t -> string -> Caqti_query.t
      val uri : Uri.t
      val db_arg : Pgx_with_io.t
      val select_type_oid : Pgx_with_io.Prepared.s
    end) =
  struct
    open Connection_arg

    let env' = env driver_info

    let db_txn = ref None

    module Response = struct

      type ('b, 'm) t = {
        row_type: 'b Caqti_type.t;
        rows: Pgx.Value.t list list;
      }

      let returned_count {rows; _} = return (Ok (List.length rows))
      let affected_count _ = return (Error `Unsupported)

      let exec _ = return (Ok ())

      let find resp = return
        (match resp.rows with
         | [row] -> decode_row ~uri resp.row_type row
         | _ -> assert false)

      let find_opt resp = return
        (match resp.rows with
         | [] -> Ok None
         | [row] ->
            (match decode_row ~uri resp.row_type row with
             | Ok x -> Ok (Some x)
             | Error _ as r -> r)
         | _ -> assert false)

      let fold f resp acc =
        let rec loop = function
         | [] -> Stdlib.Result.ok
         | row :: rows -> fun acc ->
            (match decode_row ~uri resp.row_type row with
             | Ok x -> acc |> f x |> loop rows
             | Error _ as r -> r)
        in
        return (loop resp.rows acc)

      let fold_s f resp acc =
        let rec loop = function
         | [] -> fun acc -> return (Ok acc)
         | row :: rows -> fun acc ->
            (match decode_row ~uri resp.row_type row with
             | Ok x -> f x acc >>=? loop rows
             | Error _ as r -> return r)
        in
        loop resp.rows acc

      let iter_s f resp =
        let rec loop = function
         | [] -> return (Ok ())
         | row :: rows ->
            (match decode_row ~uri resp.row_type row with
             | Ok x -> f x >>=? fun () -> loop rows
             | Error _ as r -> return r)
        in
        loop resp.rows

      let to_stream resp =
        Stream.of_list resp.rows
          |> Stream.map_result ~f:(decode_row ~uri resp.row_type)
    end

    type prepared = {
      query: string;
      pgx_prepared: Pgx_with_io.Prepared.s;
      rev_quotes: Pgx.Value.t list;
    }

    let in_use = ref false
    let prepare_cache : (int, prepared) Hashtbl.t = Hashtbl.create 19

    let reset _ = return () (* FIXME *)

    let using_db f =
      if !in_use then
        failwith "Invalid concurrent usage of PostgreSQL connection detected.";
      in_use := true;
      let db = match !db_txn with None -> db_arg | Some db -> db in
      cleanup
        (fun () -> f db >|= fun res -> in_use := false; res)
        (fun () -> reset db >|= fun _ -> in_use := false)

    let check_mult ~query req rows =
      let reject msg =
        Error (Caqti_error.response_rejected ~uri ~query (Caqti_error.Msg msg))
      in
      (match Caqti_mult.expose (Caqti_request.row_mult req), rows with
       | (`Zero | `Zero_or_one), []
       | (`One | `Zero_or_one), [_]
       | `Zero_or_more, _ ->
          Ok ()
       | `Zero, _ :: _ ->
          reject "Received multiple rows where none expected."
       | `One, [] ->
          reject "Received no rows where one expected."
       | `One, _ :: _ :: _ ->
          reject "Received more than one row when one expected."
       | `Zero_or_one, _ :: _ :: _ ->
          reject "Received more than one row when at most one expected.")

    let type_oid_cache = Hashtbl.create 11

    let field_type_oid ft =
      let name = pg_type_name ft in
      (match Hashtbl.find_opt type_oid_cache name with
       | Some oid -> return (Ok oid)
       | None ->
          let params = [Pgx.Value.of_string name] in
          let*? row =
            intercept_request_failed ~uri ~query:Q.select_type_oid (fun () ->
              Pgx_with_io.Prepared.execute select_type_oid ~params)
          in
          let fail s =
            let msg = Caqti_error.Msg s in
            return (Error
              (Caqti_error.request_failed ~uri ~query:Q.select_type_oid msg))
          in
          let failf fmt = Format.kasprintf fail fmt in
          (match row with
           | [[v]] ->
              (match Pgx.Value.to_int32 v with
               | Some oid ->
                  Hashtbl.add type_oid_cache name oid;
                  return (Ok oid)
               | None ->
                  failf "Expected an int32 in response from OID request.")
           | [] ->
              failf "OID for type %a not found." Caqti_type.Field.pp ft
           | [_] ->
              failf "Expected single field result from OID request."
           | _ ->
              failf "Expected at most one row from OID request."))

    let type_oids param_type =
      let rec loop :
          type a. a Caqti_type.t -> Pgx.oid list ->
          (Pgx.oid list, _) result future =
        (function
         | Unit -> fun acc -> return (Ok acc)
         | Field ft -> fun acc -> field_type_oid ft >|=? fun ft -> ft :: acc
         | Option t -> loop t
         | Tup2 (t1, t2) ->
            fun acc -> acc |> loop t2 >>=? loop t1
         | Tup3 (t1, t2, t3) ->
            fun acc -> acc |> loop t3 >>=? loop t2 >>=? loop t1
         | Tup4 (t1, t2, t3, t4) ->
            fun acc -> acc |> loop t4 >>=? loop t3 >>=? loop t2 >>=? loop t1
         | Custom {rep; _} -> loop rep
         | Annot (_, t) -> loop t)
      in
      loop (Caqti_type.option param_type) []

    let call ~f req param =
      using_db @@ fun db ->
      let*? query, rows =
        (match Caqti_request.query_id req with
         | Some query_id ->
            let param_type = Caqti_request.param_type req in
            let*? pq =
              (match Hashtbl.find_opt prepare_cache query_id with
               | Some pq -> return (Ok pq)
               | None ->
                  let templ = Caqti_request.query req driver_info in
                  let query, rev_quotes = query_string ~env:env' templ in
                  let name = sprintf "q%d" query_id in
                  let*? param_type_oids = type_oids param_type in
                  let*? quote_type_oids =
                    let+? string_oid = field_type_oid Caqti_type.String in
                    List.rev_map (fun _ -> string_oid) rev_quotes
                  in
                  let types = List.rev_append quote_type_oids param_type_oids in
                  let+? pgx_prepared =
                    intercept_request_failed ~uri ~query (fun () ->
                      Pgx_with_io.Prepared.prepare ~name ~query ~types db)
                  in
                  let pq = {query; pgx_prepared; rev_quotes} in
                  Hashtbl.add prepare_cache query_id pq; pq)
            in
            (match encode_param ~uri param_type param with
             | Error _ as r -> return r
             | Ok regular_params ->
                let params = List.rev_append pq.rev_quotes regular_params in
                let+? rows =
                  intercept_request_failed ~uri ~query:pq.query (fun () ->
                    Pgx_with_io.Prepared.execute pq.pgx_prepared ~params)
                in
                (pq.query, rows))
         | None ->
            let templ = Caqti_request.query req driver_info in
            let query, rev_quotes = query_string ~env:env' templ in
            let param_type = Caqti_request.param_type req in
            (match encode_param ~uri param_type param with
             | Error _ as r -> return r
             | Ok regular_params ->
                let params = List.rev_append rev_quotes regular_params in
                let+? rows =
                  intercept_request_failed ~uri ~query (fun () ->
                    Pgx_with_io.execute db query ~params)
                in
                (query, rows)))
      in
      let*? () = return (check_mult ~query req rows) in
      f {Response.row_type = Caqti_request.row_type req; rows}

    let deallocate req =
      (match Caqti_request.query_id req with
       | Some query_id ->
          (match Hashtbl.find_opt prepare_cache query_id with
           | None ->
              return (Ok ())
           | Some prepared ->
              Hashtbl.remove prepare_cache query_id;
              intercept_request_failed ~uri ~query:"DEALLOCATE"
                (fun () -> Pgx_with_io.Prepared.close prepared.pgx_prepared))
       | None ->
          failwith "deallocate called on oneshot request")

    let disconnect () =
      using_db @@ fun _ ->
      catch
        (fun () ->
          (match !db_txn with
           | None -> return ()
           | Some db -> Pgx_with_io.close db) >>= fun () ->
          Pgx_with_io.close db_arg)
        (function
         | Pgx.PostgreSQL_Error (msg, _) ->
            Log.err (fun f -> f "Failed to disconnect %a: %s" Uri.pp uri msg)
         | exn ->
            raise exn)

    let validate () =
      (* No need to handle exceptions here, since alive is a catch-all wrapper
       * around ping. *)
      using_db Pgx_with_io.alive

    let check f = f true

    let exec q p = call ~f:Response.exec q p

    let start () =
      (match !db_txn with
       | None ->
          intercept_request_failed ~uri ~query:"BEGIN" begin fun () ->
            Pgx_with_io.begin_work db_arg >|= fun db ->
            db_txn := Some db
          end
       | Some _ ->
          failwith "A transaction already in progress.")

    let commit () =
      (match !db_txn with
       | Some db ->
          intercept_request_failed ~uri ~query:"COMMIT" begin fun () ->
            db_txn := None;
            Pgx_with_io.commit db
          end
       | None ->
          failwith "No transaction to commit.")

    let rollback () =
      (match !db_txn with
       | Some db ->
          intercept_request_failed ~uri ~query:"ROLLBACK" begin fun () ->
            db_txn := None;
            Pgx_with_io.rollback db
          end
       | None ->
          failwith "No transaction to roll back.")

    let set_statement_timeout t =
      let t_arg =
        (match t with
         | None -> 0
         | Some t -> max 1 (int_of_float (t *. 1000.0 +. 500.0)))
      in
      let query = sprintf "SET statement_timeout TO %d" t_arg in
      using_db @@ fun db ->
        intercept_request_failed ~uri ~query @@ fun () ->
          Pgx_with_io.execute_unit db query
  end

  let driver_info = driver_info

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a future := 'a future
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t

  let setup (module Connection : CONNECTION) =
    Connection.exec Q.set_timezone_to_utc () >|= function
     | Ok () -> Ok ()
     | Error err -> Error (`Post_connect err)

  let connect ?(env = no_env) ~tweaks_version:_ uri =
    let*? {host; port; user; password; database; unix_domain_socket_dir} =
      return (parse_uri uri)
    in
    let*? db =
      intercept_connect_failed ~uri
        (Pgx_with_io.connect
          ?host ?port ?user ?password ?database ?unix_domain_socket_dir)
    in
    let*? select_type_oid =
      intercept_request_failed ~uri ~query:Q.select_type_oid
        (fun () -> Pgx_with_io.Prepared.prepare db ~query:Q.select_type_oid)
      >|= Result.map_error (fun err -> `Post_connect err)
    in
    let module B = Make_connection_base (struct
      let env = env
      let uri = uri
      let db_arg = db
      let select_type_oid = select_type_oid
    end) in
    let module Connection = struct
      let driver_info = driver_info
      include B
      include Caqti_connection.Make_convenience (System) (B)
      include Caqti_connection.Make_populate (System) (B)
    end in
    let+? () = setup (module Connection) in
    (module Connection : CONNECTION)
end

let () =
  Caqti_platform_net.define_driver "pgx" (module Connect_functor)
