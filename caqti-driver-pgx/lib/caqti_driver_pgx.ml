(* Copyright (C) 2021--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_platform
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

let no_env _ _ = raise Not_found

let driver_info =
  Caqti_driver_info.create
    ~uri_scheme:"pgx"
    ~dialect_tag:`Pgsql
    ~parameter_style:(`Indexed (fun i -> "$" ^ string_of_int (succ i)))
    ~can_pool:true
    ~can_concur:true
    ~can_transact:true
    ()

let pg_type_name : type a. a Caqti_type.Field.t -> string = function
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
   | Caqti_query.S qs -> List_ext.fold extract_quotes qs
  in
  let nQ, rev_quotes = extract_quotes templ (0, []) in

  let buf = Buffer.create 64 in
  let rec write_query_string = function
   | Caqti_query.L s -> fun jQ -> Buffer.add_string buf s; jQ
   | Caqti_query.Q _ -> fun jQ -> bprintf buf "$%d" jQ; jQ + 1
   | Caqti_query.P j -> fun jQ -> bprintf buf "$%d" (nQ + 1 + j); jQ
   | Caqti_query.E _ -> assert false
   | Caqti_query.S qs -> List_ext.fold write_query_string qs
  in
  let _jQ = write_query_string templ 1 in
  (Buffer.contents buf, rev_quotes)

let rec encode_field
    : type a. uri: Uri.t -> a Caqti_type.Field.t -> a -> Pgx.Value.t
    = fun ~uri field_type x ->
  (match field_type with
   | Caqti_type.Bool -> Pgx.Value.of_bool x
   | Caqti_type.Int -> Pgx.Value.of_int x
   | Caqti_type.Int16 -> Pgx.Value.of_int x
   | Caqti_type.Int32 -> Pgx.Value.of_int32 x
   | Caqti_type.Int64 -> Pgx.Value.of_int64 x
   | Caqti_type.Float -> Pgx.Value.of_float x
   | Caqti_type.String -> Pgx.Value.of_string x
   | Caqti_type.Enum _ -> Pgx.Value.of_string x
   | Caqti_type.Octets -> Pgx.Value.of_binary x
   | Caqti_type.Pdate -> Pgx.Value.of_string (Conv.iso8601_of_pdate x)
   | Caqti_type.Ptime -> Pgx.Value.of_string (pgstring_of_pdate x)
   | Caqti_type.Ptime_span -> Pgx.Value.of_string (pgstring_of_ptime_span x)
   | _ ->
      (match Caqti_type.Field.coding driver_info field_type with
       | None -> Request_utils.raise_encode_missing ~uri ~field_type ()
       | Some (Caqti_type.Field.Coding {rep; encode; _}) ->
          (match encode x with
           | Ok y -> encode_field ~uri rep y
           | Error msg ->
              let msg = Caqti_error.Msg msg in
              let typ = Caqti_type.field field_type in
              Request_utils.raise_encode_rejected ~uri ~typ msg)))

let encode_param ~uri t param =
  let write_value ~uri ft fv acc = encode_field ~uri ft fv :: acc in
  let write_null ~uri:_ _ acc = Pgx.Value.null :: acc in
  try
    Request_utils.encode_param ~uri {write_value; write_null} t param []
      |> List.rev |> Result.ok
  with Caqti_error.Exn (#Caqti_error.call as err) ->
    Error err

let rec decode_field
    : type a. uri: Uri.t -> a Caqti_type.Field.t -> Pgx.Value.t -> a
    = fun ~uri field_type v ->
  let wrap_conv_exn f s =
    (match f s with
     | y -> y
     | exception Pgx.Value.Conversion_failure msg_str ->
        let msg = Caqti_error.Msg msg_str in
        let typ = Caqti_type.field field_type in
        Request_utils.raise_decode_rejected ~uri ~typ msg)
  in
  let wrap_conv_res f s =
    (match f s with
     | Ok y -> y
     | Error msg_str ->
        let msg = Caqti_error.Msg msg_str in
        let typ = Caqti_type.field field_type in
        Request_utils.raise_decode_rejected ~uri ~typ msg)
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
      v |> wrap_conv_exn Pgx.Value.to_string_exn
        |> wrap_conv_res Conv.pdate_of_iso8601
   | Caqti_type.Ptime ->
      v |> wrap_conv_exn Pgx.Value.to_string_exn
        |> wrap_conv_res Conv.ptime_of_rfc3339_utc
   | Caqti_type.Ptime_span ->
      v |> wrap_conv_exn Pgx.Value.to_string_exn
        |> wrap_conv_res ptime_span_of_pgstring
   | _ ->
      (match Caqti_type.Field.coding driver_info field_type with
       | None -> Request_utils.raise_decode_missing ~uri ~field_type ()
       | Some (Caqti_type.Field.Coding {rep; decode; _}) ->
          let y = decode_field ~uri rep v in
          (match decode y with
           | Ok y -> y
           | Error msg ->
              let msg = Caqti_error.Msg msg in
              let typ = Caqti_type.field field_type in
              Request_utils.raise_decode_rejected ~uri ~typ msg)))

let decode_row ~uri row_type =
  let read_value ~uri ft = function
   | [] -> assert false
   | field :: fields ->
      let y = decode_field ~uri ft field in
      (y, fields)
  in
  let rec skip_null n xs =
    if n = 0 then Some xs else
    (match xs with
     | [] -> assert false
     | x :: xs' when Pgx.Value.(compare null) x = 0 -> skip_null (n - 1) xs'
     | _ :: _ -> None)
  in
  let decode = Request_utils.decode_row ~uri {read_value; skip_null} row_type in
  fun fields ->
    try
      let (y, fields) = decode fields in
      assert (fields = []);
      Ok y
    with
     | Caqti_error.Exn (#Caqti_error.retrieve as err) -> Error err

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

module Connect_functor (System : Caqti_platform.System_sig.S) = struct
  open System
  open System.Fiber.Infix

  let ( let*? ) m f = m >>= function Ok x -> f x | Error _ as r -> Fiber.return r
  let ( let+? ) m f = m >|= function Ok x -> Ok (f x) | Error _ as r -> r
  let ( >>=? ) = ( let*? )
  let ( >|=? ) = ( let+? )

  let intercept h f =
    Fiber.catch
      (fun () -> f () >|= fun y -> Ok y)
      (function
       | Pgx.PostgreSQL_Error (msg, err) ->
          Fiber.return (Error (h (Pgx_msg (msg, err))))
       | End_of_file ->
          Fiber.return (Error (h (Caqti_error.Msg "Unexpected EOF from server.")))
       | Failure msg -> (* Raised by our Pgx.Io implementation. *)
          Fiber.return (Error (h (Caqti_error.Msg msg)))
       | exn ->
          raise exn)

  let intercept_request_failed ~uri ~query =
    intercept (Caqti_error.request_failed ~uri ~query)
  let intercept_connect_failed ~uri =
    intercept (Caqti_error.connect_failed ~uri)

  (* We need to pass connect_env into open_connection below.  This means that
   * PGX will be instantiated for each connection. *)
  module Pass_connect_env
    (Connect_env : sig val sw : Switch.t val connect_env : connect_env end) =
  struct
    open Connect_env

    module Pgx_with_io = Pgx.Make (struct
      type 'a t = 'a Fiber.t
      let return = Fiber.return
      let ( >>= ) = ( >>= )
      let catch = Fiber.catch

      include Net

      type sockaddr = Unix of string | Inet of string * int

      let open_connection sockaddr =
        (match sockaddr with
         | Unix path ->
            connect ~sw ~connect_env (Sockaddr.unix path)
         | Inet (host, port) ->
            (match Ipaddr.of_string host with
             | Ok ipaddr ->
                connect ~sw ~connect_env (Sockaddr.tcp (ipaddr, port))
             | Error _ ->
                let host' = host
                  |> Domain_name.of_string_exn
                  |> Domain_name.host_exn
                in
                getaddrinfo ~connect_env host' port >>= (function
                 | Ok [] ->
                    failwith "The host name does not resolve."
                 | Ok (sockaddr :: _) ->
                    connect ~sw ~connect_env sockaddr
                 | Error (`Msg msg) ->
                    failwith msg)))
        >|= function Ok conn -> conn | Error (`Msg msg) -> failwith msg

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

      let protect f ~finally = Fiber.finally f finally

      module Sequencer = struct
        type 'a monad = 'a Fiber.t
        include Sequencer
      end
    end)
  end

  module Make_connection_base
    (Pgx_with_io : Pgx.S with type 'a Io.t = 'a Fiber.t)
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
        query: string;
        row_type: 'b Caqti_type.t;
        prepared: Pgx_with_io.Prepared.s;
        params: Pgx.Value.t list;
      }

      let returned_count _ = Fiber.return (Error `Unsupported)
      let affected_count _ = Fiber.return (Error `Unsupported)

      let reject ~query msg =
        Error (Caqti_error.response_rejected ~uri ~query (Caqti_error.Msg msg))

      let exec {query; prepared; params; _} =
        intercept_request_failed ~uri ~query (fun () ->
          Pgx_with_io.Prepared.execute prepared ~params) >|=
        (function
         | Ok [] -> Ok ()
         | Ok _ ->
            reject ~query "Received multiple rows where none were expected."
         | Error _ as r -> r)

      let find {query; row_type; prepared; params} =
        intercept_request_failed ~uri ~query (fun () ->
          Pgx_with_io.Prepared.execute prepared ~params) >|=
        (function
         | Ok [row] -> decode_row ~uri row_type row
         | Ok [] ->
            reject ~query "Received no rows where one was expected."
         | Ok _ ->
            reject ~query "Received more than one row where one was expected."
         | Error _ as r -> r)

      let find_opt {query; row_type; prepared; params} =
        intercept_request_failed ~uri ~query (fun () ->
          Pgx_with_io.Prepared.execute prepared ~params) >|=
        (function
         | Ok [] -> Ok None
         | Ok [row] ->
            decode_row ~uri row_type row |> Result.map (fun x -> Some x)
         | Ok _ ->
            reject ~query
              "Received two or more rows where at most one was expected."
         | Error _ as r -> r)

      let fold f {query; row_type; prepared; params} =
        let decode = decode_row ~uri row_type in
        let f acc row =
          Fiber.return @@ match acc with
           | Ok acc ->
              (match decode row with
               | Ok row -> Ok (f row acc)
               | Error _ as r -> r)
           | Error _ as r -> r
        in
        fun acc ->
          intercept_request_failed ~uri ~query begin fun () ->
            Pgx_with_io.Prepared.execute_fold ~f prepared ~params ~init:(Ok acc)
          end >|= Stdlib.Result.join

      let fold_s f {query; row_type; prepared; params} =
        let decode = decode_row ~uri row_type in
        let f acc row =
          (match acc with
           | Ok acc ->
              (match decode row with
               | Ok row -> f row acc
               | Error _ as r -> Fiber.return r)
           | Error _ as r -> Fiber.return r)
        in
        fun acc ->
          intercept_request_failed ~uri ~query begin fun () ->
            Pgx_with_io.Prepared.execute_fold ~f prepared ~params ~init:(Ok acc)
          end >|= Stdlib.Result.join

      let iter_s f {query; row_type; prepared; params} =
        let decode = decode_row ~uri row_type in
        let f acc row =
          (match acc with
           | Ok () ->
              (match decode row with
               | Ok row -> f row
               | Error _ as r -> Fiber.return r)
           | Error _ as r -> Fiber.return r)
        in
        intercept_request_failed ~uri ~query begin fun () ->
          Pgx_with_io.Prepared.execute_fold ~f prepared ~params ~init:(Ok ())
        end >|= Stdlib.Result.join

      let to_stream resp () =
        fold List.cons resp [] >|= Result.map List.rev >|= function
         | Ok [] -> Stream.Nil
         | Ok (row :: rows) -> Stream.Cons (row, Stream.of_list rows)
         | Error err -> (Stream.Error err)
    end

    type prepared = {
      query: string;
      pgx_prepared: Pgx_with_io.Prepared.s;
      rev_quotes: Pgx.Value.t list;
    }

    let in_use = ref false
    let prepare_cache : (int, prepared) Hashtbl.t = Hashtbl.create 19

    let reset _ = Fiber.return () (* FIXME *)

    let using_db f =
      if !in_use then
        failwith "Invalid concurrent usage of PostgreSQL connection detected.";
      in_use := true;
      let db = match !db_txn with None -> db_arg | Some db -> db in
      Fiber.cleanup
        (fun () -> f db >|= fun res -> in_use := false; res)
        (fun () -> reset db >|= fun _ -> in_use := false)

    let type_oid_cache = Hashtbl.create 11

    let field_type_oid ft =
      let name = pg_type_name ft in
      (match Hashtbl.find_opt type_oid_cache name with
       | Some oid -> Fiber.return (Ok oid)
       | None ->
          let params = [Pgx.Value.of_string name] in
          let*? row =
            intercept_request_failed ~uri ~query:Q.select_type_oid (fun () ->
              Pgx_with_io.Prepared.execute select_type_oid ~params)
          in
          let fail s =
            let msg = Caqti_error.Msg s in
            Fiber.return (Error
              (Caqti_error.request_failed ~uri ~query:Q.select_type_oid msg))
          in
          let failf fmt = Format.kasprintf fail fmt in
          (match row with
           | [[v]] ->
              (match Pgx.Value.to_int32 v with
               | Some oid ->
                  Hashtbl.add type_oid_cache name oid;
                  Fiber.return (Ok oid)
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
          (Pgx.oid list, _) result Fiber.t =
        (function
         | Unit -> fun acc -> Fiber.return (Ok acc)
         | Field ft -> fun acc -> field_type_oid ft >|=? fun ft -> ft :: acc
         | Option t -> loop t
         | Product (_, prod) ->
            let rec loop_prod : type i. (a, i) Caqti_type.product -> _ =
              (function
               | Proj_end -> fun acc -> Fiber.return (Ok acc)
               | Proj (t, _, prod) ->
                  let loop_t = loop t in
                  let loop_prod = loop_prod prod in
                  fun acc -> loop_prod acc >>=? loop_t)
            in
            loop_prod prod
         | Annot (_, t) -> loop t)
      in
      loop (Caqti_type.option param_type) []

    let pp_request_with_param ppf =
      Caqti_request.make_pp_with_param ~env ~driver_info () ppf

    let call ~f req param =
      Log.debug ~src:Logging.request_log_src (fun f ->
        f "Sending %a" pp_request_with_param (req, param)) >>= fun () ->
      using_db @@ fun db ->
      let pre_prepare () =
        let templ = Caqti_request.query req driver_info in
        let query, rev_quotes = query_string ~env:env' templ in
        let*? param_types = type_oids (Caqti_request.param_type req) in
        let+? string_oid = field_type_oid Caqti_type.String in
        let quote_types = List.rev_map (fun _ -> string_oid) rev_quotes in
        let types = List.rev_append quote_types param_types in
        (query, types, rev_quotes)
      in
      let post_prepare pq =
        (match encode_param ~uri (Caqti_request.param_type req) param with
         | Error _ as r -> Fiber.return r
         | Ok regular_params ->
            let params = List.rev_append pq.rev_quotes regular_params in
            f {
              Response.row_type = Caqti_request.row_type req;
              query = pq.query;
              prepared = pq.pgx_prepared;
              params;
            })
      in
      (match Caqti_request.query_id req with
       | Some query_id ->
          (match Hashtbl.find_opt prepare_cache query_id with
           | Some pq -> Fiber.return (Ok pq)
           | None ->
              let*? query, types, rev_quotes = pre_prepare () in
              let name = sprintf "q%d" query_id in
              let+? pgx_prepared =
                intercept_request_failed ~uri ~query (fun () ->
                  Pgx_with_io.Prepared.prepare ~name ~query ~types db)
              in
              let pq = {query; pgx_prepared; rev_quotes} in
              Hashtbl.add prepare_cache query_id pq; pq)
          >>=? post_prepare
       | None ->
          let*? query, types, rev_quotes = pre_prepare () in
          Pgx_with_io.Prepared.with_prepare db ~types ~query
            ~f:(fun pgx_prepared ->
                  post_prepare {query; pgx_prepared; rev_quotes}))

    let deallocate req =
      (match Caqti_request.query_id req with
       | Some query_id ->
          (match Hashtbl.find_opt prepare_cache query_id with
           | None ->
              Fiber.return (Ok ())
           | Some prepared ->
              Hashtbl.remove prepare_cache query_id;
              intercept_request_failed ~uri ~query:"DEALLOCATE"
                (fun () -> Pgx_with_io.Prepared.close prepared.pgx_prepared))
       | None ->
          failwith "deallocate called on oneshot request")

    let disconnect () =
      using_db @@ fun _ ->
      Fiber.catch
        (fun () ->
          (match !db_txn with
           | None -> Fiber.return ()
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
    with type 'a fiber := 'a Fiber.t
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t

  let setup (module Connection : CONNECTION) =
    Connection.exec Q.set_timezone_to_utc () >|= function
     | Ok () -> Ok ()
     | Error err -> Error (`Post_connect err)

  let connect ~sw ~connect_env ?(env = no_env) ~tweaks_version:_ uri =
    let*? {host; port; user; password; database; unix_domain_socket_dir} =
      Fiber.return (parse_uri uri)
    in
    let open
      Pass_connect_env (struct let sw = sw let connect_env = connect_env end)
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
    let module B = Make_connection_base (Pgx_with_io) (struct
      let env = env
      let uri = uri
      let db_arg = db
      let select_type_oid = select_type_oid
    end) in
    let module Connection = struct
      let driver_info = driver_info
      let driver_connection = None
      include B
      include Connection_utils.Make_convenience (System) (B)
      include Connection_utils.Make_populate (System) (B)
    end in
    let+? () = setup (module Connection) in
    (module Connection : CONNECTION)
end

let () =
  Caqti_platform.Driver_loader.register "pgx" (module Connect_functor)
