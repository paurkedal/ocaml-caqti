(* Copyright (C) 2021--2025  Petter A. Urkedal <paurkedal@gmail.com>
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
open Postgresql_conv
open Printf

let rec find_map_list f = function
 | [] -> None
 | x :: xs -> (match f x with | None -> find_map_list f xs | Some _ as y -> y)

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

exception Failed_with_msg of Caqti_error.msg

let host_of_string str =
  (match Domain_name.of_string str with
   | Ok dom ->
      (match Domain_name.host dom with
       | Ok dom -> Some dom
       | Error _ -> None)
   | Error _ -> None)

let pg_type_name : type a. a Caqti_type.Field.t -> string = function
 | Bool -> "bool"
 | Int -> "int8"
 | Int16 -> "int2"
 | Int32 -> "int4"
 | Int64 -> "int8"
 | Float -> "float8"
 | String -> "text"
 | Octets -> "bytea"
 | Pdate -> "date"
 | Ptime -> "timestamptz"
 | Ptime_span -> "interval"
 | Enum name -> name

let encode_field
    : type a. a Caqti_type.Field.t -> a -> Pgx.Value.t
    = fun field_type x ->
  (match field_type with
   | Bool -> Pgx.Value.of_bool x
   | Int -> Pgx.Value.of_int x
   | Int16 -> Pgx.Value.of_int x
   | Int32 -> Pgx.Value.of_int32 x
   | Int64 -> Pgx.Value.of_int64 x
   | Float -> Pgx.Value.of_float x
   | String -> Pgx.Value.of_string x
   | Enum _ -> Pgx.Value.of_string x
   | Octets -> Pgx.Value.of_binary x
   | Pdate -> Pgx.Value.of_string (Conv.iso8601_of_pdate x)
   | Ptime -> Pgx.Value.of_string (pgstring_of_ptime x)
   | Ptime_span -> Pgx.Value.of_string (pgstring_of_ptime_span x))

let query_string ~subst templ =
  let open Caqti_template in
  let templ = Query.expand ~final:true subst templ in

  let rec extract_quotes : Query.t -> _ = function
   | V (ft, v) -> fun (n, acc) -> (n + 1, encode_field ft v :: acc)
   | Q s -> fun (n, acc) -> (n + 1, Pgx.Value.of_string s :: acc)
   | L _ | P _ -> Fun.id
   | E _ -> fun _ -> assert false
   | S qs -> List_ext.fold extract_quotes qs
  in
  let nQ, rev_quotes = extract_quotes templ (0, []) in

  let buf = Buffer.create 64 in
  let rec write_query_string : Query.t -> _ = function
   | L s -> fun jQ -> Buffer.add_string buf s; jQ
   | V _ -> fun jQ -> bprintf buf "$%d" jQ; jQ + 1
   | Q _ -> fun jQ -> bprintf buf "$%d" jQ; jQ + 1
   | P j -> fun jQ -> bprintf buf "$%d" (nQ + 1 + j); jQ
   | E _ -> assert false
   | S qs -> List_ext.fold write_query_string qs
  in
  let _jQ = write_query_string templ 1 in
  (Buffer.contents buf, rev_quotes)

let encode_param ~uri t param =
  let write_value ~uri:_ ft fv acc = encode_field ft fv :: acc in
  let write_null ~uri:_ _ acc = Pgx.Value.null :: acc in
  try
    Request_utils.encode_param ~uri {write_value; write_null} t param []
      |> List.rev |> Result.ok
  with Caqti_error.Exn (#Caqti_error.call as err) ->
    Error err

let decode_field
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
   | Bool -> wrap_conv_exn Pgx.Value.to_bool_exn v
   | Int -> wrap_conv_exn Pgx.Value.to_int_exn v
   | Int16 -> wrap_conv_exn Pgx.Value.to_int_exn v
   | Int32 -> wrap_conv_exn Pgx.Value.to_int32_exn v
   | Int64 -> wrap_conv_exn Pgx.Value.to_int64_exn v
   | Float -> wrap_conv_exn Pgx.Value.to_float_exn v
   | String -> wrap_conv_exn Pgx.Value.to_string_exn v
   | Enum _ -> wrap_conv_exn Pgx.Value.to_string_exn v
   | Octets -> wrap_conv_exn Pgx.Value.to_binary_exn v
   | Pdate ->
      v |> wrap_conv_exn Pgx.Value.to_string_exn
        |> wrap_conv_res Conv.pdate_of_iso8601
   | Ptime ->
      v |> wrap_conv_exn Pgx.Value.to_string_exn
        |> wrap_conv_res Conv.ptime_of_rfc3339_utc
   | Ptime_span ->
      v |> wrap_conv_exn Pgx.Value.to_string_exn
        |> wrap_conv_res ptime_span_of_pgstring)

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
  let select_type_oid = "SELECT oid FROM pg_catalog.pg_type WHERE typname = $1"
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

let dialect = Caqti_template.Dialect.create_pgsql
  ~server_version:(Caqti_template.Version.of_string_unsafe "")
  ~client_library:`pgx
  ()

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
       | Failed_with_msg msg ->
          Fiber.return (Error (h msg))
       | Pgx.PostgreSQL_Error (msg, err) ->
          Fiber.return (Error (h (Pgx_msg (msg, err))))
       | End_of_file ->
          Fiber.return (Error (h (Caqti_error.Msg "Unexpected EOF from server.")))
       | Failure msg -> (* Raised by our Pgx.Io implementation. *)
          Fiber.return (Error (h (Caqti_error.Msg msg)))
       | exn ->
          (match Net.convert_io_exception exn with
           | Some msg -> Fiber.return (Error (h msg))
           | None -> raise exn))

  let intercept_request_failed ~uri ~query =
    intercept (Caqti_error.request_failed ~uri ~query)
  let intercept_connect_failed ~uri =
    intercept (Caqti_error.connect_failed ~uri)

  type ssl_config =
    Ssl_config : {
      impl: (module Net.TLS_PROVIDER with type tls_config = 'a);
      config: 'a;
      host: [`host] Domain_name.t option;
    } -> ssl_config

  (* We need to pass stdenv into open_connection below.  This means that
   * PGX will be instantiated for each connection. *)
  module Pass_stdenv
    (Connect_env : sig val sw : Switch.t val stdenv : stdenv end) =
  struct
    open Connect_env

    module Pgx_with_io = Pgx.Make (struct
      type 'a t = 'a Fiber.t
      let return = Fiber.return
      let ( >>= ) = ( >>= )
      let catch = Fiber.catch

      include Net

      type in_channel = Socket.t
      type out_channel = Socket.t

      type sockaddr = Unix of string | Inet of string * int

      let open_connection sockaddr =
        let connect sockaddr =
          Net.connect_tcp ~sw ~stdenv sockaddr
            >|= Result.map (fun socket -> (socket, socket))
        in
        (match sockaddr with
         | Unix path ->
            connect (Sockaddr.unix path)
         | Inet (host_or_ipaddr, port) ->
            (match Ipaddr.of_string host_or_ipaddr with
             | Ok ipaddr ->
                connect (Sockaddr.tcp (ipaddr, port))
             | Error _ ->
                (match host_of_string host_or_ipaddr with
                 | None ->
                    failwith
                      ("Cannot resolve invalid host name " ^ host_or_ipaddr)
                 | Some host ->
                    getaddrinfo ~stdenv host port >>= (function
                     | Ok [] ->
                        failwith "The host name does not resolve."
                     | Ok (sockaddr :: _) ->
                        connect sockaddr
                     | Error (`Msg msg) ->
                        failwith msg))))
        >|= function Ok conn -> conn | Error msg -> raise (Failed_with_msg msg)

      let output_char = Socket.output_char
      let output_string = Socket.output_string
      let flush = Socket.flush

      let output_binary_int oc x =
        let buf = Bytes.create 4 in
        Bytes.set_int32_be buf 0 (Int32.of_int x);
        Socket.output_string oc (Bytes.to_string buf)

      let input_char = Socket.input_char
      let really_input = Socket.really_input

      (* This closes the output channel instead of the input channel; cf.
       * Unix.open_connection. *)
      let close_in = Socket.close

      let input_binary_int ic =
        let buf = Bytes.create 4 in
        Socket.really_input ic buf 0 4 >|= fun () ->
        Int32.to_int (Bytes.get_int32_be buf 0)

      type nonrec ssl_config = ssl_config

      let upgrade_ssl =
        let upgrade ?ssl_config socket _ =
          (match ssl_config, tcp_flow_of_socket socket with
           | None, _ -> assert false (* we don't use `Auto *)
           | _, None -> assert false (* we only upgrade once *)
           | Some (Ssl_config {impl; config; host}), Some tcp_flow ->
              let module Impl = (val impl) in
              Impl.start_tls ~config ?host tcp_flow >|= function
               | Ok tls_flow ->
                  let socket = socket_of_tls_flow ~sw tls_flow in
                  (socket, socket)
               | Error msg -> raise (Failed_with_msg msg))
        in
        `Supported upgrade

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
    (Pgx_with_io : Pgx.S with type 'a Io.t = 'a Fiber.t
                          and type Io.ssl_config = ssl_config)
    (Connection_arg : sig
      val subst : Caqti_template.Query.subst
      val uri : Uri.t
      val db_arg : Pgx_with_io.t
      val select_type_oid : Pgx_with_io.Prepared.s
      val dynamic_capacity : int
    end) =
  struct
    open Connection_arg

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

    module Pcache =
      Request_cache.Make (struct type t = prepared let weight _ = 1 end)

    let in_use = ref false
    let pcache : Pcache.t = Pcache.create ~dynamic_capacity dialect

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
         | Field ft -> fun acc -> field_type_oid ft >|=? fun ft -> ft :: acc
         | Option t -> loop t
         | Product (_, _, prod) ->
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
      Caqti_template.Request.make_pp_with_param ~subst ~dialect () ppf

    let free_prepared prepared =
      intercept_request_failed ~uri ~query:"DEALLOCATE"
        (fun () -> Pgx_with_io.Prepared.close prepared.pgx_prepared)

    let deallocate req =
      (match Caqti_template.Request.prepare_policy req with
       | Dynamic | Static ->
          (match Pcache.deallocate pcache req with
           | None ->
              Fiber.return (Ok ())
           | Some (prepared, commit) ->
              free_prepared prepared >|=? commit)
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

    let fresh_name = Request_utils.fresh_name_generator "caq"

    let call ~f req param =
      using_db @@ fun db ->
      deallocate_some () >>=? fun () ->
      Log.debug ~src:Logging.request_log_src (fun f ->
        f "Sending %a" pp_request_with_param (req, param)) >>= fun () ->
      let pre_prepare () =
        let templ = Caqti_template.Request.query req dialect in
        let query, rev_quotes = query_string ~subst templ in
        let*? param_types = type_oids (Caqti_request.param_type req) in
        let+? string_oid = field_type_oid Caqti_type.Field.String in
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
      (match Caqti_template.Request.prepare_policy req with
       | Dynamic | Static ->
          (match Pcache.find_and_promote pcache req with
           | Some pq -> Fiber.return (Ok pq)
           | None ->
              let*? query, types, rev_quotes = pre_prepare () in
              let name = fresh_name () in
              let+? pgx_prepared =
                intercept_request_failed ~uri ~query (fun () ->
                  Pgx_with_io.Prepared.prepare ~name ~query ~types db)
              in
              let pq = {query; pgx_prepared; rev_quotes} in
              Pcache.add pcache req pq;
              pq)
          >>=? post_prepare
       | Direct ->
          let*? query, types, rev_quotes = pre_prepare () in
          Pgx_with_io.Prepared.with_prepare db ~types ~query
            ~f:(fun pgx_prepared ->
                  post_prepare {query; pgx_prepared; rev_quotes}))

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

  let driver_info = Caqti_driver_info.of_dialect dialect

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a fiber := 'a Fiber.t
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t

  let find_tls_provider ~config ?host () =
    let with_config (module Tls_provider : Net.TLS_PROVIDER) =
      (match Caqti_connect_config.get Tls_provider.tls_config_key config with
       | None -> None
       | Some config ->
          Some (Ssl_config {impl = (module Tls_provider); config; host}))
    in
    (match find_map_list with_config (Net.tls_providers config) with
     | None -> `No
     | Some ssl_config -> `Always ssl_config)

  let connect ~sw ~stdenv ~subst ~config uri =

    (* Create PGX connection and helper functions. *)
    let*? {host; port; user; password; database; unix_domain_socket_dir} =
      Fiber.return (parse_uri uri)
    in
    let ssl =
      let host = Option.bind host host_of_string in
      find_tls_provider ~config ?host ()
    in
    let open Pass_stdenv (struct let sw = sw let stdenv = stdenv end) in
    let*? db =
      intercept_connect_failed ~uri
        (Pgx_with_io.connect
          ~ssl ?host ?port ?user ?password ?database ?unix_domain_socket_dir)
    in
    let prepare_post_connect query =
      intercept_request_failed ~uri ~query
        (fun () -> Pgx_with_io.Prepared.prepare db ~query)
      >|= Result.map_error (fun err -> `Post_connect err)
    in
    let execute_post_connect ?params query =
      intercept_request_failed ~uri ~query
        (fun () -> Pgx_with_io.execute ?params db query)
    in

    (* Run setup. *)
    let*? () =
      let query = "SET TimeZone TO 'UTC'" in
      let reject msg =
        let msg = Caqti_error.Msg msg in
        let err = Caqti_error.response_rejected ~uri ~query msg in
        Error (`Post_connect err)
      in
      execute_post_connect query >|= function
       | Ok [] -> Ok ()
       | Ok _ -> reject "Invalid response from setup request."
       | Error err -> Error (`Post_connect err)
    in

    (* Return Caqti connection module. *)
    let+? select_type_oid = prepare_post_connect Q.select_type_oid in
    let module B = Make_connection_base (Pgx_with_io) (struct
      let subst = subst dialect
      let uri = uri
      let db_arg = db
      let select_type_oid = select_type_oid
      let dynamic_capacity =
        Caqti_connect_config.(get dynamic_prepare_capacity) config
    end) in
    let module Connection = struct
      let driver_info = driver_info
      let dialect = dialect
      let driver_connection = None
      include B
      include Connection_utils.Make_convenience (System) (B)
      include Connection_utils.Make_populate (System) (B)
    end in
    (module Connection : CONNECTION)
end

let () =
  Caqti_platform.Driver_loader.register "pgx" (module Connect_functor)
