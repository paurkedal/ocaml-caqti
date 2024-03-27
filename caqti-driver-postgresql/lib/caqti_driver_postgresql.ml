(* Copyright (C) 2017--2024  Petter A. Urkedal <paurkedal@gmail.com>
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
module Pg = Postgresql

let ( |>? ) = Result.bind
let ( %>? ) f g x = match f x with Ok y -> g y | Error _ as r -> r

let pct_encoder =
  Uri.pct_encoder ~query_value:(`Custom (`Query_value, "", "=")) ()

module Int_hashable = struct
  type t = int
  let equal (i : int) (j : int) = i = j
  let hash (i : int) = Hashtbl.hash i
end

module Int_hashtbl = Hashtbl.Make (Int_hashable)

module Q = struct
  open Caqti_template.Std

  let start = T.(unit ->. unit) "BEGIN"
  let commit = T.(unit ->. unit) "COMMIT"
  let rollback = T.(unit ->. unit) "ROLLBACK"

  let type_oid = T.(string ->? int)
    "SELECT oid FROM pg_catalog.pg_type WHERE typname = ?"

  let set_timezone_to_utc = T.(unit ->. unit) ~oneshot:true
    "SET TimeZone TO 'UTC'"

  let set_statement_timeout t =
    T.(unit -->. unit) ~oneshot:true @@ fun _ ->
    (* Parameters are not supported for SET. *)
    S[L"SET statement_timeout TO "; L(string_of_int t)]
end

type Caqti_error.msg +=
  | Connect_error_msg of {
      error: Pg.error;
    }
  | Connection_error_msg of {
      error: Pg.error;
      connection_status: Pg.connection_status;
    }
  | Result_error_msg of {
      error_message: string;
      sqlstate: string;
    }

let extract_connect_error error = Connect_error_msg {error}

let extract_communication_error connection error =
  Connection_error_msg {
    error;
    connection_status = connection#status;
  }

let extract_result_error result =
  Result_error_msg {
    error_message = result#error;
    sqlstate = result#error_field Pg.Error_field.SQLSTATE;
  }

let () =
  let pp ppf = function
   | Connect_error_msg {error; _} | Connection_error_msg {error; _} ->
      Format.pp_print_string ppf (Pg.string_of_error error)
   | Result_error_msg {error_message; _} ->
      Format.pp_print_string ppf error_message
   | _ ->
      assert false
  in
  let cause = function
   | Result_error_msg {sqlstate; _} ->
      Postgresql_conv.cause_of_sqlstate sqlstate
   | _ ->
      assert false
  in
  Caqti_error.define_msg ~pp [%extension_constructor Connect_error_msg];
  Caqti_error.define_msg ~pp [%extension_constructor Connection_error_msg];
  Caqti_error.define_msg ~pp ~cause [%extension_constructor Result_error_msg]

let driver_info =
  let dummy_dialect = Caqti_template.Dialect.Pgsql {
    server_version = Caqti_template.Version.of_string_unsafe "";
    ocaml_library = `postgresql;
    reserved = ();
  } in
  Caqti_driver_info.of_dialect dummy_dialect

module Pg_ext = struct
  include Postgresql_conv

  (* Turns a constant into [(must_quote, encode)].  Note the slight difference
   * from encoded parameters like booleans and quoting. *)
  let query_string_of_value
    : type a. Pg.connection -> a Caqti_type.Field.t -> bool * (a -> string) =
    fun db ->
    let escape_string s = db#escape_string s in
    (function
     | Bool -> (false, string_of_bool)
     | Int -> (false, string_of_int)
     | Int16 -> (false, string_of_int)
     | Int32 -> (false, Int32.to_string)
     | Int64 -> (false, Int64.to_string)
     | Float -> (false, Float.to_string)
     | String -> (true, escape_string)
     | Octets -> (true, escape_string)
     | Pdate -> (true, Conv.iso8601_of_pdate)
     | Ptime -> (true, pgstring_of_ptime)
     | Ptime_span -> (true, pgstring_of_ptime_span)
     | Enum _ -> (true, escape_string))

  let query_string ~subst (db : Pg.connection) templ =
    let open Caqti_template in
    let buf = Buffer.create 64 in
    let rec loop = function
     | Query.L s -> Buffer.add_string buf s
     | Query.Q s ->
        Buffer.add_char buf '\'';
        Buffer.add_string buf (db#escape_string s);
        Buffer.add_char buf '\''
     | Query.V (ft, v) ->
        let quote, conv = query_string_of_value db ft in
        if quote then Buffer.add_char buf '\'';
        Buffer.add_string buf (conv v);
        if quote then Buffer.add_char buf '\''
     | Query.P i -> bprintf buf "$%d" (i + 1)
     | Query.E _ -> assert false
     | Query.S frags -> List.iter loop frags
    in
    loop (Query.expand ~final:true subst templ);
    Buffer.contents buf

  let escaped_connvalue s =
    let buf = Buffer.create (String.length s) in
    let aux = function
     | '\\' -> Buffer.add_string buf {|\\|}
     | '\'' -> Buffer.add_string buf {|\'|}
     | ch -> Buffer.add_char buf ch in
    String.iter aux s;
    Buffer.contents buf

  let pop_uri_param present absent param uri =
    (match Uri.get_query_param uri param with
     | None ->
        Ok (absent, uri)
     | Some value_str ->
        (match present value_str with
         | value -> Ok (value, Uri.remove_query_param uri param)
         | exception Failure msg ->
            let msg = Caqti_error.Msg msg in
            Error (Caqti_error.connect_rejected ~uri msg)))

  let parse_notice_processing = function
   | "quiet" -> `Quiet
   | "stderr" -> `Stderr
   | _ -> failwith "Invalid argument for notice_processing."

  let parse_uri uri =
    pop_uri_param parse_notice_processing `Quiet "notice_processing" uri
      |>? fun (notice_processing, uri) ->
    pop_uri_param bool_of_string false "use_single_row_mode" uri
      |>? fun (use_single_row_mode, uri) ->
    let conninfo =
      if Uri.host uri <> None then Uri.to_string ~pct_encoder uri else
      let mkparam k v = k ^ " = '" ^ escaped_connvalue v ^ "'" in
      let mkparams (k, vs) = List.map (mkparam k) vs in
      String.concat " " (List.flatten (List.map mkparams (Uri.query uri)))
    in
    Ok (conninfo, notice_processing, use_single_row_mode)
end

let bool_oid = Pg.oid_of_ftype Pg.BOOL
let int2_oid = Pg.oid_of_ftype Pg.INT2
let int4_oid = Pg.oid_of_ftype Pg.INT4
let int8_oid = Pg.oid_of_ftype Pg.INT8
let float8_oid = Pg.oid_of_ftype Pg.FLOAT8
let bytea_oid = Pg.oid_of_ftype Pg.BYTEA
let date_oid = Pg.oid_of_ftype Pg.DATE
let timestamp_oid = Pg.oid_of_ftype Pg.TIMESTAMPTZ
let interval_oid = Pg.oid_of_ftype Pg.INTERVAL
let unknown_oid = Pg.oid_of_ftype Pg.UNKNOWN

let init_param_types ~type_oid_cache =
  let oid_of_field_type : type a. a Caqti_type.Field.t -> _ = function
   | Bool -> Ok bool_oid
   | Int -> Ok int8_oid
   | Int16 -> Ok int2_oid
   | Int32 -> Ok int4_oid
   | Int64 -> Ok int8_oid
   | Float -> Ok float8_oid
   | String -> Ok unknown_oid
   | Octets -> Ok bytea_oid
   | Pdate -> Ok date_oid
   | Ptime -> Ok timestamp_oid
   | Ptime_span -> Ok interval_oid
   | Enum name -> Ok (Hashtbl.find type_oid_cache name)
  in
  let rec recurse : type a. _ -> _ -> a Caqti_type.t -> _ -> _
      = fun pt bp -> function
   | Caqti_type.Field ft -> fun i ->
      oid_of_field_type ft |>? fun oid ->
      pt.(i) <- oid;
      bp.(i) <- oid = bytea_oid;
      Ok (i + 1)
   | Caqti_type.Option t ->
      recurse pt bp t
   | Caqti_type.Product (_, _, prod) ->
      let rec loop : type i. (a, i) Caqti_type.product -> _ = function
       | Proj_end -> Result.ok
       | Proj (t, _, prod) -> recurse pt bp t %>? loop prod
      in
      loop prod
   | Caqti_type.Annot (_, t0) ->
      recurse pt bp t0
  in
  fun pt bp t ->
    recurse pt bp t 0 |>? fun np ->
    assert (np = Array.length pt);
    assert (np = Array.length bp);
    Ok ()

module type STRING_ENCODER = sig
  val encode_string : string -> string
  val encode_octets : string -> string
end

module Make_encoder (String_encoder : STRING_ENCODER) = struct
  open String_encoder

  let encode_field : type a. a Caqti_type.Field.t -> a -> string =
    fun field_type x ->
    (match field_type with
     | Bool -> Pg_ext.pgstring_of_bool x
     | Int -> string_of_int x
     | Int16 -> string_of_int x
     | Int32 -> Int32.to_string x
     | Int64 -> Int64.to_string x
     | Float -> sprintf "%.17g" x
     | String -> encode_string x
     | Enum _ -> encode_string x
     | Octets -> encode_octets x
     | Pdate -> Conv.iso8601_of_pdate x
     | Ptime -> Pg_ext.pgstring_of_ptime x
     | Ptime_span -> Pg_ext.pgstring_of_ptime_span x)

  let encode ~uri params t x =
    let write_value ~uri:_ ft fv i =
      let s = encode_field ft fv in
      params.(i) <- s; i + 1
    in
    let write_null ~uri:_ _ i = i + 1 in
    try
      let n = Request_utils.encode_param ~uri {write_value; write_null} t x 0 in
      assert (n = Array.length params);
      Ok ()
    with Caqti_error.Exn (#Caqti_error.call as err) -> Error err
end

module Param_encoder = Make_encoder (struct
  let encode_string s = s
  let encode_octets s = s
end)

let decode_field
    : type a. uri: Uri.t -> a Caqti_type.Field.t -> string -> a =
  fun ~uri field_type s ->
  let wrap_conv_exn f s =
    (try (f s) with
     | _ ->
        let msg = Caqti_error.Msg (sprintf "Invalid value %S." s) in
        let typ = Caqti_type.field field_type in
        Request_utils.raise_decode_rejected ~uri ~typ msg)
  in
  let wrap_conv_res f s =
    (match f s with
     | Ok y -> y
     | Error msg ->
        let msg = Caqti_error.Msg msg in
        let typ = Caqti_type.field field_type in
        Request_utils.raise_decode_rejected ~uri ~typ msg)
  in
  (match field_type with
   | Bool -> wrap_conv_exn Pg_ext.bool_of_pgstring s
   | Int -> wrap_conv_exn int_of_string s
   | Int16 -> wrap_conv_exn int_of_string s
   | Int32 -> wrap_conv_exn Int32.of_string s
   | Int64 -> wrap_conv_exn Int64.of_string s
   | Float -> wrap_conv_exn float_of_string s
   | String -> s
   | Enum _ -> s
   | Octets -> Postgresql.unescape_bytea s
   | Pdate -> wrap_conv_res Conv.pdate_of_iso8601 s
   | Ptime -> wrap_conv_res Conv.ptime_of_rfc3339_utc s
   | Ptime_span -> wrap_conv_res Pg_ext.ptime_span_of_pgstring s)

let decode_row ~uri row_type =
  let read_value ~uri ft (resp, i, j) =
    let y = decode_field ~uri ft (resp#getvalue i j) in
    (y, (resp, i, j + 1))
  in
  let skip_null n (resp, i, j) =
    let j' = j + n in
    let rec check k = k = j' || resp#getisnull i k && check (k + 1) in
    if check j then Some (resp, i, j') else None
  in
  let decode = Request_utils.decode_row ~uri {read_value; skip_null} row_type in
  fun (resp, i) ->
    (match decode (resp, i, 0) with
     | (y, (_, _, j)) -> assert (j = Caqti_type.length row_type); Ok y
     | exception Caqti_error.Exn (`Decode_rejected _ as err) -> Error err)

type prepared = {
  query: string;
  param_length: int;
  param_types: Pg.oid array;
  binary_params: bool array;
  single_row_mode: bool;
}

module Connect_functor
  (System : Caqti_platform.System_sig.S)
  (System_unix : Caqti_platform_unix.System_sig.S
    with type 'a fiber := 'a System.Fiber.t
     and type stdenv := System.stdenv) =
struct
  open System
  open System.Fiber.Infix
  open System_unix
  module H = Connection_utils.Make_helpers (System)

  let (>>=?) m f = m >>= function Ok x -> f x | Error _ as r -> Fiber.return r
  let (>|=?) m f = m >|= function Ok x -> f x | Error _ as r -> r

  let driver_info = driver_info

  module Pg_io = struct

    let communicate ~stdenv db step =
      let aux fd =
        let rec loop = function
         | Pg.Polling_reading ->
            Unix.poll ~stdenv ~read:true fd >>= fun _ ->
            (match step () with
             | exception Pg.Error msg -> Fiber.return (Error msg)
             | ps -> loop ps)
         | Pg.Polling_writing ->
            Unix.poll ~stdenv ~write:true fd >>= fun _ ->
            (match step () with
             | exception Pg.Error msg -> Fiber.return (Error msg)
             | ps -> loop ps)
         | Pg.Polling_failed | Pg.Polling_ok ->
            Fiber.return (Ok ())
        in
        loop Pg.Polling_writing
      in
      (match db#socket with
       | exception Pg.Error msg -> Fiber.return (Error msg)
       | socket -> Unix.wrap_fd aux (Obj.magic socket))

    let get_next_result ~stdenv ~uri ~query db =
      let rec retry fd =
        db#consume_input;
        if db#is_busy then
          Unix.poll ~stdenv ~read:true fd >>= (fun _ -> retry fd)
        else
          Fiber.return (Ok db#get_result)
      in
      try Unix.wrap_fd retry (Obj.magic db#socket)
      with Pg.Error err ->
        let msg = extract_communication_error db err in
        Fiber.return (Error (Caqti_error.request_failed ~uri ~query msg))

    let get_one_result ~stdenv ~uri ~query db =
      get_next_result ~stdenv ~uri ~query db >>=? function
       | None ->
          let msg = Caqti_error.Msg "No response received after send." in
          Fiber.return (Error (Caqti_error.request_failed ~uri ~query msg))
       | Some result ->
          Fiber.return (Ok result)

    let get_final_result ~stdenv ~uri ~query db =
      get_one_result ~stdenv ~uri ~query db >>=? fun result ->
      get_next_result ~stdenv ~uri ~query db >>=? function
       | None ->
          Fiber.return (Ok result)
       | Some _ ->
          let msg = Caqti_error.Msg "More than one response received." in
          Fiber.return (Error (Caqti_error.response_rejected ~uri ~query msg))

    let check_query_result ~uri ~query ~row_mult ~single_row_mode result =
      let reject msg =
        let msg = Caqti_error.Msg msg in
        Error (Caqti_error.response_rejected ~uri ~query msg)
      in
      let fail msg =
        let msg = Caqti_error.Msg msg in
        Error (Caqti_error.request_failed ~uri ~query msg)
      in
      (match result#status with
       | Pg.Command_ok ->
          (match Caqti_mult.expose row_mult with
           | `Zero -> Ok ()
           | (`One | `Zero_or_one | `Zero_or_more) ->
              reject "Tuples expected for this query.")
       | Pg.Tuples_ok ->
          if single_row_mode then
            if result#ntuples = 0 then Ok () else
            reject "Tuples returned in single-row-mode."
          else
          (match Caqti_mult.expose row_mult with
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
       | Pg.Bad_response ->
          let msg = extract_result_error result in
          Error (Caqti_error.response_rejected ~uri ~query msg)
       | Pg.Fatal_error ->
          let msg = extract_result_error result in
          Error (Caqti_error.request_failed ~uri ~query msg)
       | Pg.Nonfatal_error -> Ok () (* TODO: Log *)
       | Pg.Copy_out | Pg.Copy_in | Pg.Copy_both ->
          reject "Received unexpected copy response."
       | Pg.Single_tuple ->
          if not single_row_mode then
            reject "Received unexpected single tuple response." else
          if result#ntuples <> 1 then
            reject "Expected a single row in single-row mode." else
          Ok ())

    let check_command_result ~uri ~query result =
      check_query_result
        ~uri ~query ~row_mult:Caqti_mult.zero ~single_row_mode:false result
  end

  (* Driver Interface *)

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a fiber := 'a Fiber.t
     and type ('a, 'err) stream := ('a, 'err) Stream.t

  module Make_connection_base
    (Connection_arg : sig
      val stdenv : stdenv
      val dialect : Caqti_template.Dialect.t
      val subst : Caqti_template.Query.subst
      val uri : Uri.t
      val db : Pg.connection
      val use_single_row_mode : bool
    end) =
  struct
    open Connection_arg

    module Copy_encoder = Make_encoder (struct

      let encode_string s =
        let buf = Buffer.create (String.length s) in
        for i = 0 to String.length s - 1 do
          (match s.[i] with
           | '\\' -> Buffer.add_string buf "\\\\"
           | '\n' -> Buffer.add_string buf "\\n"
           | '\r' -> Buffer.add_string buf "\\r"
           | '\t' -> Buffer.add_string buf "\\t"
           | c -> Buffer.add_char buf c)
        done;
        Buffer.contents buf

      let encode_octets s = encode_string (db#escape_bytea s)
    end)

    let in_use = ref false
    let in_transaction = ref false
    let prepare_cache : prepared Int_hashtbl.t = Int_hashtbl.create 19

    let query_name_of_id = sprintf "_caq%d"

    let wrap_pg ~query f =
      try Ok (f ()) with
       | Postgresql.Error err ->
          let msg = extract_communication_error db err in
          Error (Caqti_error.request_failed ~uri ~query msg)

    let reset () =
      Log.warn (fun p ->
        p "Lost connection to <%a>, reconnecting." Caqti_error.pp_uri uri)
        >>= fun () ->
      in_transaction := false;
      (match db#reset_start with
       | exception Pg.Error _ -> Fiber.return false
       | true ->
          Int_hashtbl.clear prepare_cache;
          Pg_io.communicate ~stdenv db (fun () -> db#reset_poll) >|=
          (function
           | Error _ -> false
           | Ok () -> (try db#status = Pg.Ok with Pg.Error _ -> false))
       | false ->
          Fiber.return false)

    let rec retry_on_connection_error ?(n = 1) f =
      if !in_transaction then f () else
      (f () : (_, [> Caqti_error.call]) result Fiber.t) >>=
      (function
       | Ok _ as r -> Fiber.return r
       | Error (`Request_failed
            {Caqti_error.msg = Connection_error_msg
              {error = Postgresql.Connection_failure _; _}; _})
            as r when n > 0 ->
          reset () >>= fun reset_ok ->
          if reset_ok then
            retry_on_connection_error ~n:(n - 1) f
          else
            Fiber.return r
       | Error _ as r -> Fiber.return r)

    let send_oneshot_query
          ?params ?param_types ?binary_params ?(single_row_mode = false)
          query =
      retry_on_connection_error begin fun () ->
        Fiber.return @@ wrap_pg ~query begin fun () ->
          db#send_query ?params ?param_types ?binary_params query;
          if single_row_mode then db#set_single_row_mode;
          db#consume_input
        end
      end

    let send_prepared_query query_id prepared params =
      let {query; param_types; binary_params; single_row_mode; _} =
        prepared
      in
      retry_on_connection_error begin fun () ->
        begin
          if Int_hashtbl.mem prepare_cache query_id then Fiber.return (Ok ()) else
          Fiber.return @@ wrap_pg ~query begin fun () ->
            db#send_prepare ~param_types (query_name_of_id query_id) query;
            db#consume_input
          end >>=? fun () ->
          Pg_io.get_final_result ~stdenv ~uri ~query db >|=? fun result ->
          Pg_io.check_command_result ~uri ~query result |>? fun () ->
          Ok (Int_hashtbl.add prepare_cache query_id prepared)
        end >|=? fun () ->
        wrap_pg ~query begin fun () ->
          db#send_query_prepared
            ~params ~binary_params (query_name_of_id query_id);
          if single_row_mode then db#set_single_row_mode;
          db#consume_input
        end
      end

    let fetch_one_result ~query () =
      Pg_io.get_one_result ~stdenv ~uri ~query db

    let fetch_final_result ~query () =
      Pg_io.get_final_result ~stdenv ~uri ~query db

    let fetch_single_row ~query () =
      Pg_io.get_one_result ~stdenv ~uri ~query db >>=? fun result ->
      (match result#status with
       | Pg.Single_tuple ->
          assert (result#ntuples = 1);
          Fiber.return (Ok (Some result))
       | Pg.Tuples_ok ->
          assert (result#ntuples = 0);
          Pg_io.get_next_result ~stdenv ~uri ~query db >|=?
          (function
           | None -> Ok None
           | Some _ ->
              let msg =
                Caqti_error.Msg "Extra result after final single-row result." in
              Error (Caqti_error.response_rejected ~uri ~query msg))
       | _ ->
          Fiber.return @@ Result.map (fun () -> None) @@
          Pg_io.check_query_result
            ~uri ~query ~row_mult:Caqti_mult.zero_or_more ~single_row_mode:true
            result)

    module Response = struct

      type source =
        | Complete of Pg.result
        | Single_row

      type ('b, 'm) t = {
        row_type: 'b Caqti_type.t;
        source: source;
        query: string;
      }

      let returned_count = function
       | {source = Complete result; _} ->
          Fiber.return (Ok result#ntuples)
       | {source = Single_row; _} ->
          Fiber.return (Error `Unsupported)

      let affected_count = function
       | {source = Complete result; _} ->
          Fiber.return (Ok (int_of_string result#cmd_tuples))
       | {source = Single_row; _} ->
          Fiber.return (Error `Unsupported)

      let exec _ = Fiber.return (Ok ())

      let find = function
       | {row_type; source = Complete result; _} ->
          Fiber.return (decode_row ~uri row_type (result, 0))
       | {source = Single_row; _} ->
          assert false

      let find_opt = function
       | {row_type; source = Complete result; _} ->
          Fiber.return begin
            if result#ntuples = 0 then Ok None else
            (match decode_row ~uri row_type (result, 0) with
             | Ok y -> Ok (Some y)
             | Error _ as r -> r)
          end
       | {source = Single_row; _} ->
          assert false

      let fold f {row_type; query; source} =
        let decode = decode_row ~uri row_type in
        (match source with
         | Complete result ->
            let n = result#ntuples in
            let rec loop i acc =
              if i = n then Ok acc else
              (match decode (result, i) with
               | Ok y -> loop (i + 1) (f y acc)
               | Error _ as r -> r)
            in
            fun acc -> Fiber.return (loop 0 acc)
         | Single_row ->
            let rec loop acc =
              fetch_single_row ~query () >>=? function
               | None -> Fiber.return (Ok acc)
               | Some result ->
                  (match decode (result, 0) with
                   | Ok y -> loop (f y acc)
                   | Error _ as r -> Fiber.return r)
            in
            loop)

      let fold_s f {row_type; query; source} =
        let decode = decode_row ~uri row_type in
        (match source with
         | Complete result ->
            let n = result#ntuples in
            let rec loop i acc =
              if i = n then Fiber.return (Ok acc) else
              (match decode (result, i) with
               | Ok y -> f y acc >>=? loop (i + 1)
               | Error _ as r -> Fiber.return r)
            in
            loop 0
         | Single_row ->
            let rec loop acc =
              fetch_single_row ~query () >>=? function
               | None -> Fiber.return (Ok acc)
               | Some result ->
                  (match decode (result, 0) with
                   | Ok y -> f y acc >>=? loop
                   | Error _ as r -> Fiber.return r)
            in
            loop)

      let iter_s f {row_type; query; source} =
        let decode = decode_row ~uri row_type in
        (match source with
         | Complete result ->
            let n = result#ntuples in
            let rec loop i =
              if i = n then Fiber.return (Ok ()) else
              (match decode (result, i) with
               | Ok y -> f y >>=? fun () -> loop (i + 1)
               | Error _ as r -> Fiber.return r)
            in
            loop 0
         | Single_row ->
            let rec loop () =
              fetch_single_row ~query () >>=? function
               | None -> Fiber.return (Ok ())
               | Some result ->
                  (match decode (result, 0) with
                   | Ok y -> f y >>=? fun () -> loop ()
                   | Error _ as r -> Fiber.return r)
            in
            loop ())

      let to_stream {row_type; query; source} =
        let decode = decode_row ~uri row_type in
        (match source with
         | Complete result ->
            let n = result#ntuples in
            let rec seq i () =
              if i = n then Fiber.return Stream.Nil else
              (match decode (result, i) with
               | Ok y -> Fiber.return (Stream.Cons (y, seq (i + 1)))
               | Error err -> Fiber.return (Stream.Error err))
            in
            seq 0
         | Single_row ->
            let rec seq () =
              fetch_single_row ~query () >|= function
               | Ok None -> Stream.Nil
               | Ok (Some result) ->
                  (match decode (result, 0) with
                   | Ok y -> Stream.Cons (y, seq)
                   | Error err -> Stream.Error err)
               | Error err -> Stream.Error err
            in
            seq)
    end

    let type_oid_cache = Hashtbl.create 19

    let pp_request_with_param ppf =
      Caqti_template.Request.make_pp_with_param ~subst ~dialect () ppf

    let call' ~f req param =
      Log.debug ~src:Logging.request_log_src (fun f ->
        f "Sending %a" pp_request_with_param (req, param)) >>= fun () ->

      let single_row_mode =
        use_single_row_mode
          && Caqti_mult.can_be_many (Caqti_template.Request.row_mult req)
      in

      (* Prepare, if requested, and send the query. *)
      let param_type = Caqti_template.Request.param_type req in
      (match Caqti_template.Request.query_id req with
       | None ->
          let templ = Caqti_template.Request.query req dialect in
          let query = Pg_ext.query_string ~subst db templ in
          let param_length = Caqti_type.length param_type in
          let param_types = Array.make param_length 0 in
          let binary_params = Array.make param_length false in
          init_param_types ~type_oid_cache param_types binary_params param_type
            |> Fiber.return >>=? fun () ->
          let params = Array.make param_length Pg.null in
          (match Param_encoder.encode ~uri params param_type param with
           | Ok () ->
              send_oneshot_query ~params ~binary_params ~single_row_mode query
                >|=? fun () ->
              Ok query
           | Error _ as r ->
              Fiber.return r)
       | Some query_id ->
          begin
            try Ok (Int_hashtbl.find prepare_cache query_id) with
             | Not_found ->
                let templ = Caqti_template.Request.query req dialect in
                let query = Pg_ext.query_string ~subst db templ in
                let param_length = Caqti_type.length param_type in
                let param_types = Array.make param_length 0 in
                let binary_params = Array.make param_length false in
                init_param_types
                    ~type_oid_cache param_types binary_params param_type
                  |>? fun () ->
                Ok {query; param_length; param_types; binary_params;
                    single_row_mode}
          end |> Fiber.return >>=? fun prepared ->
          let params = Array.make prepared.param_length Pg.null in
          (match Param_encoder.encode ~uri params param_type param with
           | Ok () ->
              send_prepared_query query_id prepared params >|=? fun () ->
              Ok prepared.query
           | Error _ as r ->
              Fiber.return r))
        >>=? fun query ->

      (* Fetch and process the result. *)
      let row_type = Caqti_template.Request.row_type req in
      if single_row_mode then
        f Response.{row_type; query; source = Single_row}
      else begin
        let row_mult = Caqti_template.Request.row_mult req in
        fetch_final_result ~query () >>=? fun result ->
        (match
            Pg_io.check_query_result
              ~uri ~query ~row_mult ~single_row_mode result
         with
         | Ok () -> f Response.{row_type; query; source = Complete result}
         | Error _ as r -> Fiber.return r)
      end

    let rec fetch_type_oids : type a. a Caqti_type.t -> _ = function
     | Caqti_type.Field (Enum name as field_type)
          when not (Hashtbl.mem type_oid_cache name) ->
        call' ~f:Response.find_opt Q.type_oid name >>=
        (function
         | Ok (Some oid) ->
            Fiber.return (Ok (Hashtbl.add type_oid_cache name oid))
         | Ok None ->
            Log.warn (fun p ->
              p "Failed to query OID for enum %s." name) >|= fun () ->
            Error (Caqti_error.encode_missing ~uri ~field_type ())
         | Error (`Encode_rejected _ | `Decode_rejected _ |
                  `Response_failed _ as err) ->
            Log.err (fun p ->
              p "Failed to fetch obtain OID for enum %s due to: %a"
                name Caqti_error.pp err) >|= fun () ->
            Error (Caqti_error.encode_missing ~uri ~field_type ())
         | Error #Caqti_error.call as r ->
            Fiber.return r)
     | Caqti_type.Field _ -> Fiber.return (Ok ())
     | Caqti_type.Option t -> fetch_type_oids t
     | Caqti_type.Product (_, _, prod) ->
        let rec loop : type i. (a, i) Caqti_type.product -> _ = function
         | Proj_end -> Fiber.return (Ok ())
         | Proj (t, _, prod) -> fetch_type_oids t >>=? fun () -> loop prod
        in
        loop prod
     | Caqti_type.Annot (_, t0) -> fetch_type_oids t0

    let using_db f =
      if !in_use then
        failwith "Invalid concurrent usage of PostgreSQL connection detected.";
      in_use := true;
      Fiber.cleanup
        (fun () -> f () >|= fun res -> in_use := false; res)
        (fun () -> reset () >|= fun _ -> in_use := false)

    let call ~f req param = using_db @@ fun () ->
      fetch_type_oids (Caqti_template.Request.param_type req) >>=? fun () ->
      call' ~f req param

    let deallocate req =
      (match Caqti_template.Request.query_id req with
       | Some query_id ->
          if Int_hashtbl.mem prepare_cache query_id then
            begin
              let query = sprintf "DEALLOCATE _caq%d" query_id in
              send_oneshot_query query >>=? fun () ->
              fetch_final_result ~query () >|=? fun result ->
              Int_hashtbl.remove prepare_cache query_id;
              Pg_io.check_query_result
                ~uri ~query ~row_mult:Caqti_mult.zero ~single_row_mode:false
                result
            end
          else
            Fiber.return (Ok ())
       | None ->
          failwith "deallocate called on oneshot request")

    let disconnect () = using_db @@ fun () ->
      try db#finish; Fiber.return () with Pg.Error err ->
        Log.warn (fun p ->
          p "While disconnecting from <%a>: %s"
            Caqti_error.pp_uri uri (Pg.string_of_error err))

    let validate () = using_db @@ fun () ->
      if (try db#consume_input; db#status = Pg.Ok with Pg.Error _ -> false) then
        Fiber.return true
      else
        reset ()

    let check f = f (try db#status = Pg.Ok with Pg.Error _ -> false)

    let exec q p = call ~f:Response.exec q p
    let start () = exec Q.start () >|=? fun () -> Ok (in_transaction := true)
    let commit () = in_transaction := false; exec Q.commit ()
    let rollback () = in_transaction := false; exec Q.rollback ()

    let set_statement_timeout t =
      let t_arg =
        (match t with
         | None -> 0
         | Some t -> max 1 (int_of_float (t *. 1000.0 +. 500.0)))
      in
      call ~f:Response.exec (Q.set_statement_timeout t_arg) ()

    let populate ~table ~columns row_type data =
      let query =
        sprintf "COPY %s (%s) FROM STDIN" table (String.concat "," columns)
      in
      let param_length = Caqti_type.length row_type in
      let fail msg =
        Fiber.return
          (Error (Caqti_error.request_failed ~uri ~query (Caqti_error.Msg msg)))
      in
      let pg_error err =
        let msg = extract_communication_error db err in
        Fiber.return (Error (Caqti_error.request_failed ~uri ~query msg))
      in
      let put_copy_data data =
        let rec loop fd =
          match db#put_copy_data data with
          | Pg.Put_copy_error ->
              fail "Unable to put copy data"
          | Pg.Put_copy_queued ->
              Fiber.return (Ok ())
          | Pg.Put_copy_not_queued ->
              Unix.poll ~stdenv ~write:true fd >>= fun _ -> loop fd
        in
        (match db#socket with
         | exception Pg.Error msg -> pg_error msg
         | socket -> Unix.wrap_fd loop (Obj.magic socket))
      in
      let copy_row row =
        let params = Array.make param_length "\\N" in
        (match Copy_encoder.encode ~uri params row_type row with
         | Ok () ->
            Fiber.return (Ok (String.concat "\t" (Array.to_list params)))
         | Error _ as r ->
            Fiber.return r)
        >>=? fun param_string -> put_copy_data (param_string ^ "\n")
      in
      begin
        (* Send the copy command to start the transfer.
         * Skip checking that there is only a single result: while in copy mode
         * we can repeatedly get the latest result and it will always be
         * Copy_in, so checking for a single result would trigger an error.
         *)
        send_oneshot_query query >>=? fun () ->
        fetch_one_result ~query ()
        >>=? fun result ->
          (* We expect the Copy_in response only - turn other success responses
           * into errors, and delegate error handling.
           *)
          (match result#status with
           | Pg.Copy_in -> Fiber.return (Ok ())
           | Pg.Command_ok -> fail "Received Command_ok when expecting Copy_in"
           | _ -> Fiber.return (Pg_io.check_command_result ~uri ~query result))
        >>=? fun () -> System.Stream.iter_s ~f:copy_row data
        >>=? fun () ->
          (* End the copy *)
          let rec copy_end_loop fd =
            match db#put_copy_end () with
            | Pg.Put_copy_error ->
                fail "Unable to finalize copy"
            | Pg.Put_copy_not_queued ->
                Unix.poll ~stdenv ~write:true fd >>= fun _ ->
                copy_end_loop fd
            | Pg.Put_copy_queued ->
                Fiber.return (Ok ())
          in
          (match db#socket with
           | exception Pg.Error msg -> pg_error msg
           | socket -> Unix.wrap_fd copy_end_loop (Obj.magic socket))
        >>=? fun () ->
          (* After ending the copy, there will be a new result for the initial
           * query.
           *)
        fetch_final_result ~query () >|=? Pg_io.check_command_result ~uri ~query
      end
  end

  let connect ~sw:_ ~stdenv ~subst ~config:_ uri =
    Fiber.return (Pg_ext.parse_uri uri)
      >>=? fun (conninfo, notice_processing, use_single_row_mode) ->
    (match new Pg.connection ~conninfo () with
     | exception Pg.Error err ->
        let msg = extract_connect_error err in
        Fiber.return (Error (Caqti_error.connect_failed ~uri msg))
     | db ->
        Pg_io.communicate ~stdenv db (fun () -> db#connect_poll) >>=
        (function
         | Error err ->
            let msg = extract_communication_error db err in
            Fiber.return (Error (Caqti_error.connect_failed ~uri msg))
         | Ok () ->
            (match db#status <> Pg.Ok with
             | exception Pg.Error err ->
                let msg = extract_communication_error db err in
                Fiber.return (Error (Caqti_error.connect_failed ~uri msg))
             | true ->
                let msg = Caqti_error.Msg db#error_message in
                Fiber.return (Error (Caqti_error.connect_failed ~uri msg))
             | false ->
                db#set_notice_processing notice_processing;
                let server_version =
                  let v0, v1, v2 = db#server_version in
                  Caqti_template.Version.of_string_unsafe
                    (if v0 < 10 then
                      Printf.sprintf "%d.%d.%d" v0 v1 v2
                     else
                      Printf.sprintf "%d.%d" v0 (v1 * 100 + v2))
                in
                let module B = Make_connection_base
                  (struct
                    let dialect = Caqti_template.Dialect.Pgsql {
                      server_version;
                      ocaml_library = `postgresql;
                      reserved = ();
                    }
                    let subst = subst dialect
                    let stdenv = stdenv
                    let uri = uri
                    let db = db
                    let use_single_row_mode = use_single_row_mode
                  end)
                in
                let module Connection = struct
                  let driver_info = driver_info
                  let driver_connection = None
                  include B
                  include Connection_utils.Make_convenience (System) (B)
                end in
                Connection.exec Q.set_timezone_to_utc () >|=
                (function
                 | Ok () -> Ok (module Connection : CONNECTION)
                 | Error err -> Error (`Post_connect err)))))
end

let () =
  let open Caqti_platform_unix.Driver_loader in
  register "postgres" (module Connect_functor);
  register "postgresql" (module Connect_functor)
