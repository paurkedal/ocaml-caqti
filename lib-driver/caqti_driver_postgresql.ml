(* Copyright (C) 2017--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_common_priv
open Printf
module Pg = Postgresql

let ( |>? ) r f = match r with Ok x -> f x | Error _ as r -> r
let ( %>? ) f g x = match f x with Ok y -> g y | Error _ as r -> r

module Int_hashable = struct
  type t = int
  let equal (i : int) (j : int) = i = j
  let hash (i : int) = Hashtbl.hash i
end

module Int_hashtbl = Hashtbl.Make (Int_hashable)

let start_req = Caqti_request.exec Caqti_type.unit "BEGIN"
let commit_req = Caqti_request.exec Caqti_type.unit "COMMIT"
let rollback_req = Caqti_request.exec Caqti_type.unit "ROLLBACK"
let type_oid_req = Caqti_request.find_opt Caqti_type.string Caqti_type.int
  "SELECT oid FROM pg_catalog.pg_type WHERE typname = ?"
let set_timezone_to_utc_req = Caqti_request.exec ~oneshot:true Caqti_type.unit
  "SET TimeZone TO 'UTC'"

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

  let query_string (db : Pg.connection) templ =
    let buf = Buffer.create 64 in
    let rec loop = function
     | Caqti_query.L s -> Buffer.add_string buf s
     | Caqti_query.Q s ->
        Buffer.add_char buf '\'';
        Buffer.add_string buf (db#escape_string s);
        Buffer.add_char buf '\''
     | Caqti_query.P i -> bprintf buf "$%d" (i + 1)
     | Caqti_query.S frags -> List.iter loop frags in
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

  let parse_uri uri =
    (match Uri.get_query_param uri "notice_processing" with
     | None ->
        Ok (`Quiet, uri)
     | Some "quiet" ->
        Ok (`Quiet, Uri.remove_query_param uri "notice_processing")
     | Some "stderr" ->
        Ok (`Stderr, Uri.remove_query_param uri "notice_processing")
     | Some _ ->
        let msg = Caqti_error.Msg "Invalid argument for notice_processing." in
        Error (Caqti_error.connect_rejected ~uri msg))
    |>? fun (notice_processing, uri') ->
    let conninfo =
      if Uri.host uri <> None then Uri.to_string uri' else
      let mkparam k v = k ^ " = '" ^ escaped_connvalue v ^ "'" in
      let mkparams (k, vs) = List.map (mkparam k) vs in
      String.concat " " (List.flatten (List.map mkparams (Uri.query uri')))
    in
    Ok (conninfo, notice_processing)

  let string_of_ptime_span t =
    let is_neg = Ptime.Span.compare t Ptime.Span.zero < 0 in
    let d, ps = Ptime.Span.(to_d_ps (abs t ))in
    let buf = Buffer.create 32 in
    if d <> 0 then bprintf buf "%d days " (if is_neg then -d else d);
    let s, ps = Int64.(div ps 1_000_000_000_000L |> to_int,
                       rem ps 1_000_000_000_000L) in
    let hour, s = s / 3600, s mod 3600 in
    let minute, s = s / 60, s mod 60 in
    if is_neg then Buffer.add_char buf '-';
    bprintf buf "%02d:%02d:%02d" hour minute s;
    if ps <> 0L then bprintf buf ".%06Ld" (Int64.div ps 1_000_000L);
    Buffer.contents buf

  let ps_of_decimal s =
    try
      (match String.length s with
       | 2 ->
          Ok (Int64.(mul (of_string s) 1_000_000_000_000L))
       | n when n < 2 ->
          Error "Missing digits in seconds of interval string."
       | _ when s.[2] <> '.' ->
          Error "Expected period after seconds in interval string."
       | n ->
          let buf = Bytes.make 14 '0' in
          Bytes.blit_string s 0 buf 0 2;
          Bytes.blit_string s 3 buf 2 (min 12 (n - 3));
          Ok (Int64.of_string (Bytes.to_string buf)))
    with Failure _ ->
      Error "Seconds in interval string is not numeric."

  let ps_of_hms s =
    (match String.split_on_char ':' s with
     | [hours; minutes; seconds] ->
        (try
          let hour = int_of_string hours in
          let minute = int_of_string minutes in
          (match ps_of_decimal seconds with
           | Ok ps ->
              let hm = Int64.of_int (abs hour * 60 + minute) in
              let ps = Int64.(add ps (mul hm 60_000_000_000_000L)) in
              Ok (if s.[0] = '-' then Int64.neg ps else ps)
           | Error msg -> Error msg)
         with Failure _ ->
          Error "Non-integer hour or minute in interval string.")
     | _ ->
        Error "Expected HH:MM:SS[.F] in interval string.")

  let ptime_span_of_string s =
    let span_of_d_ps (d, ps) =
      let d, ps =
        if Int64.compare ps Int64.zero >= 0
        then (d, ps)
        else (d - 1, Int64.add ps 86_400_000_000_000_000L) in
      (match Ptime.Span.of_d_ps (d, ps) with
       | Some t -> Ok t
       | None -> Error "Out for range for Ptime.span.") in
    try
      (match String.split_on_char ' ' s with
       | [d; "days"] ->
          span_of_d_ps (int_of_string d, 0L)
       | [d; "days"; hms] ->
          ps_of_hms hms |>? fun ps ->
          span_of_d_ps (int_of_string d, ps)
       | [hms] ->
          ps_of_hms hms |>? fun ps ->
          span_of_d_ps (0, ps)
       | _ ->
          Error "Unhandled interval format.")
    with Failure _ ->
      Error "Non-integer days in interval string."
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

let init_param_types ~uri ~type_oid_cache =
  let rec oid_of_field_type : type a. a Caqti_type.Field.t -> _ = function
   | Caqti_type.Bool -> Ok bool_oid
   | Caqti_type.Int -> Ok int8_oid
   | Caqti_type.Int16 -> Ok int2_oid
   | Caqti_type.Int32 -> Ok int4_oid
   | Caqti_type.Int64 -> Ok int8_oid
   | Caqti_type.Float -> Ok float8_oid
   | Caqti_type.String -> Ok unknown_oid
   | Caqti_type.Octets -> Ok bytea_oid
   | Caqti_type.Pdate -> Ok date_oid
   | Caqti_type.Ptime -> Ok timestamp_oid
   | Caqti_type.Ptime_span -> Ok interval_oid
   | Caqti_type.Enum name -> Ok (Hashtbl.find type_oid_cache name)
   | field_type ->
      (match Caqti_type.Field.coding driver_info field_type with
       | None ->
          Error (Caqti_error.encode_missing ~uri ~field_type ())
       | Some (Caqti_type.Field.Coding {rep; _}) ->
          oid_of_field_type rep)
  in
  let rec recurse : type a. _ -> _ -> a Caqti_type.t -> _ -> _
      = fun pt bp -> function
   | Caqti_type.Unit -> fun i -> Ok i
   | Caqti_type.Field ft -> fun i ->
      oid_of_field_type ft |>? fun oid ->
      pt.(i) <- oid;
      bp.(i) <- oid = bytea_oid;
      Ok (i + 1)
   | Caqti_type.Option t ->
      recurse pt bp t
   | Caqti_type.Tup2 (t0, t1) ->
      recurse pt bp t0 %>? recurse pt bp t1
   | Caqti_type.Tup3 (t0, t1, t2) ->
      recurse pt bp t0 %>? recurse pt bp t1 %>?
      recurse pt bp t2
   | Caqti_type.Tup4 (t0, t1, t2, t3) ->
      recurse pt bp t0 %>? recurse pt bp t1 %>?
      recurse pt bp t2 %>? recurse pt bp t3
   | Caqti_type.Custom {rep; _} ->
      recurse pt bp rep
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

  let rec encode_field
      : type a. uri: Uri.t -> a Caqti_type.field -> a -> (string, _) result =
    fun ~uri field_type x ->
    (match field_type with
     | Caqti_type.Bool -> Ok (Pg_ext.string_of_bool x)
     | Caqti_type.Int -> Ok (string_of_int x)
     | Caqti_type.Int16 -> Ok (string_of_int x)
     | Caqti_type.Int32 -> Ok (Int32.to_string x)
     | Caqti_type.Int64 -> Ok (Int64.to_string x)
     | Caqti_type.Float -> Ok (sprintf "%.17g" x)
     | Caqti_type.String -> Ok (encode_string x)
     | Caqti_type.Enum _ -> Ok (encode_string x)
     | Caqti_type.Octets -> Ok (encode_octets x)
     | Caqti_type.Pdate -> Ok (iso8601_of_pdate x)
     | Caqti_type.Ptime ->
        Ok (Ptime.to_rfc3339 ~space:true ~tz_offset_s:0 ~frac_s:6 x)
     | Caqti_type.Ptime_span ->
        Ok (Pg_ext.string_of_ptime_span x)
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

  let rec encode'
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
     | Caqti_type.Option t, Some x ->
        encode' ~uri params t x
     | Caqti_type.Tup2 (t0, t1), (x0, x1) ->
        encode' ~uri params t0 x0 %>?
        encode' ~uri params t1 x1
     | Caqti_type.Tup3 (t0, t1, t2), (x0, x1, x2) ->
        encode' ~uri params t0 x0 %>?
        encode' ~uri params t1 x1 %>?
        encode' ~uri params t2 x2
     | Caqti_type.Tup4 (t0, t1, t2, t3), (x0, x1, x2, x3) ->
        encode' ~uri params t0 x0 %>?
        encode' ~uri params t1 x1 %>?
        encode' ~uri params t2 x2 %>?
        encode' ~uri params t3 x3
     | Caqti_type.Custom {rep; encode = encode_custom; _}, x -> fun i ->
        (match encode_custom x with
         | Ok y ->
            encode' ~uri params rep y i
         | Error msg ->
            let msg = Caqti_error.Msg msg in
            Error (Caqti_error.encode_rejected ~uri ~typ:t msg))
     | Caqti_type.Annot (_, t0), x0 ->
        encode' ~uri params t0 x0)

  let encode ~uri params t x =
    (match encode' ~uri params t x 0 with
     | Ok n -> assert (n = Array.length params); Ok ()
     | Error _ as r -> r)
end

module Param_encoder = Make_encoder (struct
  let encode_string s = s
  let encode_octets s = s
end)

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
   | Caqti_type.Int16 -> conv int_of_string s
   | Caqti_type.Int32 -> conv Int32.of_string s
   | Caqti_type.Int64 -> conv Int64.of_string s
   | Caqti_type.Float -> conv float_of_string s
   | Caqti_type.String -> Ok s
   | Caqti_type.Enum _ -> Ok s
   | Caqti_type.Octets -> Ok (Postgresql.unescape_bytea s)
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
   | Caqti_type.Ptime_span ->
      (match Pg_ext.ptime_span_of_string s with
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
        (resp#getisnull i k && null_only (k + 1)) in
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
       | Error _ as r -> r)
   | Caqti_type.Annot (_, t0) -> fun j ->
      decode_row' ~uri row t0 j)

let decode_row ~uri resp row_type =
  (match decode_row' ~uri resp row_type 0 with
   | Ok (j, y) -> assert (j = Caqti_type.length row_type); Ok y
   | Error _ as r -> r)

type prepared = {
  query: string;
  param_length: int;
  param_types: Pg.oid array;
  binary_params: bool array;
}

module Connect_functor (System : Caqti_driver_sig.System_unix) = struct
  open System
  module H = Caqti_connection.Make_helpers (System)

  let (>>=?) m mf = m >>= (function Ok x -> mf x | Error _ as r -> return r)
  let (>|=?) m f = m >|= (function Ok x -> f x | Error _ as r -> r)

  let driver_info = driver_info

  module Pg_io = struct

    let communicate db step =
      let aux fd =
        let rec loop = function
         | Pg.Polling_reading ->
            Unix.poll ~read:true fd >>= fun _ ->
            (match step () with
             | exception Pg.Error msg -> return (Error msg)
             | ps -> loop ps)
         | Pg.Polling_writing ->
            Unix.poll ~write:true fd >>= fun _ ->
            (match step () with
             | exception Pg.Error msg -> return (Error msg)
             | ps -> loop ps)
         | Pg.Polling_failed | Pg.Polling_ok ->
            return (Ok ())
        in
        loop Pg.Polling_writing
      in
      (match db#socket with
       | exception Pg.Error msg -> return (Error msg)
       | socket -> Unix.wrap_fd aux (Obj.magic socket))

    let get_next_result ~uri ~query db =
      let rec retry fd =
        db#consume_input;
        if db#is_busy then
          Unix.poll ~read:true fd >>= (fun _ -> retry fd)
        else
          return (Ok db#get_result)
      in
      try Unix.wrap_fd retry (Obj.magic db#socket)
      with Pg.Error msg ->
        return (Error (Caqti_error.request_failed ~uri ~query (Pg_msg msg)))

    let get_result ~uri ~query ?(ensure_single_result=true) db =
      get_next_result ~uri ~query db >>=
      (function
       | Ok None ->
          let msg = Caqti_error.Msg "No response received after send." in
          return (Error (Caqti_error.request_failed ~uri ~query msg))
       | Ok (Some result) ->
          if ensure_single_result then
            get_next_result ~uri ~query db >>=
            (function
             | Ok None -> return (Ok result)
             | Ok (Some _) ->
                let msg = Caqti_error.Msg "More than one response received." in
                return (Error (Caqti_error.response_rejected ~uri ~query msg))
             | Error _ as r -> return r)
          else
            return (Ok result)
       | Error _ as r -> return r)

    let check_query_result ~uri ~query result mult =
      let reject msg =
        let msg = Caqti_error.Msg msg in
        Error (Caqti_error.response_rejected ~uri ~query msg) in
      let fail msg =
        let msg = Caqti_error.Msg msg in
        Error (Caqti_error.request_failed ~uri ~query msg) in
      (match result#status with
       | exception Pg.Error msg ->
          Error (Caqti_error.request_failed ~uri ~query (Pg_msg msg))
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

    let get_and_check_result ~uri ~query db =
      get_result ~uri ~query db
      >|=? check_command_result ~uri ~query
  end

  (* Driver Interface *)

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a future := 'a System.future
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t

  module Make_connection_base
          (Db : sig val uri : Uri.t val db : Pg.connection end) =
  struct
    open Db

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

    let using_db_ref = ref false
    let using_db f =
      H.assert_single_use ~what:"PostgreSQL connection" using_db_ref f

    let in_transaction = ref false
    let prepare_cache : prepared Int_hashtbl.t = Int_hashtbl.create 19

    let query_name_of_id = sprintf "_caq%d"

    let wrap_pg ~query f =
      try Ok (f ()) with
       | Postgresql.Error err ->
          Error (Caqti_error.request_failed ~uri ~query (Pg_msg err))

    let send_query ?params ?param_types ?binary_params query =
      wrap_pg ~query @@ fun () ->
      db#send_query ?params ?param_types ?binary_params query

    let send_prepare ?param_types query_id query =
      wrap_pg ~query @@ fun () ->
      db#send_prepare ?param_types (query_name_of_id query_id) query

    let send_query_prepared ?params ?binary_params query_id query =
      wrap_pg ~query @@ fun () ->
      db#send_query_prepared ?params ?binary_params (query_name_of_id query_id)

    let reset () =
      Log.warn (fun p ->
        p "Lost connection to <%a>, reconnecting." Caqti_error.pp_uri uri)
        >>= fun () ->
      in_transaction := false;
      (match db#reset_start with
       | exception Pg.Error _ -> return false
       | true ->
          Int_hashtbl.clear prepare_cache;
          Pg_io.communicate db (fun () -> db#reset_poll) >|=
          (function
           | Error _ -> false
           | Ok () -> (try db#status = Pg.Ok with Pg.Error _ -> false))
       | false ->
          return false)

    let rec retry_on_connection_error ?(n = 1) f =
      if !in_transaction then f () else
      (f () : (_, [> Caqti_error.call]) result future) >>=
      (function
       | Ok _ as r -> return r
       | Error (`Request_failed
                {Caqti_error.msg = Pg_msg (Postgresql.Connection_failure _); _})
            as r when n > 0 ->
          reset () >>= fun reset_ok ->
          if reset_ok then
            retry_on_connection_error ~n:(n - 1) f
          else
            return r
       | Error _ as r -> return r)

    let query_oneshot ?params ?param_types ?binary_params ?ensure_single_result
                      query =
      retry_on_connection_error begin fun () ->
        return (send_query ?params ?param_types ?binary_params query)
      end >>= function
       | Error _ as r -> return r
       | Ok () -> Pg_io.get_result ~uri ~query ?ensure_single_result db

    let query_prepared query_id prepared params =
      let {query; param_types; binary_params; _} = prepared in
      retry_on_connection_error begin fun () ->
        begin
          if Int_hashtbl.mem prepare_cache query_id then return (Ok()) else
          (match send_prepare ~param_types query_id query with
           | Ok () ->
              Pg_io.get_result ~uri ~query db >|=
              (function
               | Ok result -> Pg_io.check_command_result ~uri ~query result
               | Error _ as r -> r)
           | Error _ as r ->
              return r) >|=? fun () ->
          Ok (Int_hashtbl.add prepare_cache query_id prepared)
        end >|=? fun () ->
        send_query_prepared ~params ~binary_params query_id query
      end >>=? fun () ->
      Pg_io.get_result ~uri ~query db

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

      let to_stream {row_type; result} =
        let n = result#ntuples in
        let rec f i () =
          if i = n then return Stream.Nil else
          (match decode_row ~uri (result, i) row_type with
           | Ok y -> return @@ Stream.Cons (y, f (i + 1))
           | Error err -> return @@ Stream.Error err) in
        f 0
    end

    let type_oid_cache = Hashtbl.create 19

    let call' ~f req param =
      Log.debug ~src:request_log_src (fun f ->
        f "Sending %a" (Caqti_request.pp_with_param ~driver_info) (req, param))
        >>= fun () ->

      (* Prepare, if requested, and send the query. *)
      let param_type = Caqti_request.param_type req in
      (match Caqti_request.query_id req with
       | None ->
          let templ = Caqti_request.query req driver_info in
          let query = Pg_ext.query_string Db.db templ in
          let param_length = Caqti_type.length param_type in
          let param_types = Array.make param_length 0 in
          let binary_params = Array.make param_length false in
          init_param_types
              ~uri ~type_oid_cache param_types binary_params param_type
            |> return >>=? fun () ->
          let params = Array.make param_length Pg.null in
          (match Param_encoder.encode ~uri params param_type param with
           | Ok () ->
              query_oneshot ~params ~binary_params query >|=? fun result ->
              Ok (query, result)
           | Error _ as r ->
              return r)
       | Some query_id ->
          begin
            try Ok (Int_hashtbl.find prepare_cache query_id) with
             | Not_found ->
                let templ = Caqti_request.query req driver_info in
                let query = Pg_ext.query_string Db.db templ in
                let param_length = Caqti_type.length param_type in
                let param_types = Array.make param_length 0 in
                let binary_params = Array.make param_length false in
                init_param_types
                    ~uri ~type_oid_cache param_types binary_params param_type
                  |>? fun () ->
                Ok {query; param_length; param_types; binary_params}
          end |> return >>=? fun prepared ->
          let params = Array.make prepared.param_length Pg.null in
          (match Param_encoder.encode ~uri params param_type param with
           | Ok () ->
              query_prepared query_id prepared params >|=? fun result ->
              Ok (prepared.query, result)
           | Error _ as r ->
              return r))
        >>=? fun (query, result) ->

      (* Fetch and process the result. *)
      let row_type = Caqti_request.row_type req in
      let row_mult = Caqti_request.row_mult req in
      (match Pg_io.check_query_result ~uri ~query result row_mult with
       | Ok () -> f Response.{row_type; result}
       | Error _ as r -> return r)

    let rec fetch_type_oids : type a. a Caqti_type.t -> _ = function
     | Caqti_type.Unit -> return (Ok ())
     | Caqti_type.Field (Caqti_type.Enum name as field_type)
          when not (Hashtbl.mem type_oid_cache name) ->
        call' ~f:Response.find_opt type_oid_req name >>=
        (function
         | Ok (Some oid) ->
            return (Ok (Hashtbl.add type_oid_cache name oid))
         | Ok None ->
            Log.warn (fun p ->
              p "Failed to query OID for enum %s." name) >|= fun () ->
            Error (Caqti_error.encode_missing ~uri ~field_type ())
         | Error (`Encode_rejected _ | `Decode_rejected _ as err) ->
            Log.err (fun p ->
              p "Failed to fetch obtain OID for enum %s due to: %a"
                name Caqti_error.pp err) >|= fun () ->
            Error (Caqti_error.encode_missing ~uri ~field_type ())
         | Error #Caqti_error.call as r ->
            return r)
     | Caqti_type.Field _ -> return (Ok ())
     | Caqti_type.Option t -> fetch_type_oids t
     | Caqti_type.Tup2 (t1, t2) ->
        fetch_type_oids t1 >>=? fun () ->
        fetch_type_oids t2
     | Caqti_type.Tup3 (t1, t2, t3) ->
        fetch_type_oids t1 >>=? fun () ->
        fetch_type_oids t2 >>=? fun () ->
        fetch_type_oids t3
     | Caqti_type.Tup4 (t1, t2, t3, t4) ->
        fetch_type_oids t1 >>=? fun () ->
        fetch_type_oids t2 >>=? fun () ->
        fetch_type_oids t3 >>=? fun () ->
        fetch_type_oids t4
     | Caqti_type.Custom {rep; _} -> fetch_type_oids rep
     | Caqti_type.Annot (_, t0) -> fetch_type_oids t0

    let call ~f req param = using_db @@ fun () ->
      fetch_type_oids (Caqti_request.param_type req) >>=? fun () ->
      call' ~f req param

    let deallocate req =
      (match Caqti_request.query_id req with
       | Some query_id ->
          if Int_hashtbl.mem prepare_cache query_id then
            begin
              let query = sprintf "DEALLOCATE _caq%d" query_id in
              query_oneshot query >|=? fun result ->
              Int_hashtbl.remove prepare_cache query_id;
              Pg_io.check_query_result ~uri ~query result Caqti_mult.zero
            end
          else
            return (Ok ())
       | None ->
          failwith "deallocate called on oneshot request")

    let disconnect () = using_db @@ fun () ->
      try db#finish; return () with Pg.Error err ->
        Log.warn (fun p ->
          p "While disconnecting from <%a>: %s"
            Caqti_error.pp_uri uri (Pg.string_of_error err))

    let validate () = using_db @@ fun () ->
      if (try db#status = Pg.Ok with Pg.Error _ -> false) then
        return true
      else
        reset ()

    let check f = f (try db#status = Pg.Ok with Pg.Error _ -> false)

    let exec q p = call ~f:Response.exec q p
    let start () = exec start_req () >|=? fun () -> Ok (in_transaction := true)
    let commit () = in_transaction := false; exec commit_req ()
    let rollback () = in_transaction := false; exec rollback_req ()

    let populate ~table ~columns row_type data =
      let query =
        sprintf "COPY %s (%s) FROM STDIN" table (String.concat "," columns)
      in
      let param_length = Caqti_type.length row_type in
      let fail msg =
        return
          (Error (Caqti_error.request_failed ~uri ~query (Caqti_error.Msg msg)))
      in
      let pg_error msg =
        return
          (Error (Caqti_error.request_failed ~uri ~query (Pg_msg msg)))
      in
      let put_copy_data data =
        let rec loop fd =
          match db#put_copy_data data with
          | Pg.Put_copy_error ->
              fail "Unable to put copy data"
          | Pg.Put_copy_queued ->
              return (Ok ())
          | Pg.Put_copy_not_queued ->
              Unix.poll ~write:true fd >>= fun _ -> loop fd
        in
        (match db#socket with
         | exception Pg.Error msg -> pg_error msg
         | socket -> Unix.wrap_fd loop (Obj.magic socket))
      in
      let copy_row row =
        let params = Array.make param_length "\\N" in
        (match Copy_encoder.encode ~uri params row_type row with
         | Ok () ->
            return (Ok (String.concat "\t" (Array.to_list params)))
         | Error _ as r ->
            return r)
        >>=? fun param_string -> put_copy_data (param_string ^ "\n")
      in
      begin
        (* Send the copy command to start the transfer.
         * Skip checking that there is only a single result: while in copy mode
         * we can repeatedly get the latest result and it will always be
         * Copy_in, so checking for a single result would trigger an error.
         *)
        query_oneshot ~ensure_single_result:false query
        >>=? fun result ->
          (* We expect the Copy_in response only - turn other success responses
           * into errors, and delegate error handling.
           *)
          (match result#status with
           | Pg.Copy_in -> return (Ok ())
           | Pg.Command_ok -> fail "Received Command_ok when expecting Copy_in"
           | _ -> return (Pg_io.check_command_result ~uri ~query result))
        >>=? fun () -> System.Stream.iter_s ~f:copy_row data
        >>=? fun () ->
          (* End the copy *)
          let rec copy_end_loop fd =
            match db#put_copy_end () with
            | Pg.Put_copy_error ->
                fail "Unable to finalize copy"
            | Pg.Put_copy_not_queued ->
                Unix.poll ~write:true fd >>= fun _ -> copy_end_loop fd
            | Pg.Put_copy_queued ->
                return (Ok ())
          in
          (match db#socket with
           | exception Pg.Error msg -> pg_error msg
           | socket -> Unix.wrap_fd copy_end_loop (Obj.magic socket))
        >>=? fun () ->
          (* After ending the copy, there will be a new result for the initial
           * query.
           *)
          Pg_io.get_and_check_result ~uri ~query db
      end
  end

  let connect uri =
    return (Pg_ext.parse_uri uri) >>=? fun (conninfo, notice_processing) ->
    (match new Pg.connection ~conninfo () with
     | exception Pg.Error msg ->
        return (Error (Caqti_error.connect_failed ~uri (Pg_msg msg)))
     | db ->
        Pg_io.communicate db (fun () -> db#connect_poll) >>=
        (function
         | Error msg ->
            return (Error (Caqti_error.connect_failed ~uri (Pg_msg msg)))
         | Ok () ->
            (match db#status <> Pg.Ok with
             | exception Pg.Error msg ->
                return (Error (Caqti_error.connect_failed ~uri (Pg_msg msg)))
             | true ->
                let msg = Caqti_error.Msg db#error_message in
                return (Error (Caqti_error.connect_failed ~uri msg))
             | false ->
                db#set_notice_processing notice_processing;
                let module B =
                  Make_connection_base (struct let uri = uri let db = db end)
                in
                let module Connection = struct
                  let driver_info = driver_info
                  include B
                  include Caqti_connection.Make_convenience (System) (B)
                end in
                Connection.exec set_timezone_to_utc_req () >|=
                (function
                 | Ok () -> Ok (module Connection : CONNECTION)
                 | Error err -> Error (`Post_connect err)))))
end

let () =
  Caqti_connect.define_unix_driver "postgres" (module Connect_functor);
  Caqti_connect.define_unix_driver "postgresql" (module Connect_functor)
