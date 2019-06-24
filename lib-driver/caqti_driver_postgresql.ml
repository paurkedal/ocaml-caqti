(* Copyright (C) 2017--2019  Petter A. Urkedal <paurkedal@gmail.com>
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
     | Caqti_sql.L s -> Buffer.add_string buf s
     | Caqti_sql.P i -> bprintf buf "$%d" (i + 1)
     | Caqti_sql.S frags -> List.iter loop frags in
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

(* We cache the query string and properties globally using a weak map to avoid
 * reconstruction.  Although designed to work for any query, it is only used for
 * prepared queries, as it is unclear whether it benefits oneshot queries. *)
module Any_request = struct
  type t = Any : ('a, 'b, 'm) Caqti_request.t -> t

  let hash (Any req) =
    (match Caqti_request.query_id req with
     | Some id -> Hashtbl.hash id
     | None -> Hashtbl.hash (Caqti_request.query req))

  let equal (Any req1) (Any req2) =
    (match Caqti_request.query_id req1, Caqti_request.query_id req2 with
     | Some id1, Some id2 -> id1 = id2
     | None, Some _ | Some _, None -> false
     | None, None -> Caqti_request.query req1 = Caqti_request.query req2)
end
module Request_cache = Ephemeron.K1.Make (Any_request)

type request_cache_entry = {
  query_name: string;
  query: string;
  param_length: int;
  binary_params: bool array;
}

let request_cache : request_cache_entry Request_cache.t =
  Request_cache.create 27

module Connect_functor (System : Caqti_driver_sig.System_unix) = struct
  open System

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
      let pg_error msg =
        Error (Caqti_error.request_failed ~uri ~query (Pg_msg msg)) in
      let rec aux fd =
        (match db#consume_input; db#is_busy with
         | exception Pg.Error msg -> return (pg_error msg)
         | true ->
            Unix.poll ~read:true fd >>= fun _ -> aux fd
         | false ->
            (match db#get_result with
             | exception Pg.Error msg -> return (pg_error msg)
             | result -> return (Ok result)))
      in
      (match db#socket with
       | exception Pg.Error msg -> return (pg_error msg)
       | socket -> Unix.wrap_fd aux (Obj.magic socket))

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
       | exception Pg.Error msg ->
          Error (Caqti_error.request_failed ~uri ~query (Pg_msg msg))
       | Pg.Command_ok ->
          ( match Caqti_mult.expose mult with
            | `Zero -> Ok ()
            | (`One | `Zero_or_one | `Zero_or_more) ->
              reject "Tuples expected for this query."
            | `Zero_or_more_in ->
              reject "Copy response expected for this query"
          )
       | Pg.Tuples_ok ->
          ( match Caqti_mult.expose mult with
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
            | `Zero_or_more -> Ok ()
            | `Zero_or_more_in ->
              reject "Copy response expected for this query"
          )
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

  module type CONNECTION = Caqti_connection_sig.Base
    with type 'a future := 'a System.future
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t

  module Connection (Db : sig val uri : Uri.t val db : Pg.connection end)
    : CONNECTION =
  struct
    open Db

    let prepare_cache : (int, unit) Hashtbl.t = Hashtbl.create 19

    let reset () =
      Log.warn (fun p ->
        p "Lost connection to <%a>, reconnecting." Caqti_error.pp_uri uri)
        >>= fun () ->
      (match db#reset_start with
       | exception Pg.Error _ -> return false
       | true ->
          Hashtbl.clear prepare_cache;
          Pg_io.communicate db (fun () -> db#reset_poll) >|=
          (function
           | Error _ -> false
           | Ok () -> (try db#status = Pg.Ok with Pg.Error _ -> false))
       | false ->
          return false)

    let wrap_pg ~query f =
      try Ok (f ()) with
       | Postgresql.Error err ->
          Error (Caqti_error.request_failed ~uri ~query (Pg_msg err))

    let rec retry_pg ~query ?(n = 1) f =
      (f () : (_, [> Caqti_error.call]) result future) >>=
      (function
       | Ok _ as r -> return r
       | Error (`Request_failed
                {Caqti_error.msg = Pg_msg (Postgresql.Connection_failure _); _})
            as r when n > 0 ->
          reset () >>= fun reset_ok ->
          if reset_ok then
            retry_pg ~query ~n:(n - 1) f
          else
            return r
       | Error _ as r -> return r)

    let query_oneshot ?params ?binary_params query =
      retry_pg ~query begin fun () ->
        return @@ wrap_pg ~query
          (fun () -> db#send_query ?params ?binary_params query)
      end >>=
      (function
       | Error _ as r -> return r
       | Ok () -> Pg_io.get_result ~uri ~query db)

    let query_prepared query_id {query_name; query; binary_params; _} params =
      retry_pg ~query begin fun () ->
        begin
          if Hashtbl.mem prepare_cache query_id then return (Ok ()) else
          (match wrap_pg ~query (fun() -> db#send_prepare query_name query) with
           | Ok () ->
              Pg_io.get_result ~uri ~query db >|=
                (function
                 | Ok result ->
                    (match Pg_io.check_command_result ~uri ~query result with
                     | Ok () as r -> Hashtbl.add prepare_cache query_id (); r
                     | Error _ as r -> r)
                 | Error _ as r -> r)
           | Error _ as r -> return r)
        end >|=? fun () ->
        wrap_pg ~query
          (fun () -> db#send_query_prepared ~params ~binary_params query_name)
      end >>=
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

      let to_stream {row_type; result} =
        let n = result#ntuples in
        let rec f i () =
          if i = n then return Stream.Nil else
          (match decode_row ~uri (result, i) row_type with
           | Ok y -> return @@ Stream.Cons (y, f (i + 1))
           | Error err -> return @@ Stream.Error err) in
        f 0

      let from_stream _ _stream =
        let msg = Caqti_error.Msg "from_stream not implemented" in
        return (Error (Caqti_error.not_implemented ~uri msg))
    end

    let call ~f req param =

      (* Send the query. *)
      let param_type = Caqti_request.param_type req in
      (match Caqti_request.query_id req with
       | None ->
          let templ = Caqti_request.query req driver_info in
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
          let rck = Any_request.Any req in
          let rce = try Request_cache.find request_cache rck with
           | Not_found ->
              let query_name = sprintf "_caq%d" query_id in
              let templ = Caqti_request.query req driver_info in
              let query = Pg_ext.query_string templ in
              let param_length = Caqti_type.length param_type in
              let binary_params = Array.make param_length false in
              let nbp = set_binary_params binary_params param_type 0 in
              assert (nbp = param_length);
              let rce = {query_name; query; param_length; binary_params} in
              Request_cache.add request_cache rck rce;
              rce in
          let params = Array.make rce.param_length Pg.null in
          (match encode_param ~uri params param_type param 0 with
           | Ok n ->
              assert (n = rce.param_length);
              query_prepared query_id rce params >|=? fun result ->
              Ok (rce.query, result)
           | Error _ as r -> return r))
        >>=? fun (query, result) ->

      (* Fetch and process the result. *)
      let row_type = Caqti_request.row_type req in
      let row_mult = Caqti_request.row_mult req in
      (match Pg_io.check_query_result ~uri ~query result row_mult with
       | Ok () -> f Response.{row_type; result}
       | Error _ as r -> return r)

    let disconnect () =
      try db#finish; return () with Pg.Error err ->
        Log.warn (fun p ->
          p "While disconnecting from <%a>: %s"
            Caqti_error.pp_uri uri (Pg.string_of_error err))

    let validate () =
      if (try db#status = Pg.Ok with Pg.Error _ -> false) then
        return true
      else
        reset ()

    let check f = f (try db#status = Pg.Ok with Pg.Error _ -> false)

    let exec q p = call ~f:Response.exec q p
    let start () = exec start_req ()
    let commit () = exec commit_req ()
    let rollback () = exec rollback_req ()
  end

  let connect uri =
    let conninfo = Pg_ext.conninfo_of_uri uri in
    (match new Pg.connection ~conninfo () with
     | exception Pg.Error msg ->
        return (Error (Caqti_error.connect_failed ~uri (Pg_msg msg)))
     | db ->
        Pg_io.communicate db (fun () -> db#connect_poll) >|=
        (function
         | Error msg -> Error (Caqti_error.connect_failed ~uri (Pg_msg msg))
         | Ok () ->
            (match db#status <> Pg.Ok with
             | exception Pg.Error msg ->
                Error (Caqti_error.connect_failed ~uri (Pg_msg msg))
             | true ->
                let msg = db#error_message in
                Error (Caqti_error.connect_failed ~uri (Caqti_error.Msg msg))
             | false ->
                let module Connection =
                  Connection (struct let uri = uri let db = db end) in
                Ok (module Connection : CONNECTION))))
end

let () =
  Caqti_connect.define_unix_driver "postgres" (module Connect_functor);
  Caqti_connect.define_unix_driver "postgresql" (module Connect_functor)
