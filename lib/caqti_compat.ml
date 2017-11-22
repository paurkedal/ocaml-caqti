(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_errors
open Caqti_query
open Caqti_sigs

module Type = Caqti_type

type Caqti_error.driver_msg += Driver_msg of string

let () =
  let pp ppf = function
   | Driver_msg msg -> Format.pp_print_string ppf msg
   | _ -> assert false in
  Caqti_error.define_driver_msg ~pp [%extension_constructor Driver_msg]

let template_param_order templ di =
  let open Caqti_request in
  let rec loop = function
   | L _ | S [] -> fun acc -> acc
   | P i -> fun acc -> i :: acc
   | S (frag :: templ) -> fun acc -> acc |> loop frag |> loop (S templ) in
  (match Caqti_driver_info.parameter_style di with
   | `Indexed _ -> None
   | `Linear _ -> Some (Array.of_list (List.rev (loop templ [])))
   | `None -> assert false)

let template_query_string templ di =
  let pstyle = Caqti_driver_info.parameter_style di in
  let buf = Buffer.create 64 in
  let open Caqti_request in
  let rec loop = function
   | L s -> Buffer.add_string buf s
   | P i ->
      Buffer.add_string buf
        (match pstyle with
         | `Indexed f -> f i
         | `Linear s -> s
         | `None -> assert false)
   | S frags -> List.iter loop frags in
  loop templ;
  Buffer.contents buf

module Connection_v2_of_v1
    (System : Caqti_system_sig.S)
    (C : CONNECTION with type 'a io = 'a System.io) =
struct
  open System

  let param_pdate x =
    C.Param.date_tuple (Ptime.v (x, 0L) |> Ptime.to_date)

  let tuple_pdate i tup =
    (match Ptime.of_date (C.Tuple.date_tuple i tup) with
     | None -> assert false
     | Some d -> Ptime.to_span d |> Ptime.Span.to_d_ps |> fst)

  let param_ptime x =
    C.Param.utc_float (Ptime.to_float_s x)

  let tuple_ptime i tup =
    (match Ptime.of_float_s (C.Tuple.utc_float i tup) with
     | None -> assert false
     | Some d -> d)

  let qs_of_qi = function
   | `Oneshot s -> s
   | `Prepared (_, s) -> s

  let prepare_failed uri qi msg =
    let query_string = qs_of_qi qi in
    Caqti_error.request_rejected ~uri ~query_string (Driver_msg msg)

  let execute_failed uri qi msg =
    let query_string = qs_of_qi qi in
    Caqti_error.request_failed ~uri ~query_string (Driver_msg msg)

  let miscommunication uri qi msg =
    let query_string = qs_of_qi qi in
    Caqti_error.response_rejected ~uri ~query_string msg

  let catch_response f =
    catch
      (fun () -> f () >|= fun y -> Ok y)
      (function
       | Miscommunication (uri, qi, msg) ->
          return (Error (miscommunication uri qi msg))
       | exn -> fail exn)

  exception Client_error

  let catch_response_etc client_error f =
    catch
      (fun () -> f () >|= fun y -> Ok y)
      (function
       | Client_error ->
          (match !client_error with
           | Some err -> return (Error err)
           | None -> assert false)
       | Miscommunication (uri, qi, msg) ->
          return (Error (miscommunication uri qi msg))
       | exn -> fail exn)

  module Response = struct
    type ('b, +'m) t = query * C.Param.t array * 'b Caqti_type.t

    let returned_count _ = return (Error `Unsupported)
    let affected_count _ = return (Error `Unsupported)

    let rec decode_field' : type a. a Type.Field.t -> _ -> a * int = function
     | Type.Bool -> fun (tup, i) -> (C.Tuple.bool i tup, i + 1)
     | Type.Int -> fun (tup, i) -> (C.Tuple.int i tup, i + 1)
     | Type.Int32 -> fun (tup, i) -> (C.Tuple.int32 i tup, i + 1)
     | Type.Int64 -> fun (tup, i) -> (C.Tuple.int64 i tup, i + 1)
     | Type.Float -> fun (tup, i) -> (C.Tuple.float i tup, i + 1)
     | Type.String -> fun (tup, i) -> (C.Tuple.string i tup, i + 1)
     | Type.Pday -> fun (tup, i) -> (tuple_pdate i tup, i + 1)
     | Type.Ptime -> fun (tup, i) -> (tuple_ptime i tup, i + 1)
     | ft -> fun (tup, i) ->
        (match Type.Field.coding ft with
         | None -> (* FIXME: error reporting *)
            Printf.ksprintf failwith "Unsupported field type %s."
              (Type.Field.to_string ft)
         | Some (Type.Field.Coding coding) ->
            let (y, i) = decode_field' coding.rep (tup, i) in
            (coding.decode y, i))

    let rec decode' : type a. a Type.t -> _ -> a * int =
      (function
       | Type.Unit -> fun (_, i) -> ((), i)
       | Type.Field ft -> decode_field' ft
       | Type.Option t -> fun (tup, i) ->
          let j = i + Type.length t in
          let rec all_null i j =
            if i = j then true else
            if not (C.Tuple.is_null i tup) then false else
            all_null (i + 1) j
          in
          if all_null i j then (None, j) else
          let y, k = decode' t (tup, i) in
          assert (j = k);
          (Some y, j)
       | Type.Tup2 (t0, t1) -> fun (tup, i) ->
          let x0, i = decode' t0 (tup, i) in
          let x1, i = decode' t1 (tup, i) in
          (x0, x1), i
       | Type.Tup3 (t0, t1, t2) -> fun (tup, i) ->
          let x0, i = decode' t0 (tup, i) in
          let x1, i = decode' t1 (tup, i) in
          let x2, i = decode' t2 (tup, i) in
          (x0, x1, x2), i
       | Type.Tup4 (t0, t1, t2, t3) -> fun (tup, i) ->
          let x0, i = decode' t0 (tup, i) in
          let x1, i = decode' t1 (tup, i) in
          let x2, i = decode' t2 (tup, i) in
          let x3, i = decode' t3 (tup, i) in
          (x0, x1, x2, x3), i
       | Type.Custom {rep; decode = g; _} ->
          fun (tup, i) -> let y, j = decode' rep (tup, i) in (g y, j))

    let decode rt tup =
      let y, j = decode' rt (tup, 0) in
      assert (j = C.Tuple.length tup);
      y

    let exec (q, ps, _) =
      catch_response (fun () -> C.exec q ps)

    let find (q, ps, rt) =
      catch_response (fun () -> C.find q (decode rt) ps)

    let find_opt (q, ps, rt) =
      catch_response (fun () -> C.find_opt q (decode rt) ps)

    let fold f (q, ps, rt) acc =
      catch_response (fun () -> C.fold q (fun x -> f (decode rt x)) ps acc)

    let fold_s f (q, ps, rt) acc =
      let client_error = ref None in
      let aux x acc =
        f (decode rt x) acc >>=
        (function
         | Ok acc' -> return acc'
         | Error err -> client_error := Some err; fail Client_error)
      in
      catch_response_etc client_error
        (fun () -> C.fold_s q aux ps acc)

    let iter_s f (q, ps, rt) =
      let client_error = ref None in
      let aux x =
        f (decode rt x) >>=
        (function
         | Ok () -> return ()
         | Error err -> client_error := Some err; fail Client_error)
      in
      catch_response_etc client_error (fun () -> C.iter_s q aux ps)

  end

  let rec encode_field :
      type a. a Type.Field.t -> a -> C.Param.t array -> int -> int =
      fun ft x a i ->
    (match ft with
     | Type.Bool -> a.(i) <- C.Param.bool x; i + 1
     | Type.Int -> a.(i) <- C.Param.int x; i + 1
     | Type.Int32 -> a.(i) <- C.Param.int32 x; i + 1
     | Type.Int64 -> a.(i) <- C.Param.int64 x; i + 1
     | Type.Float -> a.(i) <- C.Param.float x; i + 1
     | Type.String -> a.(i) <- C.Param.string x; i + 1
     | Type.Pday -> a.(i) <- param_pdate x; i + 1
     | Type.Ptime -> a.(i) <- param_ptime x; i + 1
     | ft ->
        (match Type.Field.coding ft with
         | None -> (* FIXME: error reporting *)
            Printf.ksprintf failwith "Unsupported field type %s."
              (Type.Field.to_string ft)
         | Some (Type.Field.Coding coding) ->
            let y = coding.encode x in
            encode_field coding.rep y a i))

  let rec encode :
      type a. a Type.t -> a -> C.Param.t array -> int -> int = fun t x a i ->
    (match t with
     | Type.Unit -> i
     | Type.Field ft -> encode_field ft x a i
     | Type.Option t ->
        (match x with
         | Some x -> encode t x a i
         | None -> i + Type.length t)
     | Type.Tup2 (t0, t1) ->
        let x0, x1 = x in
        i |> encode t0 x0 a |> encode t1 x1 a
     | Type.Tup3 (t0, t1, t2) ->
        let x0, x1, x2 = x in
        i |> encode t0 x0 a |> encode t1 x1 a
          |> encode t2 x2 a
     | Type.Tup4 (t0, t1, t2, t3) ->
        let x0, x1, x2, x3 = x in
        i |> encode t0 x0 a |> encode t1 x1 a
          |> encode t2 x2 a |> encode t3 x3 a
     | Type.Custom {rep; encode = f; _} -> encode rep (f x) a i)

  let driver_info = C.driver_info

  type cache_entry = {
    query: query;
    param_order: int array option;
  }

  let cache = Hashtbl.create 19

  let call ~(f : ('b, 'm) Response.t -> ('c, 'e) result io) req x =
    let pt = Caqti_request.param_type req in
    let ps = Array.make (Type.length pt) C.Param.null in
    let _ = encode pt x ps 0 in
    let rt = Caqti_request.row_type req in
    catch
      (fun () ->
        let {query; param_order} =
          (match Caqti_request.query_id req with
           | None ->
              let templ = Caqti_request.query_template req driver_info in
              let query =
                Caqti_query.oneshot_full (template_query_string templ) in
              let param_order = template_param_order templ driver_info in
              {query; param_order}
           | Some id ->
              (try Hashtbl.find cache id with
               | Not_found ->
                  let templ = Caqti_request.query_template req driver_info in
                  let query =
                    Caqti_query.prepare_full (template_query_string templ) in
                  let param_order = template_param_order templ driver_info in
                  Hashtbl.add cache id {query; param_order};
                  {query; param_order})) in
        let ps =
          (match param_order with
           | None -> ps
           | Some param_order -> Array.map (fun i -> ps.(i)) param_order) in
        f (query, ps, rt))
      (function
       | Prepare_failed (uri, qi, msg) ->
          return (Error (prepare_failed uri qi msg))
       | Execute_failed (uri, qi, msg) ->
          return (Error (execute_failed uri qi msg))
       | exn -> fail exn)

  let exec req p = call ~f:Response.exec req p
  let find req p = call ~f:Response.find req p
  let find_opt req p = call ~f:Response.find_opt req p
  let fold req f p acc = call ~f:(fun rsp -> Response.fold f rsp acc) req p
  let fold_s req f p acc = call ~f:(fun rsp -> Response.fold_s f rsp acc) req p
  let iter_s req f p = call ~f:(fun rsp -> Response.iter_s f rsp) req p

  let start = C.start
  let commit = C.commit
  let rollback = C.rollback
  let disconnect = C.disconnect
  let validate = C.validate
  let check = C.check

end
