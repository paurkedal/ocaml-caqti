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

open Caqti_template

let (%>) f g x = g (f x)

let empty_subst _ = raise Not_found

type linear_param =
  Linear_param : int * 'a Field_type.t * 'a -> linear_param

let linear_param_length ?(subst = empty_subst) templ =
  let templ = Query.expand subst templ in
  let rec loop : Query.t -> int -> int = function
   | L _ -> Fun.id
   | V (_, _) -> succ
   | Q _ -> succ
   | P _ -> succ
   | E _ -> assert false
   | S frags -> List_ext.fold loop frags
  in
  loop templ 0

let nonlinear_param_length ?(subst = empty_subst) templ =
  let templ = Query.expand subst templ in
  let rec loop : Query.t -> int -> int = function
   | L _ -> Fun.id
   | V _ -> Fun.id
   | Q _ -> Fun.id
   | P n -> max (n + 1)
   | E _ -> assert false
   | S frags -> List_ext.fold loop frags
  in
  loop templ 0

let linear_param_order ?(subst = empty_subst) templ =
  let templ = Query.expand subst templ in
  let a = Array.make (nonlinear_param_length templ) [] in
  let rec loop : Query.t -> _ -> _ = function
   | L _ -> Fun.id
   | V (t, v) ->
      fun (j, params) ->
        (j + 1, Linear_param (j, t, v) :: params)
   | Q s ->
      fun (j, params) ->
        (j + 1, Linear_param (j, Field_type.String, s) :: params)
   | P i -> fun (j, params) -> a.(i) <- j :: a.(i); (j + 1, params)
   | E _ -> assert false
   | S frags -> List_ext.fold loop frags
  in
  let _, params = loop templ (0, []) in
  (Array.to_list a, List.rev params)

let linear_query_string ?(subst = empty_subst) templ =
  let templ = Query.expand subst templ in
  let buf = Buffer.create 64 in
  let rec loop : Query.t -> unit = function
   | L s -> Buffer.add_string buf s
   | Q _ | V _ | P _ -> Buffer.add_char buf '?'
   | E _ -> assert false
   | S frags -> List.iter loop frags
  in
  loop templ;
  Buffer.contents buf

let raise_encode_missing ~uri ~field_type () =
  raise (Caqti_error.Exn (Caqti_error.encode_missing ~uri ~field_type ()))
let raise_encode_rejected ~uri ~typ msg =
  raise (Caqti_error.Exn (Caqti_error.encode_rejected ~uri ~typ msg))
let raise_encode_failed ~uri ~typ msg =
  raise (Caqti_error.Exn (Caqti_error.encode_failed ~uri ~typ msg))
let raise_decode_missing ~uri ~field_type () =
  raise (Caqti_error.Exn (Caqti_error.decode_missing ~uri ~field_type ()))
let raise_decode_rejected ~uri ~typ msg =
  raise (Caqti_error.Exn (Caqti_error.decode_rejected ~uri ~typ msg))
let raise_response_failed ~uri ~query msg =
  raise (Caqti_error.Exn (Caqti_error.response_failed ~uri ~query msg))
let raise_response_rejected ~uri ~query msg =
  raise (Caqti_error.Exn (Caqti_error.response_rejected ~uri ~query msg))

type 'a field_encoder = {
  write_value: 'b. uri: Uri.t -> 'b Field_type.t -> 'b -> 'a -> 'a;
  write_null: 'b. uri: Uri.t -> 'b Field_type.t -> 'a -> 'a;
}
constraint 'e = [> `Encode_rejected of Caqti_error.coding_error]

let rec encode_null_param : type a. uri: _ -> _ -> a Row_type.t -> _ =
  fun ~uri f ->
  (function
   | Field ft -> f.write_null ~uri ft
   | Option t -> encode_null_param ~uri f t
   | Product (_, _, ts) -> encode_null_param_of_product ~uri f ts
   | Annot (_, t) -> encode_null_param ~uri f t)
and encode_null_param_of_product
  : type a i. uri: _ -> _ -> (i, a) Row_type.product -> _  =
  fun ~uri f ->
  (function
   | Proj_end -> Fun.id
   | Proj (t, _, ts) ->
      encode_null_param ~uri f t %>
      encode_null_param_of_product ~uri f ts)

let reject_encode ~uri ~typ msg =
  let msg = Caqti_error.Msg msg in
  raise_encode_rejected ~uri ~typ msg

let rec encode_param
  : type a. uri: _ -> _ -> a Row_type.t -> a -> 'b -> 'b =
  fun ~uri f typ ->
  (match typ with
   | Field ft ->
      (try f.write_value ~uri ft with
       | Row_type.Reject msg -> reject_encode ~uri ~typ msg)
   | Option t ->
      let encode_none = encode_null_param ~uri f t in
      let encode_some = encode_param ~uri f t in
      (function None -> encode_none | Some x -> encode_some x)
   | Product (_, _, ts) ->
      (try encode_param_of_product ~uri f ts with
       | Row_type.Reject msg -> reject_encode ~uri ~typ msg)
   | Annot (_, t) -> encode_param ~uri f t)
and encode_param_of_product
  : type a i. uri: _ -> _ -> (i, a) Row_type.product -> a -> 'b -> 'b =
  fun ~uri f ->
  (function
   | Proj_end -> fun _ acc -> acc
   | Proj (t, p, ts) ->
      let encode_t = encode_param ~uri f t in
      let encode_ts = encode_param_of_product ~uri f ts in
      fun x acc -> encode_t (p x) acc |> encode_ts x)

type 'a field_decoder = {
  read_value: 'b. uri: Uri.t -> 'b Field_type.t -> 'a -> 'b * 'a;
  skip_null: int -> 'a -> 'a option;
}
constraint 'e = [> `Decode_rejected of Caqti_error.coding_error]

let reject_decode ~uri ~typ msg =
  let msg = Caqti_error.Msg msg in
  raise_decode_rejected ~uri ~typ msg

let rec decode_row : type a. uri: _ -> _ -> a Row_type.t -> 'b -> a * 'b =
  fun ~uri f typ ->
  (match typ with
   | Field ft ->
      f.read_value ~uri ft
   | Option t ->
      let decode_t = decode_row ~uri f t in
      let skip_null = f.skip_null (Row_type.length t) in
      fun acc ->
        (match skip_null acc with
         | Some acc -> (None, acc)
         | None ->
            let x, acc = decode_t acc in
            (Some x, acc))
   | Product (_, intro, Proj_end) ->
      fun acc ->
        (match intro with
         | Ok y -> (y, acc)
         | Error msg -> reject_decode ~uri ~typ msg)
   | Product (_, intro, Proj (t1, _, Proj (t2, _, Proj_end))) ->
      (* Optimization *)
      let decode_t1 = decode_row ~uri f t1 in
      let decode_t2 = decode_row ~uri f t2 in
      fun acc ->
        let x1, acc = decode_t1 acc in
        let x2, acc = decode_t2 acc in
        (match intro x1 x2 with
         | Ok y -> (y, acc)
         | Error msg -> reject_decode ~uri ~typ msg
         | exception Row_type.Reject msg -> reject_decode ~uri ~typ msg)
   | Product (_, intro, Proj (t1, _, Proj (t2, _, Proj (t3, _, Proj_end)))) ->
      (* Optimization *)
      let decode_t1 = decode_row ~uri f t1 in
      let decode_t2 = decode_row ~uri f t2 in
      let decode_t3 = decode_row ~uri f t3 in
      fun acc ->
        let x1, acc = decode_t1 acc in
        let x2, acc = decode_t2 acc in
        let x3, acc = decode_t3 acc in
        (match intro x1 x2 x3 with
         | Ok y -> (y, acc)
         | Error msg -> reject_decode ~uri ~typ msg
         | exception Row_type.Reject msg -> reject_decode ~uri ~typ msg)
   | Product (_, intro,
        Proj (t1, _, Proj (t2, _, Proj (t3, _, Proj (t4, _, Proj_end))))) ->
      (* Optimization *)
      let decode_t1 = decode_row ~uri f t1 in
      let decode_t2 = decode_row ~uri f t2 in
      let decode_t3 = decode_row ~uri f t3 in
      let decode_t4 = decode_row ~uri f t4 in
      fun acc ->
        let x1, acc = decode_t1 acc in
        let x2, acc = decode_t2 acc in
        let x3, acc = decode_t3 acc in
        let x4, acc = decode_t4 acc in
        (match intro x1 x2 x3 x4 with
         | Ok y -> (y, acc)
         | Error msg -> reject_decode ~uri ~typ msg
         | exception Row_type.Reject msg -> reject_decode ~uri ~typ msg)
   | Product (_, intro, ts) as typ ->
      let rec loop
        : type a i. (i, a) Row_type.product -> i -> _ -> a * _ =
        (function
         | Proj_end ->
            fun intro acc ->
              (match intro with
               | Ok y -> (y, acc)
               | Error msg -> reject_decode ~uri ~typ msg)
         | Proj (t, _, ts) ->
            let decode_t = decode_row ~uri f t in
            let decode_ts = loop ts in
            fun intro acc ->
              let x, acc = decode_t acc in
              decode_ts (intro x) acc)
      in
      (try loop ts intro with
       | Row_type.Reject msg -> reject_decode ~uri ~typ msg)
   | Annot (_, t0) ->
      decode_row ~uri f t0)

let fresh_name_generator prefix =
  let c = ref 0 in
  fun () -> incr c; Printf.sprintf "%s%d" prefix !c
