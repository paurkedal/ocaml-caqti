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

let (%>) f g x = g (f x)

let empty_subst _ = None

type linear_param =
  Linear_param : int * 'a Caqti_type.Field.t * 'a -> linear_param

let linear_param_length ?(subst = empty_subst) templ =
  let templ = Caqti_template.Query.expand subst templ in
  let rec loop = function
   | Caqti_query.L _ -> Fun.id
   | Caqti_query.V (_, _) -> succ
   | Caqti_query.Q _ -> succ
   | Caqti_query.P _ -> succ
   | Caqti_query.E _ -> assert false
   | Caqti_query.S frags -> List_ext.fold loop frags
  in
  loop templ 0

let nonlinear_param_length ?(subst = empty_subst) templ =
  let templ = Caqti_template.Query.expand subst templ in
  let rec loop = function
   | Caqti_query.L _ -> Fun.id
   | Caqti_query.V _ -> Fun.id
   | Caqti_query.Q _ -> Fun.id
   | Caqti_query.P n -> max (n + 1)
   | Caqti_query.E _ -> assert false
   | Caqti_query.S frags -> List_ext.fold loop frags
  in
  loop templ 0

let linear_param_order ?(subst = empty_subst) templ =
  let templ = Caqti_template.Query.expand subst templ in
  let a = Array.make (nonlinear_param_length templ) [] in
  let rec loop = function
   | Caqti_query.L _ -> Fun.id
   | Caqti_query.V (t, v) ->
      fun (j, params) ->
        (j + 1, Linear_param (j, t, v) :: params)
   | Caqti_query.Q s ->
      fun (j, params) ->
        (j + 1, Linear_param (j, Caqti_type.Field.String, s) :: params)
   | Caqti_query.P i -> fun (j, params) -> a.(i) <- j :: a.(i); (j + 1, params)
   | Caqti_query.E _ -> assert false
   | Caqti_query.S frags -> List_ext.fold loop frags
  in
  let _, params = loop templ (0, []) in
  (Array.to_list a, List.rev params)

let linear_query_string ?(subst = empty_subst) templ =
  let templ = Caqti_template.Query.expand subst templ in
  let buf = Buffer.create 64 in
  let rec loop = function
   | Caqti_query.L s -> Buffer.add_string buf s
   | Caqti_query.Q _ | Caqti_query.V _ | Caqti_query.P _ ->
      Buffer.add_char buf '?'
   | Caqti_query.E _ -> assert false
   | Caqti_query.S frags -> List.iter loop frags
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
  write_value: 'b. uri: Uri.t -> 'b Caqti_type.Field.t -> 'b -> 'a -> 'a;
  write_null: 'b. uri: Uri.t -> 'b Caqti_type.Field.t -> 'a -> 'a;
}
constraint 'e = [> `Encode_rejected of Caqti_error.coding_error]

let rec encode_null_param : type a. uri: _ -> _ -> a Caqti_type.t -> _ =
  fun ~uri f ->
  let open Caqti_type in
  (function
   | Field ft -> f.write_null ~uri ft
   | Option t -> encode_null_param ~uri f t
   | Product (_, _, ts) ->
      let rec loop : type a i. (a, i) product -> _ = function
       | Proj_end -> Fun.id
       | Proj (t, _, ts) -> encode_null_param ~uri f t %> loop ts
      in
      loop ts
   | Annot (_, t) -> encode_null_param ~uri f t)

let reject_encode ~uri ~typ msg =
  let msg = Caqti_error.Msg msg in
  raise_encode_rejected ~uri ~typ msg

let rec encode_param
    : type a. uri: _ -> _ -> a Caqti_type.t -> a -> 'b -> 'b =
  fun ~uri f ->
  let open Caqti_type in
  (function
   | Field ft -> f.write_value ~uri ft
   | Option t ->
      (function
       | None -> encode_null_param ~uri f t
       | Some x -> encode_param ~uri f t x)
   | Product (_, _, ts) as typ ->
      let rec loop : type i. (a, i) product -> _ = function
       | Proj_end -> fun _ acc -> acc
       | Proj (t, p, ts) ->
          let encode_t = encode_param ~uri f t in
          let encode_ts = loop ts in
          fun x acc -> encode_t (p x) acc |> encode_ts x
      in
      (try loop ts with
       | Caqti_type.Reject msg -> reject_encode ~uri ~typ msg)
   | Annot (_, t) -> encode_param ~uri f t)

type 'a field_decoder = {
  read_value: 'b. uri: Uri.t -> 'b Caqti_type.Field.t -> 'a -> 'b * 'a;
  skip_null: int -> 'a -> 'a option;
}
constraint 'e = [> `Decode_rejected of Caqti_error.coding_error]

let reject_decode ~uri ~typ msg =
  let msg = Caqti_error.Msg msg in
  raise_decode_rejected ~uri ~typ msg

let rec decode_row
    : type a. uri: _ -> _ -> a Caqti_type.t -> _ -> a * _ =
  fun ~uri f ->
  let open Caqti_type in
  (function
   | Field ft ->
      f.read_value ~uri ft
   | Option t ->
      let decode_t = decode_row ~uri f t in
      fun acc ->
        (match f.skip_null (Caqti_type.length t) acc with
         | Some acc -> (None, acc)
         | None ->
            let x, acc = decode_t acc in
            (Some x, acc))
   | Product (_, intro, Proj_end) ->
      fun acc -> (intro, acc)
   | Product (_, intro, Proj (t1, _, Proj (t2, _, Proj_end)))
        as typ -> (* optimization *)
      let decode_t1 = decode_row ~uri f t1 in
      let decode_t2 = decode_row ~uri f t2 in
      fun acc ->
        let x1, acc = decode_t1 acc in
        let x2, acc = decode_t2 acc in
        (try (intro x1 x2, acc) with
         | Caqti_type.Reject msg -> reject_decode ~uri ~typ msg)
   | Product (_, intro, Proj (t1, _, Proj (t2, _, Proj (t3, _, Proj_end))))
        as typ -> (* optimization *)
      let decode_t1 = decode_row ~uri f t1 in
      let decode_t2 = decode_row ~uri f t2 in
      let decode_t3 = decode_row ~uri f t3 in
      fun acc ->
        let x1, acc = decode_t1 acc in
        let x2, acc = decode_t2 acc in
        let x3, acc = decode_t3 acc in
        (try (intro x1 x2 x3, acc) with
         | Caqti_type.Reject msg -> reject_decode ~uri ~typ msg)
   | Product (_, intro,
        Proj (t1, _, Proj (t2, _, Proj (t3, _, Proj (t4, _, Proj_end)))))
        as typ -> (* optimization *)
      let decode_t1 = decode_row ~uri f t1 in
      let decode_t2 = decode_row ~uri f t2 in
      let decode_t3 = decode_row ~uri f t3 in
      let decode_t4 = decode_row ~uri f t4 in
      fun acc ->
        let x1, acc = decode_t1 acc in
        let x2, acc = decode_t2 acc in
        let x3, acc = decode_t3 acc in
        let x4, acc = decode_t4 acc in
        (try (intro x1 x2 x3 x4, acc) with
         | Caqti_type.Reject msg -> reject_decode ~uri ~typ msg)
   | Product (_, intro, ts) as typ ->
      let rec loop : type a i. (a, i) product -> i -> _ -> a * _ =
        (function
         | Proj_end ->
            fun intro acc -> (intro, acc)
         | Proj (t, _, ts) ->
            let decode_t = decode_row ~uri f t in
            let decode_ts = loop ts in
            fun intro acc ->
              let x, acc = decode_t acc in
              decode_ts (intro x) acc)
      in
      (try loop ts intro with
       | Caqti_type.Reject msg -> reject_decode ~uri ~typ msg)
   | Annot (_, t0) ->
      decode_row ~uri f t0)
