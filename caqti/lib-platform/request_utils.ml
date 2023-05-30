(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

let (|>?) = Result.bind
let (%>?) f g x = match f x with Ok y -> g y | Error _ as r -> r

let no_env _ = raise Not_found

let linear_param_length ?(env = no_env) templ =
  let templ = Caqti_query.expand env templ in
  let rec loop = function
   | Caqti_query.L _ -> Fun.id
   | Caqti_query.Q _ -> succ
   | Caqti_query.P _ -> succ
   | Caqti_query.E _ -> assert false
   | Caqti_query.S frags -> List_ext.fold loop frags
  in
  loop templ 0

let nonlinear_param_length ?(env = no_env) templ =
  let templ = Caqti_query.expand env templ in
  let rec loop = function
   | Caqti_query.L _ -> Fun.id
   | Caqti_query.Q _ -> Fun.id
   | Caqti_query.P n -> max (n + 1)
   | Caqti_query.E _ -> assert false
   | Caqti_query.S frags -> List_ext.fold loop frags
  in
  loop templ 0

let linear_param_order ?(env = no_env) templ =
  let templ = Caqti_query.expand env templ in
  let a = Array.make (nonlinear_param_length templ) [] in
  let rec loop = function
   | Caqti_query.L _ -> Fun.id
   | Caqti_query.Q s -> fun (j, quotes) -> (j + 1, (j, s) :: quotes)
   | Caqti_query.P i -> fun (j, quotes) -> a.(i) <- j :: a.(i); (j + 1, quotes)
   | Caqti_query.E _ -> assert false
   | Caqti_query.S frags -> List_ext.fold loop frags
  in
  let _, quotes = loop templ (0, []) in
  (Array.to_list a, List.rev quotes)

let linear_query_string ?(env = no_env) templ =
  let templ = Caqti_query.expand env templ in
  let buf = Buffer.create 64 in
  let rec loop = function
   | Caqti_query.L s -> Buffer.add_string buf s
   | Caqti_query.Q _ | Caqti_query.P _ -> Buffer.add_char buf '?'
   | Caqti_query.E _ -> assert false
   | Caqti_query.S frags -> List.iter loop frags
  in
  loop templ;
  Buffer.contents buf

type ('a, 'e) field_encoder = {
  write_value:
    'b. uri: Uri.t -> 'b Caqti_type.Field.t -> 'b -> 'a -> ('a, 'e) result;
  write_null:
    'b. uri: Uri.t -> 'b Caqti_type.Field.t -> 'a -> ('a, 'e) result;
}
constraint 'e = [> `Encode_rejected of Caqti_error.coding_error]

let rec encode_null_param
    : type a. uri: _ -> _ -> a Caqti_type.t -> _ =
  fun ~uri f ->
  let open Caqti_type in
  (function
   | Unit -> fun acc -> Ok acc
   | Field ft -> f.write_null ~uri ft
   | Option t -> encode_null_param ~uri f t
   | Product (_, ts) ->
      let rec loop : type a i. (a, i) product -> _ = function
       | [] -> Result.ok
       | (t, _) :: ts -> encode_null_param ~uri f t %>? loop ts
      in
      loop ts
   | Custom {rep; _} -> encode_null_param ~uri f rep
   | Annot (_, t) -> encode_null_param ~uri f t)

let rec encode_param
    : type a. uri: _ -> _ -> a Caqti_type.t -> a -> _ =
  fun ~uri f ->
  let open Caqti_type in
  (function
   | Unit -> fun () acc -> Ok acc
   | Field ft -> f.write_value ~uri ft
   | Option t ->
      (function
       | None -> encode_null_param ~uri f t
       | Some x -> encode_param ~uri f t x)
   | Product (_, ts) ->
      let rec loop : type i. (a, i) product -> _ = function
       | [] -> fun _ -> Result.ok
       | (t, p) :: ts ->
          let encode_t = encode_param ~uri f t in
          let encode_ts = loop ts in
          fun x acc -> encode_t (p x) acc |>? encode_ts x
      in
      loop ts
   | Custom {rep; encode; _} as t -> fun x acc ->
      (match encode x with
       | Ok y -> encode_param ~uri f rep y acc
       | Error msg ->
          let msg = Caqti_error.Msg msg in
          Error (Caqti_error.encode_rejected ~uri ~typ:t msg))
   | Annot (_, t) -> encode_param ~uri f t)

type ('a, 'e) field_decoder = {
  read_value:
    'b. uri: Uri.t -> 'b Caqti_type.Field.t -> 'a -> ('b * 'a, 'e) result;
  skip_null: int -> 'a -> 'a option;
}
constraint 'e = [> `Decode_rejected of Caqti_error.coding_error]

let rec decode_row
    : type a. uri: _ -> _ -> a Caqti_type.t -> _ -> (a * _, _) result =
  fun ~uri f ->
  let open Caqti_type in
  (function
   | Unit ->
      fun acc -> Ok ((), acc)
   | Field ft ->
      f.read_value ~uri ft
   | Option t ->
      let decode_t = decode_row ~uri f t in
      fun acc ->
        (match f.skip_null (Caqti_type.length t) acc with
         | Some acc -> Ok (None, acc)
         | None -> decode_t acc |> Result.map (fun (x, acc) -> (Some x, acc)))
   | Product (intro, [t1, _; t2, _]) -> (* optimization *)
      let decode_t1 = decode_row ~uri f t1 in
      let decode_t2 = decode_row ~uri f t2 in
      fun acc ->
        decode_t1 acc |>? fun (x1, acc) ->
        decode_t2 acc |>? fun (x2, acc) ->
        Ok (intro x1 x2, acc)
   | Product (intro, [t1, _; t2, _; t3, _]) -> (* optimization *)
      let decode_t1 = decode_row ~uri f t1 in
      let decode_t2 = decode_row ~uri f t2 in
      let decode_t3 = decode_row ~uri f t3 in
      fun acc ->
        decode_t1 acc |>? fun (x1, acc) ->
        decode_t2 acc |>? fun (x2, acc) ->
        decode_t3 acc |>? fun (x3, acc) ->
        Ok (intro x1 x2 x3, acc)
   | Product (intro, [t1, _; t2, _; t3, _; t4, _]) -> (* optimization *)
      let decode_t1 = decode_row ~uri f t1 in
      let decode_t2 = decode_row ~uri f t2 in
      let decode_t3 = decode_row ~uri f t3 in
      let decode_t4 = decode_row ~uri f t4 in
      fun acc ->
        decode_t1 acc |>? fun (x1, acc) ->
        decode_t2 acc |>? fun (x2, acc) ->
        decode_t3 acc |>? fun (x3, acc) ->
        decode_t4 acc |>? fun (x4, acc) ->
        Ok (intro x1 x2 x3 x4, acc)
   | Product (intro, ts) ->
      let rec loop : type a i. (a, i) product -> i -> _ -> (a * _, _) result =
        (function
         | [] ->
            fun intro acc -> Ok (intro, acc)
         | (t, _) :: ts ->
            let decode_t = decode_row ~uri f t in
            let decode_ts = loop ts in
            fun intro acc ->
              decode_t acc |>? fun (x, acc) ->
              decode_ts (intro x) acc)
      in
      loop ts intro
   | Custom {rep; decode; _} as typ ->
      let decode_rep = decode_row ~uri f rep in
      fun acc ->
        (match decode_rep acc with
         | Ok (x, acc) ->
            (match decode x with
             | Ok y -> Ok (y, acc)
             | Error msg ->
                let msg = Caqti_error.Msg msg in
                Error (Caqti_error.decode_rejected ~uri ~typ msg))
         | Error _ as r -> r)
   | Annot (_, t0) ->
      decode_row ~uri f t0)
