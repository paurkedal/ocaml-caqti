(* Copyright (C) 2025  Petter A. Urkedal <paurkedal@gmail.com>
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

(* equal *)

let rec equal_poly : type a. a Row_type.t -> a -> a -> bool =
  (function
   | Field ft ->
      Field_type.equal_value ft
   | Option t ->
      let equal_t = equal_poly t in
      fun x y ->
        (match x, y with
         | None, None -> true
         | Some x, Some y -> equal_t x y
         | None, Some _ | Some _, None -> false)
   | Product (_, _, pt) -> equal_product pt
   | Annot (_, t) -> equal_poly t)

and equal_product : type i a. (i, a) Row_type.product -> a -> a -> bool =
  (function
   | Proj_end -> fun _ _ -> true
   | Proj (t, p, pt) ->
      let equal_t = equal_poly t in
      let equal_pt = equal_product pt in
      fun x y -> equal_t (p x) (p y) && equal_pt x y)

let equal (t : _ Row_type.t) = equal_poly t

(* pp *)

type pp_state = {
  mutable field_num: int;
}

let pp_field_sep state ppf () =
  if state.field_num > 0 then
    begin
      Format.pp_print_char ppf ',';
      Format.pp_print_space ppf ()
    end;
  state.field_num <- state.field_num + 1

let pp_rep_lit n lit state =
  fun ppf () ->
    for _ = 1 to n do
      pp_field_sep state ppf ();
      Format.pp_print_string ppf lit
    done

let rec pp_poly
  : type a. a Row_type.t -> pp_state -> Format.formatter -> a -> unit =
  (function
   | Field ft ->
      fun state ppf x ->
        pp_field_sep state ppf ();
        Field_type.pp_value ppf (ft, x)
   | Option t ->
      let pp_t = pp_poly t in
      let length_t = Row_type.length t in
      fun state ->
        let case_none = pp_rep_lit length_t "NONE" state in
        let case_some = pp_t state in
        fun ppf ->
          (function None -> case_none ppf () | Some x -> case_some ppf x)
   | Product (_, _, pt) -> pp_product pt
   | Annot (`Redacted, t) ->
      let length_t = Row_type.length t in
      fun state ppf _ -> pp_rep_lit length_t "#redacted#" state ppf ())

and pp_product
  : type i a. (i, a) Row_type.product -> pp_state ->
    Format.formatter -> a -> unit =
  (function
   | Proj_end -> fun _state _ppf _x -> ()
   | Proj (t, p, pt) ->
      let pp_t = pp_poly t in
      let pp_pt = pp_product pt in
      fun state ->
        let pp_t_state = pp_t state in
        let pp_pt_state = pp_pt state in
        fun ppf x ->
          pp_t_state ppf (p x);
          pp_pt_state ppf x)

let pp (t : _ Row_type.t) = pp_poly t {field_num = 1}
