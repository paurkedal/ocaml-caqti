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

(** (v2) Request specification. *)

type template =
  | L of string
  | P of int
  | S of template list

type ('a, 'b, +'m) t constraint 'm = [< `Zero | `One | `Many]

val create :
  ?oneshot: bool ->
  'a Caqti_type.t -> 'b Caqti_type.t -> 'm Caqti_mult.t ->
  (Caqti_driver_info.t -> template) -> ('a, 'b, 'm) t

val create_p :
  ?oneshot: bool ->
  'a Caqti_type.t -> 'b Caqti_type.t -> 'm Caqti_mult.t ->
  (Caqti_driver_info.t -> string) -> ('a, 'b, 'm) t

val params_type : ('a, _, _) t -> 'a Caqti_type.t
val row_type : (_, 'b, _) t -> 'b Caqti_type.t
val row_mult : (_, _, 'm) t -> 'm Caqti_mult.t

val query_id : ('a, 'b, 'm) t -> int option
val query_template : ('a, 'b, 'm) t -> Caqti_driver_info.t -> template
