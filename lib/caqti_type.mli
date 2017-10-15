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

(** Type descriptor for a span of database columns. *)

type _ t =
  | Unit : unit t
  | Bool : bool t
  | Int : int t
  | Int32 : int32 t
  | Int64 : int64 t
  | Float : float t
  | String : string t
  | Pdate : int t
  | Ptime : Ptime.t t
  | Option : 'a t -> 'a option t
  | [] : unit Caqti_tuple.t t
  | (::) : 'a t * 'b Caqti_tuple.t t -> ('a * 'b) Caqti_tuple.t t
  | Custom : {rep: 'b t; encode: 'a -> 'b; decode: 'b -> 'a} -> 'a t

val length : 'a t -> int

val pp_hum : Format.formatter -> 'a t -> unit

val to_string_hum : 'a t -> string
