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

(** (v2) Type descriptors for fields and tuples. *)

type 'a field = ..

module Field : sig
  type 'a t = 'a field

  type ex = Ex : 'a t -> ex

  type _ coding = Coding : {
    rep: 'b t;
    encode: 'a -> ('b, string) result;
    decode: 'b -> ('a, string) result;
  } -> 'a coding

  type get_coding = {get_coding: 'a. Caqti_driver_info.t -> 'a t -> 'a coding}

  val define_coding : 'a field -> get_coding -> unit

  val coding : Caqti_driver_info.t -> 'a field -> 'a coding option

  val to_string : 'a t -> string
end

type _ field +=
  | Bool : bool field
  | Int : int field
  | Int32 : int32 field
  | Int64 : int64 field
  | Float : float field
  | String : string field
  | Octets : string field
  | Pday : int field
  | Ptime : Ptime.t field

type _ t = private
  | Unit : unit t
  | Field : 'a field -> 'a t
  | Option : 'a t -> 'a option t
  | Tup2 : 'a t * 'b t -> ('a * 'b) t
  | Tup3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Tup4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
  | Custom : {
      rep: 'b t;
      encode: 'a -> ('b, string) result;
      decode: 'b -> ('a, string) result;
    } -> 'a t

type ex = Ex : 'a t -> ex

val length : 'a t -> int

val pp_hum : Format.formatter -> 'a t -> unit
val pp_ex_hum : Format.formatter -> ex -> unit

val to_string_hum : 'a t -> string

val unit : unit t
val field : 'a field -> 'a t
val option : 'a t -> 'a option t
val tup2 : 'a t -> 'b t -> ('a * 'b) t
val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
val custom :
  encode: ('a -> ('b, string) result) ->
  decode: ('b -> ('a, string) result) ->
  'b t -> 'a t

val bool : bool t
val int : int t
val int32 : int32 t
val int64 : int64 t
val float : float t
val string : string t
val octets : string t
val pday : int t
val ptime : Ptime.t t
