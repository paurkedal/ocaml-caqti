(* Copyright (C) 2024  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Database field types. *)

open Shims

type 'a t =
  | Bool : bool t
  | Int : int t
  | Int16 : int t
  | Int32 : int32 t
  | Int64 : int64 t
  | Float : float t
  | String : string t
  | Octets : string t
  | Pdate : Ptime.t t
  | Ptime : Ptime.t t
  | Ptime_span : Ptime.span t
  | Enum : string -> string t

val unify : 'a t -> 'b t -> ('a, 'b) Type.eq option

val equal_value : 'a t -> 'a -> 'a -> bool

val to_string : 'a t -> string

val pp : Format.formatter -> 'a t -> unit

val pp_value : Format.formatter -> 'a t * 'a -> unit
