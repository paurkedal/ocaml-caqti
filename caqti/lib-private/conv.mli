(* Copyright (C) 2019--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Miscellaneous conversions. *)

val datetuple_of_iso8601 : string -> int * int * int
val iso8601_of_datetuple : int * int * int -> string

val ptime_of_rfc3339_utc : string -> (Ptime.t, string) result

val pdate_of_iso8601 : string -> (Ptime.t, string) result
val iso8601_of_pdate : Ptime.t -> string
