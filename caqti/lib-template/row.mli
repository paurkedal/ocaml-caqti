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

val equal : 'a Row_type.t -> 'a -> 'a -> bool
(** [equal_value t] is the equality predicate for values of row type [t]. *)

val pp : 'a Row_type.t -> Format.formatter -> 'a -> unit
(** [pp_value ppf (t, v)] prints a human representation of [v] given the type
    descriptor [t]. This function is meant for debugging; the output is neither
    guaranteed to be consistent across releases nor to contain a complete record
    of the data. *)
