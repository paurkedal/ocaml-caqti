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

(** Type descriptors for requests. *)

type ('a, 'b, +'m) t = 'a Row_type.t * 'b Row_type.t * 'm Row_mult.t

module Infix : sig

  val ( -->. ) :
    'a Row_type.t -> unit Row_type.t -> ('a, unit, Row_mult.zero) t

  val ( -->! ) :
    'a Row_type.t -> 'b Row_type.t -> ('a, 'b, Row_mult.one) t

  val ( -->? ) :
    'a Row_type.t -> 'b Row_type.t -> ('a, 'b, Row_mult.zero_or_one) t

  val ( -->* ) :
    'a Row_type.t -> 'b Row_type.t -> ('a, 'b, Row_mult.zero_or_more) t

end

val param_type : ('a, _, _) t -> 'a Row_type.t
val row_type : (_, 'b, _) t -> 'b Row_type.t
val row_mult : (_, _, 'm) t -> 'm Row_mult.t
