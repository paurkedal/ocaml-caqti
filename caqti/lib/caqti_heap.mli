(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** {b Internal:} Min-heap.

    This is a very simple implementation deemed sufficient for {!Caqti_pool}.
    There are more optimal algorithms if you need the best performance on larger
    heaps. *)

module type S = sig
  type t
  type elt

  val empty : t
  val is_empty : t -> bool
  val card : t -> int
  val push : elt -> t -> t
  val merge : t -> t -> t
  val pop_e : t -> elt * t
end

module Make (Elt : Set.OrderedType) : S with type elt = Elt.t
