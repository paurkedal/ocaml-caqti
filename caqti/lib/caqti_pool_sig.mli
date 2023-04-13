(* Copyright (C) 2017--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Resource pool signature. *)

module type S = sig

  type +'a future

  type ('a, +'e) t

  val size : ('a, 'e) t -> int
  (** [size pool] is the current number of open resources in [pool]. *)

  val use :
    ?priority: float ->
    ('a -> ('b, 'e) result future) -> ('a, 'e) t -> ('b, 'e) result future
  (** [use f pool] calls [f] on a resource drawn from [pool], handing back the
      resource to the pool when [f] exits.

      @param priority
        Requests for the resource are handled in decreasing order of priority.
        The default priority is [0.0]. *)

  val drain : ('a, 'e) t -> unit future
  (** [drain pool] closes all resources in [pool]. The pool is still usable, as
      new resources will be created on demand. *)

end
