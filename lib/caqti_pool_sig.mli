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

(** Resource pool signature. *)

(** The signature of pools of reusable resources.  We use it to keep pools of
    open database connections.  A generic implementation is found in
    {!Caqti_pool}, and instantiations are available for each cooperative
    thread monad. *)
module type S = sig

  type 'a io

  type 'a t

  val create :
        ?max_size: int ->
        ?check: ('a -> (bool -> unit) -> unit) ->
        ?validate: ('a -> bool io) ->
        (unit -> 'a io) -> ('a -> unit io) -> 'a t
  (** This pool construction function should be considered internal. *)

  val size : 'a t -> int
  (** The current number of allocations in the pool. *)

  val use : ?priority: float -> ('a -> 'b io) -> 'a t -> 'b io
  (** [use f pool] calls [f] on a resource drawn from [pool], handing back the
      resource to the pool when [f] exits.

      @param priority
        Users passing a high value for priority gets run before lower priority
        users. The default priority is [0.0]. *)

  val drain : 'a t -> unit io
  (** [drain pool] closes all resources in [pool]. The pool is still usable, as
      new resources will be created on demand. *)

end
