(* Copyright (C) 2017--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** (v1) Resource pool signature. *)

[@@@ocaml.deprecated]

(** The signature of pools of reusable resources.  We use it to keep pools of
    open database connections.  A generic implementation is found in
    {!Caqti_pool} which is used to provide [connect_pool] functions.

    This interface is based on [Lwt_pool]. *)
module type S = sig

  type 'a io

  type 'a t

  val create :
        ?max_size: int ->
        ?check: ('a -> (bool -> unit) -> unit) ->
        ?validate: ('a -> bool io) ->
        (unit -> 'a io) -> ('a -> unit io) -> 'a t
  (** (internal) [create alloc free] is a pool of resources allocated by
      [alloc] and freed by [free]. This is primarily indented for implementing
      the [connect_pool] functions.

      @param max_size
        Maximum number of resources to allocate at any given time.

      @param check
        A function used to check a resource after use.

      @param validate
        A function to check before use that a resource is still valid. *)

  val size : 'a t -> int
  (** [size pool] is the current number of open resources in [pool]. *)

  val use : ?priority: float -> ('a -> 'b io) -> 'a t -> 'b io
  (** [use f pool] calls [f] on a resource drawn from [pool], handing back the
      resource to the pool when [f] exits.

      @param priority
        Requests for the resource are handled in decreasing order of priority.
        The default priority is [0.0]. *)

  val drain : 'a t -> unit io
  (** [drain pool] closes all resources in [pool]. The pool is still usable, as
      new resources will be created on demand. *)

end
