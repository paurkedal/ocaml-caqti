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

(** Cache for associating data to requests for a fixed dialect. *)

module type S = sig
  type elt
  type t

  val create : ?dynamic_capacity: int -> Caqti_template.Dialect.t -> t
  (** [create dialect] creates a cache of prepared queries for requests
      specialized for [dialect].

      @param dynamic_capacity
        is the sum weight of elements of the LRU used for dynamic requests below
        which {!trim} will not attempt to release anything. *)

  val find_and_promote :
    t -> ('a, 'b, 'm) Caqti_template.Request.t -> elt option
  (** [find_and_promote cache request] promotes and returns the data associated
      with [request] in [cache], if any. *)

  val add : t -> ('a, 'b, 'm) Caqti_template.Request.t -> elt -> unit
  (** Given the hard precondition that [request] is not bound in [cache], as
      verified by {!find_and_promote}, [add cache request data] binds it to
      [data]. *)

  val remove_and_discard : t -> ('a, 'b, 'm) Caqti_template.Request.t -> unit
  (** [remove_and_discard cache request] removes the binding for [request] from
      [cache]. The caller is assumed to have just extracted the element with
      {!find_and_promote} and is resposible for releasing related resources. *)

  val deallocate :
    t -> ('a, 'b, 'm) Caqti_template.Request.t -> (elt * (unit -> unit)) option
  (** [deallocate request] returns the entry for [request], if any, along with a
      function to remove it from the cache. *)

  val iter : (elt -> unit) -> t -> unit
  (** [iter f cache] calls [f] on each element of [cache]. *)

  val elements : t -> elt list
  (** [elements cache] is an arbitrarily ordered list of all elements, whether
      associated with a static or dynamic request. *)

  val trim : ?max_promote_count: int -> t -> elt list * (unit -> unit)
  (** [trim ?promote_count cache] carrious out some work trimming elements from
      the dynamic LRU cache.
      The call promotes up to [max_promote_count] elements, which are alive
      according to the garbage collector, and stop at the next one.
      It is up to the caller to free any resources associated with the returned
      elements. *)

  val clear_and_discard : t -> unit
  (** [clear cache] removes all entries form [cache].  Elements are not
      returned, since this function may be used after a connection reset which
      invalidates the cached resources, but can be requested by calling
      {!elements} first. *)

  val dynamic_weight : t -> int
end

module Make : functor (Elt : Lru.Weighted) -> S with type elt = Elt.t
