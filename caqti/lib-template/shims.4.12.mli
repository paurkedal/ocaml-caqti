(* Copyright (C) 2024--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Compatibility shims.

    This is the fallback implementation providing replacements for recent
    additions to the standard library. *)

module Type : sig
  type (_, _) eq = Equal : ('a, 'a) eq
  (** Type equality witness.  This will eventually be replaced by the equavalent
      definition available in [Stdlib.Type] since OCaml 5.1, but for now, we
      must keep backwards compatibility with older compilers. *)
end

(**/**)

module Atomic : sig
  type 'a t = 'a Stdlib.Atomic.t
  val make : 'a -> 'a t
  val fetch_and_add : int t -> int -> int
end

(* This is an opportunistically memoizing fallback. *)
val memo_if_safe :
  ?hashed: (('a -> int) * ('a -> 'a -> bool)) ->
  ?weight: ('b -> int) ->
  cap: int ->
  (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
