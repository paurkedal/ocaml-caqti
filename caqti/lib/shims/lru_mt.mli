(* Copyright (C) 2024--2026  Petter A. Urkedal <paurkedal@gmail.com>
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

[@@@alert caqti_private "This shim is for internal use by Caqti"]

(** Thread-safe extension to [Lru]. *)

val memo_if_safe :
  ?hashed: (('a -> int) * ('a -> 'a -> bool)) ->
  ?weight: ('b -> int) ->
  cap: int ->
  (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
(** This is either a thread-safe version of {!Lru.memo} or, if thread-primitives
    are missing, a non-memoizing fallback. *)
