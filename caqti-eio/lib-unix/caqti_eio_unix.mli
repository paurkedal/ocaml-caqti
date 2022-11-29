(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Connecting with Eio.

    This module contains the signature and connect function specialized for use
    with Eio. *)

include Caqti_connect_sig.S_without_connect with type 'a future := 'a

val connect :
  ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
  ?tweaks_version: int * int ->
  Eio.Stdenv.t -> sw: Eio.Switch.t -> Uri.t ->
  (connection, [> Caqti_error.load_or_connect]) result

val connect_pool :
  ?max_size: int -> ?max_idle_size: int -> ?max_use_count: int option ->
  ?post_connect: (connection -> (unit, 'connect_error) result) ->
  ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
  ?tweaks_version: int * int ->
  Eio.Stdenv.t -> sw: Eio.Switch.t -> Uri.t ->
  ((connection, [> Caqti_error.connect] as 'connect_error) Pool.t,
   [> Caqti_error.load]) result

val or_fail : ('a, [< Caqti_error.t]) result -> 'a
(** Eliminates the error-case by raising {!Caqti_error.Exn}. *)
