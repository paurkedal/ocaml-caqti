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

include Caqti_connect_sig.Connect
  with type 'a future := 'a
   and type ('a, 'e) pool := ('a, 'e) Pool.t
   and type connection := connection
   and type 'a connect_fun :=
    ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
    ?tweaks_version: int * int ->
    Eio.Stdenv.t -> Uri.t -> 'a

val or_fail : ('a, [< Caqti_error.t]) result -> 'a
(** Eliminates the error-case by raising {!Caqti_error.Exn}. *)
