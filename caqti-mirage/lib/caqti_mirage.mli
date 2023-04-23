(* Copyright (C) 2022--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Functions for connecting to databases from MirageOS unikernels

    This module contains functions for connecting to databases using the
    MirageOS platform libraries, providing support for PGX but not drivers based
    on bindings.

    See also {!Caqti_lwt} for basic Lwt support. *)

module Make :
  functor (RANDOM : Mirage_random.S) ->
  functor (TIME : Mirage_time.S) ->
  functor (MCLOCK : Mirage_clock.MCLOCK) ->
  functor (PCLOCK : Mirage_clock.PCLOCK) ->
  functor (STACK : Tcpip.Stack.V4V6) ->
  functor (DNS : Dns_client_mirage.S) ->
  Caqti_connect_sig.S
    with type 'a future := 'a Lwt.t
     and module Stream = Caqti_lwt.Stream
     and module type CONNECTION = Caqti_lwt.CONNECTION
     and type connection := Caqti_lwt.connection
     and type 'a connect_fun := STACK.t -> DNS.t -> Uri.t -> 'a
     and type 'a with_connection_fun := STACK.t -> DNS.t -> Uri.t -> 'a
