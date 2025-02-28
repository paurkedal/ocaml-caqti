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

    See also {!Caqti_lwt} for basic Lwt support.

    {b The caqti-mirage library is experimental at this point.}  Feedback from
    MirageOS users on the current API is very welcome. *)

module Make :
  functor (STACK : Tcpip.Stack.V4V6) ->
  functor (DNS : Dns_client_mirage.S) ->
sig
  module Pool : Caqti_pool_sig.S with type 'a fiber := 'a Lwt.t
  include Caqti_connect_sig.S
    with type 'a fiber := 'a Lwt.t
     and type ('a, 'e) stream := ('a, 'e) Caqti_lwt.Stream.t
     and type ('a, 'e) pool := ('a, 'e) Pool.t
     and type connection := Caqti_lwt.connection
     and type 'a with_switch := ?sw: Caqti_lwt.Switch.t -> 'a
     and type 'a with_stdenv := STACK.t -> DNS.t -> 'a
end
