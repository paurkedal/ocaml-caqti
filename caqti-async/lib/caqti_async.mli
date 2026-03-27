(* Copyright (C) 2014--2026  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Connector for Async. *)

open Async_kernel

module Stream : Caqti.Stream.S with type 'a fiber := 'a Deferred.t
module Pool : Caqti.Pool.S with type 'a fiber := 'a Deferred.t

module type CONNECTION = Caqti.Connection.S
  with type 'a fiber := 'a Deferred.t
   and type ('a, 'e) stream := ('a, 'e) Stream.t

include Caqti.Connect.S
  with type 'a fiber := 'a Deferred.t
   and type 'a with_switch := 'a
   and type 'a with_stdenv := 'a
   and type ('a, 'e) stream := ('a, 'e) Stream.t
   and type ('a, 'e) pool := ('a, 'e) Pool.t
   and type connection = (module CONNECTION)

(**/**)
module System : Caqti_platform.System_sig.S
  with type 'a Fiber.t = 'a Deferred.t
   and module Stream = Stream
   and type stdenv = unit
   and type Net.tcp_flow = Async_unix.Reader.t * Async_unix.Writer.t
   and type Net.tls_flow = Async_unix.Reader.t * Async_unix.Writer.t
