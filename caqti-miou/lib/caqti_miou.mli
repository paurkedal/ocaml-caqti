(* Copyright (C) 2023--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

type switch

module Stream : Caqti_stream_sig.S with type 'a fiber := 'a
module Switch : Caqti_switch_sig.S
  with type 'a fiber := 'a
   and type t = switch

module System_core : sig
  include Caqti_platform.System_sig.CORE
    with type 'a Fiber.t = 'a
     and module Stream = Stream
     and type Switch.t = Switch.t
     and type stdenv := unit
end

module type CONNECTION = Caqti_connection_sig.S
  with type 'a fiber := 'a
   and type ('a, 'e) stream := ('a, 'e) Stream.t

type connection = (module CONNECTION)

val or_fail : ('a, [< Caqti_error.t ]) result -> 'a
