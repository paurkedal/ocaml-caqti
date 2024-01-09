(* Copyright (C) 2024  Petter A. Urkedal <paurkedal@gmail.com>
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

module type PLATFORM = sig
  type context
  module Fiber : sig
    type +'a t
    module Infix : sig
      val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
      val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    end
  end
  val name : string
  val run_fiber : (unit -> 'a Fiber.t) -> 'a
  val run_main : (context -> 'a) -> 'a
  val or_fail : ('a, [< Caqti_error.t]) result -> 'a Fiber.t

  module Stream : Caqti_stream_sig.S with type 'a fiber := 'a Fiber.t

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a fiber := 'a Fiber.t
     and type ('a, 'err) stream := ('a, 'err) Stream.t

  type connection = (module CONNECTION)

  val connect :
    ?config: Caqti_connect_config.t -> context -> Uri.t ->
    (connection, [> Caqti_error.load_or_connect]) result Fiber.t
end
