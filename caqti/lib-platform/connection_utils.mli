(* Copyright (C) 2019--2020  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Internal connection-related utilities. *)

module Make_helpers : functor (Sys : System_sig.S) -> sig
  open Sys

  val assert_single_use :
    what: string -> bool ref -> (unit -> 'a Fiber.t) -> 'a Fiber.t
end

module Make_convenience :
  functor (Sys : System_sig.S) ->
  functor (_ : Caqti_connection_sig.Base
                with type 'a fiber := 'a Sys.Fiber.t
                 and type ('a, 'err) stream := ('a, 'err) Sys.Stream.t) ->
  Caqti_connection_sig.Convenience with type 'a fiber := 'a Sys.Fiber.t

module Make_populate :
  functor (Sys : System_sig.S) ->
  functor (_ : Caqti_connection_sig.Base
                with type 'a fiber := 'a Sys.Fiber.t
                 and type ('a, 'err) stream := ('a, 'err) Sys.Stream.t) ->
  Caqti_connection_sig.Populate
    with type 'a fiber := 'a Sys.Fiber.t
     and type ('a, 'err) stream := ('a, 'err) Sys.Stream.t
