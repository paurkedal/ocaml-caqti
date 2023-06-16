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

(** Connection functor and registration for driver using the Unix module. *)

module type DRIVER_FUNCTOR =
  functor (System : Caqti_platform.System_sig.S) ->
  functor (System_unix : System_sig.S
    with type 'a fiber := 'a System.Fiber.t
     and type stdenv := System.stdenv) ->
  Caqti_platform.Driver_sig.S
    with type 'a fiber := 'a System.Fiber.t
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t
     and type switch := System.Switch.t
     and type stdenv := System.stdenv

val register : string -> (module DRIVER_FUNCTOR) -> unit
(** [define_unix_driver scheme m] installs [m] as a handler for the URI scheme
    [scheme].  This call must be done by a backend installed with findlib name
    caqti-driver-{i scheme} as part of its initialization. *)

module Make
    (System : Caqti_platform.System_sig.S)
    (System_unix : System_sig.S
      with type 'a fiber := 'a System.Fiber.t
       and type stdenv := System.stdenv) :
  Caqti_platform.Driver_sig.Loader
    with type 'a fiber := 'a System.Fiber.t
     and type ('a, 'e) stream := ('a, 'e) System.Stream.t
     and type switch := System.Switch.t
     and type stdenv := System.stdenv
(** Constructs the main module used to connect to a database for the given
    concurrency model. *)
