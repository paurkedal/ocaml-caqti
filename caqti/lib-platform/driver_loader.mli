(* Copyright (C) 2022--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Registration and Loading of Drivers

    This interface is unstable and may change between minor versions.  If you
    are developing an external driver, please open an issue to sort out
    requirements and to announce you need for a stable driver API. *)

(** This is the signature implemented by drivers, given the system dependencies.
    More precisely, drivers implement either {!DRIVER_FUNCTOR} or
    {!Caqti_platform_unix.Driver_loader.DRIVER_FUNCTOR} depending on
    requirements. *)
module type DRIVER = sig
  type +'a fiber
  type (+'a, +'err) stream
  type switch
  type stdenv

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a fiber := 'a fiber
     and type ('a, 'err) stream := ('a, 'err) stream

  val driver_info : Caqti_driver_info.t

  val connect :
    sw: switch ->
    stdenv: stdenv ->
    ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
    config: Caqti_connect_config.t ->
    Uri.t ->
    ((module CONNECTION), [> Caqti_error.connect]) result fiber
end

(** {2 Registration} *)

module type DRIVER_FUNCTOR =
  functor (System : System_sig.S) ->
  DRIVER
    with type 'a fiber := 'a System.Fiber.t
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t
     and type switch := System.Switch.t
     and type stdenv := System.stdenv
(** The functor implemented by drivers independent from the unix library. *)

val register : string -> (module DRIVER_FUNCTOR) -> unit
(** [register scheme driver_functor] registers [driver_functor] as the driver
    implementation for handling the URI scheme [scheme]. *)

(** {2 Usage} *)

(** The the interface used internally to load drivers. *)
module type S = sig
  type +'a fiber
  type (+'a, +'e) stream
  type switch
  type stdenv

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a fiber := 'a fiber
     and type ('a, 'e) stream := ('a, 'e) stream

  module type DRIVER = DRIVER
    with type 'a fiber := 'a fiber
     and type ('a, 'e) stream := ('a, 'e) stream
     and type switch := switch
     and type stdenv := stdenv

  val provides_unix : bool

  val find_and_apply : string -> (module DRIVER) option
end

module Make (System : System_sig.S) : S
  with type 'a fiber := 'a System.Fiber.t
   and type ('a, 'e) stream := ('a, 'e) System.Stream.t
   and type switch := System.Switch.t
   and type stdenv := System.stdenv
(** Instantiation of the loader interface for give system dependencies. *)
