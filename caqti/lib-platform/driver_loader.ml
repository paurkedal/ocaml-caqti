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

module type DRIVER_FUNCTOR =
  functor (System : System_sig.S) ->
  DRIVER
    with type 'a fiber := 'a System.Fiber.t
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t
     and type switch := System.Switch.t
     and type stdenv := System.stdenv

let drivers = Hashtbl.create 5
let register scheme p = Hashtbl.add drivers scheme p

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

module Make (System : System_sig.S) = struct
  module type DRIVER = DRIVER
    with type 'a fiber := 'a System.Fiber.t
     and type ('a, 'e) stream := ('a, 'e) System.Stream.t
     and type switch := System.Switch.t
     and type stdenv := System.stdenv

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a fiber := 'a System.Fiber.t
     and type ('a, 'e) stream := ('a, 'e) System.Stream.t

  let provides_unix = false

  let find_and_apply scheme =
    (match Hashtbl.find_opt drivers scheme with
     | None -> None
     | Some (module F : DRIVER_FUNCTOR) ->
        Some (module F (System) : DRIVER))
end
