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

module type DRIVER_FUNCTOR =
  functor (System : Caqti_platform.System_sig.S) ->
  functor (_ : System_sig.S
    with type 'a fiber := 'a System.Fiber.t
     and type stdenv := System.stdenv) ->
  Caqti_platform.Driver_loader.DRIVER
    with type 'a fiber := 'a System.Fiber.t
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t
     and type switch := System.Switch.t
     and type stdenv := System.stdenv

let drivers = Hashtbl.create 5
let register scheme p = Hashtbl.add drivers scheme p

module Make
  (System : Caqti_platform.System_sig.S)
  (System_unix : System_sig.S
    with type 'a fiber := 'a System.Fiber.t
     and type stdenv := System.stdenv) =
struct
  module Core_loader = Caqti_platform.Driver_loader.Make (System)

  module type DRIVER = Core_loader.DRIVER
  module type CONNECTION = Core_loader.CONNECTION

  let provides_unix = true

  let find_and_apply' scheme =
    (match Hashtbl.find_opt drivers scheme with
     | None -> None
     | Some (module F : DRIVER_FUNCTOR) ->
        let module Driver = F (System) (System_unix) in
        Some (module Driver : DRIVER))

  let find_and_apply scheme =
    (match Core_loader.find_and_apply scheme with
     | Some _ as r -> r
     | None -> find_and_apply' scheme)
end
