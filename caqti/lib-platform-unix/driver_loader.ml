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

module type DRIVER_FUNCTOR =
  functor (System : Caqti_platform.System_sig.S) ->
  functor (System_unix : System_sig.S
    with type 'a future := 'a System.future
     and type connect_env := System.connect_env) ->
  Caqti_platform.Driver_sig.S
    with type 'a future := 'a System.future
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t
     and type switch := System.Switch.t
     and type connect_env := System.connect_env

let drivers = Hashtbl.create 5
let register scheme p = Hashtbl.add drivers scheme p

module Make
  (System : Caqti_platform.System_sig.S)
  (System_unix : System_sig.S
    with type 'a future := 'a System.future
     and type connect_env := System.connect_env) =
struct
  module Core_loader = Caqti_platform.Driver_loader.Make (System)

  module type DRIVER = Core_loader.DRIVER

  let load_driver_unix ~uri scheme =
    (match Hashtbl.find_opt drivers scheme with
     | None ->
        let msg = "Driver not found for unix platform." in
        Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg))
     | Some make_driver ->
        let module Make_driver = (val make_driver : DRIVER_FUNCTOR) in
        let module Driver = Make_driver (System) (System_unix) in
        Ok (module Driver : DRIVER))

  let load_driver ~uri scheme =
    (match Core_loader.load_driver ~uri scheme with
     | Ok _ as r -> r
     | Error (`Load_rejected _) as r -> r
     | Error (`Load_failed _) ->
        (match load_driver_unix ~uri scheme with
         | Ok _ as r -> r
         | Error (`Load_rejected _) as r -> r
         | Error (`Load_failed _) ->
            let msg = "Driver not found, including among UNIX drivers." in
            Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg))))

end
