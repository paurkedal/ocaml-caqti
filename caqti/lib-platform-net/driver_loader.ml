(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
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
  functor (System : System_sig.S) ->
  Caqti_driver_sig.S
    with type 'a future := 'a System.future
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t

let drivers = Hashtbl.create 5
let register scheme p = Hashtbl.add drivers scheme p

module Make (System : System_sig.S) = struct
  module type DRIVER = Caqti_driver_sig.S
    with type 'a future := 'a System.future
     and type ('a, 'e) stream := ('a, 'e) System.Stream.t

  let load_driver ~uri scheme =
    (match Hashtbl.find_opt drivers scheme with
     | None ->
        let msg = "driver not found for net platform." in
        Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg))
     | Some make_driver ->
        let module Make_driver = (val make_driver : DRIVER_FUNCTOR) in
        let module Driver = Make_driver (System) in
        Ok (module Driver : DRIVER))
end
