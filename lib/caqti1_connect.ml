(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf

let drivers = Hashtbl.create 11
let define_driver scheme p = Hashtbl.add drivers scheme p

let load_driver_functor ~uri scheme =
  (try Ok (Hashtbl.find drivers scheme) with
   | Not_found ->
      (match
        !Caqti_connect.dynload_library ("caqti-driver-" ^ scheme ^ ".v1")
        [@ocaml.warning "-3"]
       with
       | Ok () ->
          (try Ok (Hashtbl.find drivers scheme) with
           | Not_found ->
              let msg = sprintf "The driver for %s did not register itself \
                                 after apparently loading." scheme in
              Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg)))
       | Error msg ->
          Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg))))

module Make (System : Caqti1_system_sig.S) = struct
  open System

  module type DRIVER = Caqti1_driver_sig.S with type 'a io := 'a System.io

  let drivers : (string, (module DRIVER)) Hashtbl.t = Hashtbl.create 11

  let load_driver uri =
    (match Uri.scheme uri with
     | None ->
        let msg = "Missing URI scheme." in
        Error (Caqti_error.load_rejected ~uri (Caqti_error.Msg msg))
     | Some scheme ->
        (try Ok (Hashtbl.find drivers scheme) with
         | Not_found ->
            (match load_driver_functor ~uri scheme with
             | Ok v1_functor ->
                let module F = (val v1_functor : Caqti1_driver_sig.F) in
                let module Driver = F (System) in
                let driver = (module Driver : DRIVER) in
                Hashtbl.add drivers scheme driver;
                Ok driver
             | Error _ as r -> r)))

  module type CONNECTION = Caqti1_sigs.CONNECTION with type 'a io = 'a System.io

  let connect uri : (module CONNECTION) System.io =
    (match load_driver uri with
     | Ok driver ->
        let module Driver = (val driver) in
        Driver.connect uri >>= fun client ->
        let module Client = (val client) in
        return (module Client : CONNECTION)
     | Error err ->
        let msg = Caqti_error.show err in
        (match Uri.scheme uri with
         | None ->
            fail (Caqti1_plugin.Plugin_missing ("?", msg))
         | Some scheme ->
            fail (Caqti1_plugin.Plugin_missing ("caqti-driver-" ^ scheme ^ ".v1", msg))))

  module Pool = Caqti1_pool.Make (System)

  let connect_pool ?max_size uri : (module CONNECTION) Pool.t =
    let connect () = connect uri in
    let disconnect (module Conn : CONNECTION) = Conn.disconnect () in
    let validate (module Conn : CONNECTION) = Conn.validate () in
    let check (module Conn : CONNECTION) = Conn.check in
    Pool.create ?max_size ~validate ~check connect disconnect
end
