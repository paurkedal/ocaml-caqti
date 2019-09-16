(* Copyright (C) 2014--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

let dynload_library = ref @@ fun lib ->
  Error (sprintf "Neither %s nor the dynamic linker is linked into the \
                  application." lib)

let define_loader f = dynload_library := f

let drivers = Hashtbl.create 11
let define_unix_driver scheme p = Hashtbl.add drivers scheme p

let scheme_driver_name = function
  | "postgres" | "postgresql" -> "caqti-driver-postgresql"
  | s -> "caqti-driver-" ^ s

let load_driver_functor ~uri scheme =
  (try Ok (Hashtbl.find drivers scheme) with
   | Not_found ->
      (match !dynload_library (scheme_driver_name scheme) with
       | Ok () ->
          (try Ok (Hashtbl.find drivers scheme) with
           | Not_found ->
              let msg = sprintf "The driver for %s did not register itself \
                                 after apparently loading." scheme in
              Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg)))
       | Error msg ->
          Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg))))

module Make_unix (System : Caqti_driver_sig.System_unix) = struct
  open System

  module type DRIVER = Caqti_driver_sig.S
    with type 'a future := 'a System.future
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t

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
             | Ok make_driver ->
                let module Make_driver =
                  (val make_driver : Caqti_driver_sig.Of_system_unix) in
                let module Driver = Make_driver (System) in
                let driver = (module Driver : DRIVER) in
                Hashtbl.add drivers scheme driver;
                Ok driver
             | Error _ as r -> r)))

  module type CONNECTION_BASE = Caqti_connection_sig.Base
    with type 'a future := 'a System.future
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a future := 'a System.future
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t

  type connection = (module CONNECTION)

  let connect uri : ((module CONNECTION), _) result future =
    (match load_driver uri with
     | Ok driver ->
        let module Driver = (val driver) in
        Driver.connect uri
     | Error err ->
        return (Error err))

  module Pool = Caqti_pool.Make (System)
  module Stream = System.Stream

  let connect_pool ?max_size uri =
    (match load_driver uri with
     | Ok driver ->
        let module Driver = (val driver) in
        let connect () = Driver.connect uri in
        let disconnect (module Db : CONNECTION) = Db.disconnect () in
        let validate (module Db : CONNECTION) = Db.validate () in
        let check (module Db : CONNECTION) = Db.check in
        let di = Driver.driver_info in
        let max_size =
          if not (Caqti_driver_info.(can_concur di && can_pool di))
          then Some 1
          else max_size in
        Ok (Pool.create ?max_size ~validate ~check connect disconnect)
     | Error err ->
        Error err)
end
