(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

let drivers_v1 = Hashtbl.create 11
let define_driver_v1 scheme p = Hashtbl.add drivers_v1 scheme p

type Caqti_error.driver_msg += Driver_msg of string

let () =
  let pp ppf = function
   | Driver_msg s -> Format.pp_print_string ppf s
   | _ -> assert false in
  Caqti_error.define_driver_msg ~pp [%extension_constructor Driver_msg]

let load_driver_functor_v1 ~uri scheme =
  (try Ok (Hashtbl.find drivers_v1 scheme) with
   | Not_found ->
      (match !dynload_library ("caqti-driver-" ^ scheme) with
       | Ok () ->
          (try Ok (Hashtbl.find drivers_v1 scheme) with
           | Not_found ->
              let msg = sprintf "The driver for %s did not register itself \
                                 after apparently loading." scheme in
              Error (Caqti_error.load_failed ~uri msg))
       | Error msg -> Error (Caqti_error.load_failed ~uri msg)))

module Make_v1 (System : Caqti_system_sig.V1) = struct
  open System

  module type DRIVER = Caqti_driver_sig.V1 with type 'a io := 'a System.io

  let drivers : (string, (module DRIVER)) Hashtbl.t = Hashtbl.create 11

  let load_driver uri =
    (match Uri.scheme uri with
     | None ->
        Error (Caqti_error.load_rejected ~uri "Missing URI scheme.")
     | Some scheme ->
        (try Ok (Hashtbl.find drivers scheme) with
         | Not_found ->
            (match load_driver_functor_v1 ~uri scheme with
             | Ok v1_functor ->
                let module F = (val v1_functor : Caqti_driver_sig.V1_FUNCTOR) in
                let module Driver = F (System) in
                let driver = (module Driver : DRIVER) in
                Hashtbl.add drivers scheme driver;
                Ok driver
             | Error _ as r -> r)))

  module type CONNECTION = Caqti_sigs.CONNECTION with type 'a io = 'a System.io

  let connect uri : (module CONNECTION) System.io =
    (match load_driver uri with
     | Ok driver ->
        let module Driver = (val driver) in
        Driver.connect uri >>= fun client ->
        let module Client = (val client) in
        return (module Client : CONNECTION)
     | Error err ->
        let msg = Caqti_error.to_string_hum err in
        (match Uri.scheme uri with
         | None ->
            fail (Caqti_plugin.Plugin_missing ("?", msg))
         | Some scheme ->
            fail (Caqti_plugin.Plugin_missing ("caqti-driver-" ^ scheme, msg))))

  module Pool = Caqti_pool.Make_v1 (System)

  let connect_pool ?max_size uri : (module CONNECTION) Pool.t =
    let connect () = connect uri in
    let disconnect (module Conn : CONNECTION) = Conn.disconnect () in
    let validate (module Conn : CONNECTION) = Conn.validate () in
    let check (module Conn : CONNECTION) = Conn.check in
    Pool.create ?max_size ~validate ~check connect disconnect
end

module Make_v2 (System : Caqti_system_sig.V1) = struct
  open System

  module V1 = Make_v1 (System)
  open V1

  module type CONNECTION =
    Caqti_connection_sig.S with type 'a io := 'a System.io

  type connection = (module CONNECTION)

  let catch_connect_issue f =
    catch (fun () -> f () >|= fun y -> Ok y)
      (function
       | Caqti_errors.Connect_failed (uri, msg) ->
          return (Error (Caqti_error.connect_failed ~uri (Driver_msg msg)))
       | exn ->
          fail exn)

  let connect uri : ((module CONNECTION), _) result io =
    (match load_driver uri with
     | Ok driver ->
        let module Driver = (val driver) in
        catch_connect_issue @@ fun () ->
        Driver.connect uri >>= fun v1 ->
        let module V1 = (val v1) in
        let module V2 = Caqti_compat.Connection_v2_of_v1 (System) (V1) in
        return (module V2 : CONNECTION)
     | Error err ->
        return (Error err))

  module Pool = Caqti_pool.Make_v2 (System)

  let connect_pool ?max_size uri =
    (match load_driver uri with
     | Ok driver ->
        let module Driver = (val driver) in
        let connect () =
          catch_connect_issue @@ fun () ->
          Driver.connect uri >>= fun v1 ->
          let module V1 = (val v1) in
          let module V2 = Caqti_compat.Connection_v2_of_v1 (System) (V1) in
          return (module V2 : CONNECTION) in
        let disconnect (module Db : CONNECTION) = Db.disconnect () in
        let validate (module Db : CONNECTION) = Db.validate () in
        let check (module Db : CONNECTION) = Db.check in
        let di = Driver.driver_info in
        let max_size =
          if not (Caqti_driver_info.(can_concur di && can_pool di))
          then Some 1
          else max_size in
        let free c =
          disconnect c >|= function Ok () -> true | Error _ -> false in
        Ok (Pool.create ?max_size ~validate ~check connect free)
     | Error err ->
        Error err)
end

module Make = Make_v1
