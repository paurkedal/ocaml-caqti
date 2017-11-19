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

open Caqti_plugin
open Caqti_sigs

let scheme_plugins = Hashtbl.create 11
let register_scheme scheme p = Hashtbl.add scheme_plugins scheme p

type Caqti_error.driver_msg += Driver_msg of string

let () =
  let pp ppf = function
   | Driver_msg s -> Format.pp_print_string ppf s
   | _ -> assert false in
  Caqti_error.define_driver_msg ~pp [%extension_constructor Driver_msg]

module Make (System : Caqti_system_sig.S) = struct
  open System

  module System = System

  module type DRIVER = Caqti_driver_sig.S with type 'a io := 'a System.io

  let drivers : (string, (module DRIVER)) Hashtbl.t = Hashtbl.create 11

  let load_driver scheme =
    try Hashtbl.find drivers scheme with Not_found ->
    let driver_functor =
      ensure_plugin
        (fun () -> try Some (Hashtbl.find scheme_plugins scheme)
                   with Not_found -> None)
        ("caqti-driver-" ^ scheme) in
    let module Make_driver = (val driver_functor : Caqti_driver_sig.F) in
    let module Driver = Make_driver (System) in
    let driver = (module Driver : DRIVER) in
    Hashtbl.add drivers scheme driver; driver

  let load_driver_for uri =
    (match Uri.scheme uri with
     | None -> failwith "caqti_*.connect: Missing URI scheme."
     | Some scheme -> load_driver scheme)

  module type CONNECTION = CONNECTION with type 'a io = 'a System.io

  let connect uri : (module CONNECTION) System.io =
    try
      let driver = load_driver_for uri in
      let module Driver = (val driver) in
      Driver.connect uri >>= fun client ->
      let module Client = (val client) in
      return (module Client : CONNECTION)
    with xc -> fail xc

  module Pool = Caqti_pool.Make (System)

  let connect_pool ?max_size uri : (module CONNECTION) Pool.t =
    let connect () = connect uri in
    let disconnect (module Conn : CONNECTION) = Conn.disconnect () in
    let validate (module Conn : CONNECTION) = Conn.validate () in
    let check (module Conn : CONNECTION) = Conn.check in
    Pool.create ?max_size ~validate ~check connect disconnect

  module type CONNECTION_V2 =
    Caqti_connection_sig.S with type 'a io := 'a System.io

  let catch_load_issue uri f =
    try Ok (f ()) with
     | Failure msg ->
        Error (Caqti_error.connect_unavailable ~uri msg)
     | Plugin_missing (pkg, msg) ->
        let msg = "Missing driver " ^ pkg ^ ": " ^ msg in
        Error (Caqti_error.connect_unavailable ~uri msg)
     | Plugin_invalid (pkg, msg) ->
        let msg = "Invalid driver " ^ pkg ^ ": " ^ msg in
        Error (Caqti_error.connect_unavailable ~uri msg)

  let catch_connect_issue f =
    catch (fun () -> f () >|= fun y -> Ok y)
      (function
       | Caqti_errors.Connect_failed (uri, msg) ->
          return (Error (Caqti_error.connect_failed ~uri (Driver_msg msg)))
       | exn ->
          fail exn)

  let catch_connect_issue_exn f =
    catch f
      (function
       | Caqti_errors.Connect_failed (uri, msg) ->
          let err = Caqti_error.connect_failed ~uri (Driver_msg msg) in
          fail (Caqti_error.Exn err)
       | exn ->
          fail exn)

  let connect_v2 uri : ((module CONNECTION_V2), _) result io =
    (match catch_load_issue uri @@ fun () -> load_driver_for uri with
     | Ok driver ->
        let module Driver = (val driver) in
        catch_connect_issue @@ fun () ->
        Driver.connect uri >>= fun v1 ->
        let module V2 = Caqti_compat.Connection_v2_of_v1 (System) (val v1) in
        return (module V2 : CONNECTION_V2)
     | Error err ->
        return (Error err))

  let connect_pool_v2 ?max_size uri
      : ((module CONNECTION_V2) Pool.t, _) result =
    catch_load_issue uri @@ fun () ->
    let driver = load_driver_for uri in
    let module Driver = (val driver) in
    let connect () =
      catch_connect_issue_exn @@ fun () ->
      Driver.connect uri >>= fun v1 ->
      let module V2 = Caqti_compat.Connection_v2_of_v1 (System) (val v1) in
      return (module V2 : CONNECTION_V2) in
    let disconnect (module Db : CONNECTION_V2) = Db.disconnect () in
    let validate (module Db : CONNECTION_V2) = Db.validate () in
    let check (module Db : CONNECTION_V2) = Db.check in
    let di = Driver.driver_info in
    let max_size =
      if not (Caqti_driver_info.(can_concur di && can_pool di))
      then Some 1
      else max_size in
    Pool.create ?max_size ~validate ~check connect disconnect
end
