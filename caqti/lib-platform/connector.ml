(* Copyright (C) 2014--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

open Printf

let default_tweaks_version = (1, 7)

let dynload_library = ref @@ fun lib ->
  Error (sprintf "Neither %s nor the dynamic linker is linked into the \
                  application." lib)

let define_loader f = dynload_library := f

let load_library name = !dynload_library name

let scheme_driver_name = function
 | "postgres" | "postgresql" -> "caqti-driver-postgresql"
 | s -> "caqti-driver-" ^ s

let retry_with_library load_driver ~uri scheme =
  (match load_driver ~uri scheme with
   | Error (`Load_failed _) ->
      let driver_name = scheme_driver_name scheme in
      (match load_library driver_name with
       | Ok () ->
          (match load_driver ~uri scheme with
           | Error (`Load_failed _) ->
              let msg = Printf.sprintf
                "The %s did not register a handler for the URI scheme %s."
                driver_name scheme
              in
              Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg))
           | r -> r)
       | Error msg ->
          Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg)))
   | r -> r)

module Make
  (System : System_sig.S)
  (Pool : Pool.S
    with type 'a fiber := 'a System.Fiber.t
     and type switch := System.Switch.t
     and type connect_env := System.connect_env)
  (Loader : Driver_sig.Loader
    with type 'a fiber := 'a System.Fiber.t
     and type switch := System.Switch.t
     and type connect_env := System.connect_env
     and type ('a, 'e) stream := ('a, 'e) System.Stream.t) =
struct
  open System
  open System.Fiber.Infix

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a fiber := 'a Fiber.t
     and type ('a, 'err) stream := ('a, 'err) Stream.t

  type connection = (module CONNECTION)

  let (>>=?) m f = m >>= function Ok x -> f x | Error _ as r -> Fiber.return r
  let (>|=?) m f = m >|= function Ok x -> (Ok (f x)) | Error _ as r -> r
  let (let+?) = (>|=?)

  module type DRIVER = Driver_sig.S
    with type 'a fiber := 'a Fiber.t
     and type ('a, 'err) stream := ('a, 'err) Stream.t
     and type switch := System.Switch.t
     and type connect_env := System.connect_env

  let drivers : (string, (module DRIVER)) Hashtbl.t = Hashtbl.create 11

  let load_driver uri =
    (match Uri.scheme uri with
     | None ->
        let msg = "Missing URI scheme." in
        Error (Caqti_error.load_rejected ~uri (Caqti_error.Msg msg))
     | Some scheme ->
        (try Ok (Hashtbl.find drivers scheme) with
         | Not_found ->
            (match retry_with_library Loader.load_driver ~uri scheme with
             | Ok driver ->
                Hashtbl.add drivers scheme driver;
                Ok driver
             | Error _ as r -> r)))

  let connect
        ?env ?(tweaks_version = default_tweaks_version) ~sw ~connect_env uri
      : ((module CONNECTION), _) result Fiber.t =
    Switch.check sw;
    (match load_driver uri with
     | Ok driver ->
        let module Driver = (val driver) in
        let+? conn = Driver.connect ~sw ~connect_env ?env ~tweaks_version uri in
        let module Conn = (val conn : CONNECTION) in
        let module Conn' = struct
          include Conn
          let disconnect =
            let hook = Switch.on_release_cancellable sw disconnect in
            fun () -> Switch.remove_hook hook; disconnect ()
        end in
        (module Conn' : CONNECTION)
     | Error err ->
        Fiber.return (Error err))

  let with_connection
        ?env ?(tweaks_version = default_tweaks_version) ~connect_env uri f =
    Switch.run begin fun sw ->
      connect ~sw ~connect_env ?env ~tweaks_version uri >>=? f
    end

  let connect_pool
        ?max_size ?max_idle_size ?max_idle_age ?(max_use_count = Some 100)
        ?post_connect
        ?env ?(tweaks_version = default_tweaks_version) ~sw ~connect_env uri =
    Switch.check sw;
    let check_arg cond =
      if not cond then invalid_arg "Caqti_connect.Make.connect_pool"
    in
    (match max_size, max_idle_size with
     | None, None -> ()
     | Some max_size, None -> check_arg (max_size >= 0)
     | None, Some _ -> check_arg false
     | Some max_size, Some max_idle_size ->
        check_arg (max_size >= 0);
        check_arg (0 <= max_idle_size && max_idle_size <= max_size));
    (match load_driver uri with
     | Ok driver ->
        let module Driver = (val driver) in
        let connect =
          (match post_connect with
           | None ->
              fun () ->
                (Driver.connect ~sw ~connect_env ?env ~tweaks_version uri
                    :> (connection, _) result Fiber.t)
           | Some post_connect ->
              fun () ->
                (Driver.connect ~sw ~connect_env ?env ~tweaks_version uri
                    :> (connection, _) result Fiber.t)
                  >>=? fun conn -> post_connect conn
                  >|=? fun () -> conn)
        in
        let disconnect (module Db : CONNECTION) = Db.disconnect () in
        let validate (module Db : CONNECTION) = Db.validate () in
        let check (module Db : CONNECTION) = Db.check in
        let di = Driver.driver_info in
        let max_size, max_idle_size =
          (match Caqti_driver_info.can_concur di, Caqti_driver_info.can_pool di,
                 max_idle_size with
           | true, true, _ ->     max_size, max_idle_size
           | true, false, _ ->    max_size, Some 0
           | false, true, Some 0 -> Some 1, Some 0
           | false, true, _ ->      Some 1, Some 1
           | false, false, _ ->     Some 1, Some 0)
        in
        let pool =
          Pool.create
            ?max_size ?max_idle_size ?max_idle_age ~max_use_count
            ~validate ~check ~sw ~connect_env
            connect disconnect
        in
        let hook = Switch.on_release_cancellable sw (fun () -> Pool.drain pool) in
        Gc.finalise (fun _ -> Switch.remove_hook hook) pool;
        Ok pool
     | Error err ->
        Error err)
end
