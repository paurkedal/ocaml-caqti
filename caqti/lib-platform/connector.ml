(* Copyright (C) 2014--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

let dynload_library = ref None

let define_loader load = dynload_library := Some load

let load_library lib =
  (match !dynload_library with
   | Some load -> load lib
   | None ->
      Error (Printf.sprintf "\
        Neither %s nor a dynamic loader is linked into the application." lib))

let library_name_of_scheme = function
 | "postgres" | "postgresql" -> "caqti-driver-postgresql"
 | s -> "caqti-driver-" ^ s

let set_tweaks_version = function
 | None -> Fun.id
 | Some x -> Caqti_connect_config.(set tweaks_version) x

module Make
  (System : System_sig.S)
  (Pool : Pool.S
    with type 'a fiber := 'a System.Fiber.t
     and type switch := System.Switch.t
     and type stdenv := System.stdenv)
  (Loader : Driver_loader.S
    with type 'a fiber := 'a System.Fiber.t
     and type switch := System.Switch.t
     and type stdenv := System.stdenv
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

  module type DRIVER = Driver_loader.DRIVER
    with type 'a fiber := 'a Fiber.t
     and type ('a, 'err) stream := ('a, 'err) Stream.t
     and type switch := System.Switch.t
     and type stdenv := System.stdenv

  let drivers : (string, (module DRIVER)) Hashtbl.t = Hashtbl.create 11

  let message_cont : (_, _, _, _) format4 =
    if Loader.provides_unix then
      "Your entry point provides both the networking and unix components."
    else
      "Your entry point provides the networking but not the unix component, \
       which is required by drivers based on C bindings."

  let message_static = Printf.sprintf
    ("A suitable driver for the URI-scheme %s was not found. " ^^ message_cont)

  let message_dynamic = Printf.sprintf
    ("A suitable driver for the URI-scheme %s was not found \
      after linking in %s. " ^^ message_cont)

  let load_driver' ~uri scheme =
    (match Loader.find_and_apply scheme with
     | Some driver -> Ok driver
     | None ->
        (match !dynload_library with
         | None ->
            let msg = message_static scheme in
            Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg))
         | Some load ->
            let driver_lib = library_name_of_scheme scheme in
            (match load driver_lib with
             | Ok () ->
                (match Loader.find_and_apply scheme with
                 | Some driver -> Ok driver
                 | None ->
                    let msg = message_dynamic scheme driver_lib in
                    Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg)))
             | Error msg ->
                Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg)))))

  let load_driver uri =
    (match Uri.scheme uri with
     | None ->
        let msg = "Missing URI scheme." in
        Error (Caqti_error.load_rejected ~uri (Caqti_error.Msg msg))
     | Some scheme ->
        (try Ok (Hashtbl.find drivers scheme) with
         | Not_found ->
            (match load_driver' ~uri scheme with
             | Ok driver ->
                Hashtbl.add drivers scheme driver;
                Ok driver
             | Error _ as r -> r)))

  let connect
        ?env ?(config = Caqti_connect_config.default)
        ?tweaks_version ~sw ~stdenv uri
      : ((module CONNECTION), _) result Fiber.t =
    let config = set_tweaks_version tweaks_version config in
    Switch.check sw;
    (match load_driver uri with
     | Ok driver ->
        let module Driver = (val driver) in
        let+? conn = Driver.connect ~sw ~stdenv ?env ~config uri in
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
        ?env ?config ?tweaks_version ~stdenv uri f =
    Switch.run begin fun sw ->
      connect ~sw ~stdenv ?env ?config ?tweaks_version uri >>=? f
    end

  let connect_pool
        ?pool_config ?post_connect ?env
        ?(config = Caqti_connect_config.default)
        ?tweaks_version ~sw ~stdenv uri =
    let pool_config =
      (match pool_config with
       | None -> Caqti_pool_config.default_from_env ()
       | Some pool_config -> pool_config)
    in
    let config = set_tweaks_version tweaks_version config in
    Switch.check sw;
    let check_arg cond =
      if not cond then invalid_arg "Caqti_connect.Make.connect_pool"
    in
    (match Caqti_pool_config.(get max_size) pool_config,
           Caqti_pool_config.(get max_idle_size) pool_config with
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
                (Driver.connect ~sw ~stdenv ?env ~config uri
                    :> (connection, _) result Fiber.t)
           | Some post_connect ->
              fun () ->
                (Driver.connect ~sw ~stdenv ?env ~config uri
                    :> (connection, _) result Fiber.t)
                  >>=? fun conn -> post_connect conn
                  >|=? fun () -> conn)
        in
        let disconnect (module Db : CONNECTION) = Db.disconnect () in
        let validate (module Db : CONNECTION) = Db.validate () in
        let check (module Db : CONNECTION) = Db.check in
        let di = Driver.driver_info in
        let pool_config =
          (match Caqti_driver_info.can_concur di,
                 Caqti_driver_info.can_pool di,
                 Caqti_pool_config.(get max_idle_size) pool_config with
           | true, true, _ ->
              pool_config
           | true, false, _ ->
              pool_config |> Caqti_pool_config.(set max_idle_size) 0
           | false, true, Some 0 ->
              pool_config
                |> Caqti_pool_config.(set max_size) 1
                |> Caqti_pool_config.(set max_idle_size) 0
           | false, true, _ ->
              pool_config
                |> Caqti_pool_config.(set max_size) 1
                |> Caqti_pool_config.(set max_idle_size) 1
           | false, false, _ ->
              pool_config
                |> Caqti_pool_config.(set max_size) 1
                |> Caqti_pool_config.(set max_idle_size) 0)
        in
        let pool =
          Pool.create
            ~config:pool_config ~validate ~check ~sw ~stdenv connect disconnect
        in
        let hook =
          Switch.on_release_cancellable sw (fun () -> Pool.drain pool)
        in
        Gc.finalise (fun _ -> Switch.remove_hook hook) pool;
        Ok pool
     | Error err ->
        Error err)
end
