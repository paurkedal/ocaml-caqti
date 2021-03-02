(* Copyright (C) 2014--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

  let (>>=?) m f = m >>= function Ok x -> f x | Error _ as r -> return r
  let (>|=?) m f = m >|= function Ok x -> (Ok (f x)) | Error _ as r -> r

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

  let with_connection uri f =
    connect uri >>=? fun ((module Db) as conn) ->
    try
      f conn >>= fun result -> Db.disconnect () >|= fun () -> result
    with exn ->
      Db.disconnect () >|= fun () -> raise exn

  module Pool = Caqti_pool.Make (System)
  module Stream = System.Stream

  let connect_pool ?max_size ?max_idle_size ?post_connect uri =
    let check_arg cond =
      if not cond then invalid_arg "Caqti_connect.Make_unix.connect_pool"
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
                (Driver.connect uri :> (connection, _) result future)
           | Some post_connect ->
              fun () ->
                (Driver.connect uri :> (connection, _) result future)
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
          Pool.create ?max_size ?max_idle_size ~validate ~check
            connect disconnect
        in
        Ok pool
     | Error err ->
        Error err)
end
