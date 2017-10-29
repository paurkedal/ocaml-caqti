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
open Printf

let scheme_plugins = Hashtbl.create 11
let register_scheme scheme p = Hashtbl.add scheme_plugins scheme p

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
    let module Make_driver = (val driver_functor : Caqti_driver_sig.FUNCTOR) in
    let module Driver = Make_driver (System) in
    let driver = (module Driver : DRIVER) in
    Hashtbl.add drivers scheme driver; driver

  module type CONNECTION = CONNECTION with type 'a io = 'a System.io

  let connect uri : (module CONNECTION) System.io =
    match Uri.scheme uri with
    | None ->
      fail (Invalid_argument (sprintf "Cannot use schemeless URI %s"
                             (Uri.to_string uri)))
    | Some scheme ->
      try
        let driver = load_driver scheme in
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

  let connect_v2 uri : (module CONNECTION_V2) System.io =
    (match Uri.scheme uri with
     | None -> failwith "caqti_*.connect: Missing URI scheme."
     | Some scheme ->
        try
          let driver = load_driver scheme in
          let module Caqtus = (val driver) in
          connect uri >|= fun c ->
          let module C = (val c) in
          (module Caqti_compat.Connection_v2_of_v1 (System) (C) : CONNECTION_V2)
        with xc -> fail xc)
end
