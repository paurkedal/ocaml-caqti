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

module Make (System : SYSTEM) = struct
  open System

  module System = System

  module type CAQTUS = CAQTUS with type 'a io := 'a System.io

  let caqtuses : (string, (module CAQTUS)) Hashtbl.t = Hashtbl.create 11

  let load_caqtus scheme =
    try Hashtbl.find caqtuses scheme with Not_found ->
    let caqtus_functor =
      ensure_plugin
        (fun () -> try Some (Hashtbl.find scheme_plugins scheme)
                   with Not_found -> None)
        ("caqti-driver-" ^ scheme) in
    let module Caqtus_functor = (val caqtus_functor : CAQTUS_FUNCTOR) in
    let module Caqtus = Caqtus_functor (System) in
    let caqtus = (module Caqtus : CAQTUS) in
    Hashtbl.add caqtuses scheme caqtus; caqtus

  module type CONNECTION = CONNECTION with type 'a io = 'a System.io

  let connect uri : (module CONNECTION) System.io =
    match Uri.scheme uri with
    | None ->
      fail (Invalid_argument (sprintf "Cannot use schemeless URI %s"
                             (Uri.to_string uri)))
    | Some scheme ->
      try
        let caqtus = load_caqtus scheme in
        let module Caqtus = (val caqtus) in
        Caqtus.connect uri >>= fun client ->
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
end
