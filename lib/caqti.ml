(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

open Caqti_metadata
open Caqti_plugin
open Caqti_query
open Caqti_sigs
open Printf

type query_info =
  [ `Oneshot of string
  | `Prepared of string * string ]

let make_query_info backend_info = function
  | Oneshot qsf -> `Oneshot (qsf backend_info)
  | Prepared {pq_name; pq_encode} -> `Prepared (pq_name, pq_encode backend_info)

exception Connect_failed of Uri.t * string
exception Prepare_failed of Uri.t * query_info * string
exception Execute_failed of Uri.t * query_info * string
exception Miscommunication of Uri.t * query_info * string

let scheme_plugins = Hashtbl.create 11
let register_scheme scheme p = Hashtbl.add scheme_plugins scheme p

module Make (System : SYSTEM) = struct
  open System

  type 'a io = 'a System.io

  module type CONNECTION = CONNECTION with type 'a io = 'a System.io
  module type CONNECTOR = CONNECT_BASE with type 'a io = 'a System.io

  let connectors : (string, (module CONNECTOR)) Hashtbl.t = Hashtbl.create 11

  let load_connector scheme =
    try Hashtbl.find connectors scheme with Not_found ->
    let plugin =
      ensure_plugin
	(fun () -> try Some (Hashtbl.find scheme_plugins scheme)
		   with Not_found -> None)
	["caqtus-" ^ scheme; "caqti.caqtus-" ^ scheme] in
    let module Plugin = (val plugin : CONNECT_FUNCTOR) in
    let module Connector = Plugin.Make (System) in
    let connector = (module Connector : CONNECTOR) in
    Hashtbl.add connectors scheme connector; connector

  let connect uri : (module CONNECTION) System.io =
    match Uri.scheme uri with
    | None ->
      fail (Invalid_argument (sprintf "Cannot use schemeless URI %s"
			     (Uri.to_string uri)))
    | Some scheme ->
      try
	let connector = load_connector scheme in
	let module Connector = (val connector) in
	Connector.connect uri >>= fun client ->
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
