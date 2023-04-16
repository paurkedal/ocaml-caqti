(* Copyright (C) 2022--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eio.Std
open Caqti_platform

let ( let/? ) r f = Result.bind r f
let ( let-? ) r f = Result.map f r

include Connector.Make_without_connect (Caqti_eio.System.Core)

open Caqti_eio.System.Core

module System_with_net = Caqti_eio.System.With_net
module Loader_net = Caqti_platform_net.Driver_loader.Make (System_with_net)

module System_with_unix = System
module Loader_unix = Caqti_platform_unix.Driver_loader.Make (System_with_unix)

module Loader = struct

  type connect_env = System_with_net.connect_env

  module type DRIVER = Loader_unix.DRIVER

  let load_driver ~uri scheme =
    (match Loader_net.load_driver ~uri scheme with
     | Ok _ as r -> r
     | Error (`Load_rejected _) as r -> r
     | Error (`Load_failed _) ->
        (* TODO: Summarize errors. *)
        Loader_unix.load_driver ~uri scheme)

end

include Connector.Make_connect (Caqti_eio.System.Core) (Loader)

let connect ?env ?tweaks_version ~sw stdenv uri =
  let connect_env = {stdenv; sw} in
  Switch.check sw;
  let-? connection = connect ~connect_env ?env ?tweaks_version uri in
  let module C = (val connection : CONNECTION) in
  Switch.on_release sw C.disconnect;
  connection

let connect_pool
      ?max_size ?max_idle_size ?max_use_count
      ?post_connect ?env ?tweaks_version
      ~sw stdenv uri =
  let connect_env = {stdenv; sw} in
  Switch.check sw;
  let-? pool =
    connect_pool ~connect_env
      ?max_size ?max_idle_size ?max_use_count ?post_connect ?env ?tweaks_version
      uri
  in
  Switch.on_release sw (fun () -> Pool.drain pool);
  pool

let with_connection ?env ?tweaks_version stdenv uri f =
  Switch.run begin fun sw ->
    let/? connection = connect ?env ?tweaks_version stdenv ~sw uri in
    f connection
  end

let or_fail = function
 | Ok x -> x
 | Error (#Caqti_error.t as err) -> raise (Caqti_error.Exn err)
