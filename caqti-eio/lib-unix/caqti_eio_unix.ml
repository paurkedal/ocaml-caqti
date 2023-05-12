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

module Loader = Caqti_platform_unix.Driver_loader.Make (Caqti_eio.System) (System_unix)

include Connector.Make (Caqti_eio.System) (Caqti_eio.Pool) (Loader)

let connect ?env ?tweaks_version ~sw stdenv uri =
  let connect_env = Caqti_eio.System.{stdenv; sw} in
  Switch.check sw;
  let-? connection = connect ~connect_env ?env ?tweaks_version uri in
  let module C = (val connection : CONNECTION) in
  Switch.on_release sw C.disconnect;
  connection

let connect_pool
      ?max_size ?max_idle_size ?max_idle_age ?max_use_count
      ?post_connect ?env ?tweaks_version
      ~sw stdenv uri =
  let connect_env = Caqti_eio.System.{stdenv; sw} in
  Switch.check sw;
  let-? pool =
    connect_pool ~connect_env
      ?max_size ?max_idle_size ?max_idle_age ?max_use_count
      ?post_connect ?env ?tweaks_version
      uri
  in
  Switch.on_release sw (fun () -> Caqti_eio.Pool.drain pool);
  pool

let with_connection ?env ?tweaks_version stdenv uri f =
  Switch.run begin fun sw ->
    let/? connection = connect ?env ?tweaks_version stdenv ~sw uri in
    f connection
  end
