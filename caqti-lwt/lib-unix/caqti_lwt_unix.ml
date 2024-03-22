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

open Caqti_platform

module System = System
module Pool = System.Pool

module Loader = Caqti_platform_unix.Driver_loader.Make (System) (System_unix)

include Connector.Make (System) (Pool) (Loader)

let connect
      ?subst ?env ?config ?tweaks_version ?(sw = Caqti_lwt.Switch.eternal) uri =
  connect ?subst ?env ?config ?tweaks_version ~sw ~stdenv:() uri

let with_connection = with_connection ~stdenv:()

let connect_pool
      ?pool_config ?post_connect ?subst ?env ?config ?tweaks_version
      ?(sw = Caqti_lwt.Switch.eternal) uri =
  connect_pool
    ?pool_config ?post_connect ?subst ?env ?config ?tweaks_version
    ~sw ~stdenv:() uri
