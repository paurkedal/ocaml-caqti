(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
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

module System = System

module Make (Stdenv : System.STDENV) = struct
  module System_with_net = System.Make_with_net (Stdenv)
  module Loader = Caqti_platform_net.Driver_loader.Make (System_with_net)
  include Connector.Make_connect (System.Core) (Loader)
end

include Connector.Make_without_connect (System.Core)

let connect ?env ?config ?tweaks_version ~sw stdenv uri =
  Switch.check sw;
  let module Connector = Make (struct let stdenv = stdenv let sw = sw end) in
  let-? connection = Connector.connect ?env ?config ?tweaks_version uri in
  let module C = (val connection : CONNECTION) in
  Switch.on_release sw C.disconnect;
  connection

let connect_pool
      ?max_size ?max_idle_size ?max_use_count
      ?post_connect ?env ?config ?tweaks_version
      ~sw stdenv uri =
  Switch.check sw;
  let module Connector = Make (struct let stdenv = stdenv let sw = sw end) in
  let-? pool =
    Connector.connect_pool
      ?max_size ?max_idle_size ?max_use_count
      ?post_connect ?env ?config ?tweaks_version
      uri
  in
  Switch.on_release sw (fun () -> Pool.drain pool);
  pool

let with_connection ?env ?config ?tweaks_version stdenv uri f =
  Switch.run begin fun sw ->
    let/? connection = connect ?env ?config ?tweaks_version stdenv ~sw uri in
    f connection
  end

let or_fail = function
 | Ok x -> x
 | Error (#Caqti_error.t as err) -> raise (Caqti_error.Exn err)
