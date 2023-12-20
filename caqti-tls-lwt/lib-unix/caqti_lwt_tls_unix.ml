(* Copyright (C) 2023  Petter A. Urkedal <paurkedal@gmail.com>
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

open Lwt.Syntax
open Caqti_lwt_unix

module TLS_provider = struct
  type tls_config = Tls.Config.client
  let tls_config_key = Caqti_tls.Config.client

  let start_tls ~config ?host fd =
    let+ session = Tls_lwt.Unix.client_of_fd config ?host fd in
    Ok (Tls_lwt.of_t session)
end

let () =
  System.Net.tls_providers := (module TLS_provider) :: !System.Net.tls_providers
