(* Copyright (C) 2023--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_eio

module TLS_provider = struct
  type tls_config = Tls.Config.client
  let tls_config_key = Caqti_tls.Config.client

  let start_tls ~config ?host tcp_flow =
    (* As of tls-eio.0.17.3, Eio_tls.t does not provide the Close provider
     * interface, and Shutdown does not close the underlying TCP connection, so
     * we inject it into the handler.  Also, the tcp_flow is smuggled into the
     * implementation to avoid slowing down operations with a layer of wrapper
     * functions to unpack a (tls_flow, tcp_flow)-pair. *)
    let Eio.Resource.T (tls_flow, handler) = Tls_eio.client_of_flow config ?host tcp_flow in
    let source = Eio.Resource.get handler Eio.Flow.Pi.Source in
    let sink = Eio.Resource.get handler Eio.Flow.Pi.Sink in
    let shutdown = Eio.Resource.get handler Eio.Flow.Pi.Shutdown in
    let handler = Eio.Resource.handler [
      H (Eio.Flow.Pi.Source, source);
      H (Eio.Flow.Pi.Sink, sink);
      H (Eio.Flow.Pi.Shutdown, shutdown);
      H (Eio.Resource.Close, (fun _ -> Eio.Flow.close tcp_flow));
    ] in
    Ok (Eio.Resource.T (tls_flow, handler))
end

let () =
  System.Net.tls_providers := (module TLS_provider) :: !System.Net.tls_providers
