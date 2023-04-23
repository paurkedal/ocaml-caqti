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

open Lwt.Syntax
open Caqti_platform

module Make
  (RANDOM : Mirage_random.S)
  (TIME : Mirage_time.S)
  (MCLOCK : Mirage_clock.MCLOCK)
  (PCLOCK : Mirage_clock.PCLOCK)
  (STACK : Tcpip.Stack.V4V6)
  (DNS : Dns_client_mirage.S) =
struct
  module Channel = Mirage_channel.Make (STACK.TCP)
  module TCP = Conduit_mirage.TCP (STACK)

  module System = struct
    include Caqti_lwt.System

    module Pool = Caqti_platform.Pool.Make (Caqti_lwt.System)

    type connect_env = {
      stack: STACK.t;
      dns: DNS.t;
    }

    module Net = struct

      module Sockaddr = struct
        type t = [`Tcp of Ipaddr.t * int | `Unix of string]
        let unix s = `Unix s
        let tcp (host, port) = `Tcp (host, port)
      end

      type in_channel = Channel.t
      type out_channel = Channel.t

      let getaddrinfo_ipv4 dns host port =
        let extract (_, ips) =
          Ipaddr.V4.Set.elements ips
            |> List.map (fun ip -> `Tcp (Ipaddr.V4 ip, port))
        in
        DNS.getaddrinfo dns Dns.Rr_map.A host >|= Result.map extract

      let getaddrinfo_ipv6 dns host port =
        let extract (_, ips) =
          Ipaddr.V6.Set.elements ips
            |> List.map (fun ip -> `Tcp (Ipaddr.V6 ip, port))
        in
        DNS.getaddrinfo dns Dns.Rr_map.Aaaa host >|= Result.map extract

      let getaddrinfo ~connect_env:{stack; dns} host port =
        let laddrs = STACK.IP.get_ip (STACK.ip stack) in
        (match
          List.exists Ipaddr.(function V4 _ -> true | V6 _ -> false) laddrs,
          List.exists Ipaddr.(function V4 _ -> false | V6 _ -> true) laddrs
         with
         | true, true ->
            getaddrinfo_ipv4 dns host port >>= fun r4 ->
            getaddrinfo_ipv6 dns host port >|= fun r6 ->
            (match r4, r6 with
             | Ok addrs4, Ok addrs6 -> Ok (addrs4 @ addrs6)
             | Ok addrs, Error _ | Error _, Ok addrs -> Ok addrs
             | Error (`Msg msg4), Error (`Msg msg6) ->
                if String.equal msg4 msg6 then Error (`Msg msg4) else
                Error (`Msg ("IPv4: " ^ msg4 ^ " IPv6: " ^ msg6)))
         | true, false -> getaddrinfo_ipv4 dns host port
         | false, true -> getaddrinfo_ipv6 dns host port
         | false, false ->
            return (Error (`Msg "No IP address assigned to host.")))

      let connect ~connect_env:{stack; _} sockaddr =
        (match sockaddr with
         | `Unix _ ->
            Lwt.return_error
              (`Msg "Unix sockets are not available under MirageOS.")
         | `Tcp (ipaddr, port) ->
            let+ flow = TCP.connect stack (`TCP (ipaddr, port)) in
            let ch = Channel.create flow in
            Ok (ch, ch))

      let output_char oc c =
        Channel.write_char oc c;
        Lwt.return_unit

      let output_string oc s =
        Channel.write_string oc s 0 (String.length s);
        Lwt.return_unit

      let flush oc =
        Channel.flush oc >>= function
         | Ok () -> Lwt.return_unit
         | Error err ->
            Lwt.fail_with (Format.asprintf "%a" Channel.pp_write_error err)

      let input_char ic =
        Channel.read_char ic >>= function
         | Ok (`Data c) -> Lwt.return c
         | Ok `Eof -> Lwt.fail End_of_file
         | Error err ->
            Lwt.fail_with (Format.asprintf "%a" Channel.pp_error err)

      let really_input ic buf off len =
        Channel.read_exactly ~len ic >>= function
         | Ok (`Data bufs) ->
            let content = Cstruct.copyv bufs in
            Bytes.blit_string content 0 buf off len;
            Lwt.return_unit
         | Ok `Eof -> Lwt.fail End_of_file
         | Error err ->
            Lwt.fail_with (Format.asprintf "%a" Channel.pp_error err)

      let close_in oc =
        Channel.close oc >>= function
         | Ok () -> Lwt.return_unit
         | Error err ->
            Lwt.fail_with (Format.asprintf "%a" Channel.pp_write_error err)
    end
  end

  module Loader = Caqti_platform_net.Driver_loader.Make (System)

  include Connector.Make (System) (Loader)

  let connect ?env ?tweaks_version stack dns uri =
    connect ?env ?tweaks_version ~connect_env:{stack; dns} uri

  let with_connection ?env ?tweaks_version stack dns uri f =
    with_connection ?env ?tweaks_version ~connect_env:{stack; dns} uri f

  let connect_pool
        ?max_size ?max_idle_size ?max_use_count ?post_connect
        ?env ?tweaks_version stack dns uri =
    connect_pool
      ?max_size ?max_idle_size ?max_use_count ?post_connect
      ?env ?tweaks_version ~connect_env:{stack; dns} uri

end
