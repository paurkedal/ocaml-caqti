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

open Lwt.Infix
open Caqti_platform

module type SOCKET_OPS =
  Caqti_platform.System_sig.SOCKET_OPS with type 'a fiber := 'a Lwt.t

module Make
  (RANDOM : Mirage_random.S)
  (TIME : Mirage_time.S)
  (MCLOCK : Mirage_clock.MCLOCK)
  (PCLOCK : Mirage_clock.PCLOCK)
  (STACK : Tcpip.Stack.V4V6)
  (DNS : Dns_client_mirage.S) =
struct
  module TCP = STACK.TCP
  module TLS = Tls_mirage.Make (TCP)
  module TCP_channel = Mirage_channel.Make (TCP)
  module TLS_channel = Mirage_channel.Make (TLS)

  module System_core = struct
    include Caqti_lwt.System_core

    type stdenv = {
      stack: STACK.t;
      dns: DNS.t;
    }
  end

  module Alarm = struct
    type t = {cancel: unit -> unit}

    let schedule ~sw ~stdenv:_ t f =
      let t_now = Mtime_clock.now () in
      let dt_ns =
        if Mtime.is_later t ~than:t_now then 0L else
        Mtime.Span.to_uint64_ns (Mtime.span t t_now)
      in
      let task = TIME.sleep_ns dt_ns >|= f in
      let hook =
        Caqti_lwt.Switch.on_release_cancellable sw
          (fun () -> Lwt.cancel task; Lwt.return_unit)
      in
      {cancel = (fun () -> Caqti_lwt.Switch.remove_hook hook; Lwt.cancel task)}

    let unschedule alarm = alarm.cancel ()
  end

  module Pool = Caqti_platform.Pool.Make (System_core) (Alarm)

  module System = struct
    include System_core
    module Pool = Pool

    module Net = struct

      module Sockaddr = struct
        type t = [`Tcp of Ipaddr.t * int | `Unix of string]
        let unix s = `Unix s
        let tcp (host, port) = `Tcp (host, port)
      end

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

      let getaddrinfo ~stdenv:{stack; dns} host port =
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
            Lwt.return (Error (`Msg "No IP address assigned to host.")))

      module Make_stream_ops (Channel : Mirage_channel.S) = struct
        type t = Channel.t

        let output_char channel c =
          Channel.write_char channel c;
          Lwt.return_unit

        let output_string channel s =
          Channel.write_string channel s 0 (String.length s);
          Lwt.return_unit

        let flush channel =
          Channel.flush channel >>= function
           | Ok () -> Lwt.return_unit
           | Error err ->
              Lwt.fail_with (Format.asprintf "%a" Channel.pp_write_error err)

        let input_char channel =
          Channel.read_char channel >>= function
           | Ok (`Data c) -> Lwt.return c
           | Ok `Eof -> Lwt.fail End_of_file
           | Error err ->
              Lwt.fail_with (Format.asprintf "%a" Channel.pp_error err)

        let really_input channel buf off len =
          Channel.read_exactly ~len channel >>= function
           | Ok (`Data bufs) ->
              let content = Cstruct.copyv bufs in
              Bytes.blit_string content 0 buf off len;
              Lwt.return_unit
           | Ok `Eof -> Lwt.fail End_of_file
           | Error err ->
              Lwt.fail_with (Format.asprintf "%a" Channel.pp_error err)

        let close channel =
          Channel.close channel >>= function
           | Ok () -> Lwt.return_unit
           | Error err ->
              Lwt.fail_with (Format.asprintf "%a" Channel.pp_write_error err)
      end

      module TCP_stream_ops = Make_stream_ops (TCP_channel)
      module TLS_stream_ops = Make_stream_ops (TLS_channel)

      module Socket = struct
        type t = V : {
          tcp_flow: TCP_channel.flow option;
          ops: (module SOCKET_OPS with type t = 'a);
          channel: 'a;
        } -> t

        let output_char (V {ops = (module Ops); channel; _}) =
          Ops.output_char channel
        let output_string (V {ops = (module Ops); channel; _}) =
          Ops.output_string channel
        let flush (V {ops = (module Ops); channel; _}) =
          Ops.flush channel
        let input_char (V {ops = (module Ops); channel; _}) =
          Ops.input_char channel
        let really_input (V {ops = (module Ops); channel; _}) =
          Ops.really_input channel
        let close (V {ops = (module Ops); channel; _}) =
          Ops.close channel
      end

      type tcp_flow = TCP_channel.flow
      type tls_flow = Tls_flow : {
        ops: (module SOCKET_OPS with type t = 'a);
        channel: 'a;
      } -> tls_flow

      let connect_tcp ~sw:_ ~stdenv:{stack; _} sockaddr =
        (match sockaddr with
         | `Unix _ ->
            Lwt.return_error
              (`Msg "Unix sockets are not available under MirageOS.")
         | `Tcp (ipaddr, port) ->
            TCP.create_connection (STACK.tcp stack) (ipaddr, port) >|=
            (function
             | Ok flow ->
                let channel = TCP_channel.create flow in
                Ok (Socket.V {
                  tcp_flow = Some flow;
                  ops = (module TCP_stream_ops);
                  channel;
                })
             | Error err ->
                let msg = Format.asprintf "%a" TCP.pp_error err in
                Error (`Msg msg)))

      let tcp_flow_of_socket (Socket.V {tcp_flow; _}) = tcp_flow

      let socket_of_tls_flow ~sw:_ (Tls_flow {ops; channel}) =
        Socket.V {tcp_flow = None; ops; channel}

      module type TLS_PROVIDER = Caqti_platform.System_sig.TLS_PROVIDER
        with type 'a fiber := 'a Lwt.t
         and type tcp_flow := tcp_flow
         and type tls_flow := tls_flow

      module Tls_provider = struct
        type tls_config = Tls.Config.client

        let tls_config_key = Caqti_tls.Config.client

        let start_tls ~config ?host flow =
          TLS.client_of_flow config ?host flow >|=
          (function
           | Ok tls_flow ->
              Ok (Tls_flow {
                ops = (module TLS_stream_ops);
                channel = TLS_channel.create tls_flow;
              })
           | Error err ->
              let msg = Format.asprintf "%a" TLS.pp_write_error err in
              Error (`Msg msg))
      end

      let tls_providers : (module TLS_PROVIDER) list ref =
        ref [(module Tls_provider : TLS_PROVIDER)]

    end
  end

  module Loader = Caqti_platform.Driver_loader.Make (System)

  include Connector.Make (System) (Pool) (Loader)

  let connect
        ?env ?config ?tweaks_version ?(sw = Caqti_lwt.Switch.eternal)
        stack dns uri =
    connect ?env ?config ?tweaks_version ~sw ~stdenv:{stack; dns} uri

  let with_connection ?env ?config ?tweaks_version stack dns uri f =
    with_connection ?env ?config ?tweaks_version ~stdenv:{stack; dns} uri f

  let connect_pool
        ?pool_config ?post_connect ?env ?config ?tweaks_version
        ?(sw = Caqti_lwt.Switch.eternal) stack dns uri =
    connect_pool
      ?pool_config ?post_connect ?env ?config ?tweaks_version
      ~sw ~stdenv:{stack; dns} uri

end
