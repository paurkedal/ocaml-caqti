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

open Caqti_private
open Caqti_private.Std
open Lwt.Syntax

module Future = struct
  type 'a future = 'a Lwt.t
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
  let return = Lwt.return
end

module System_common = struct
  include Future

  let catch = Lwt.catch
  let finally = Lwt.finalize
  let cleanup f g = Lwt.catch f (fun exn -> g () >>= fun () -> Lwt.fail exn)
  let join = Lwt.join

  module Mvar = struct
    type 'a t = 'a Lwt_mvar.t
    let create = Lwt_mvar.create_empty
    let store x v = Lwt.async (fun () -> Lwt_mvar.put v x)
    let fetch = Lwt_mvar.take
  end

  module Log = struct
    type 'a log = 'a Logs_lwt.log
    let err ?(src = default_log_src) = Logs_lwt.err ~src
    let warn ?(src = default_log_src) = Logs_lwt.warn ~src
    let info ?(src = default_log_src) = Logs_lwt.info ~src
    let debug ?(src = default_log_src) = Logs_lwt.debug ~src
  end

  module Stream = Caqti_stream.Make (Future)
end

module System_except_networking = struct

  include System_common

  module Sequencer = struct
    type 'a t = 'a * Lwt_mutex.t
    let create m = (m, Lwt_mutex.create ())
    let enqueue (m, mutex) f = Lwt_mutex.with_lock mutex (fun () -> f m)
  end

end

include System_except_networking
include Connector.Make_without_connect (System_common)

module type S = Caqti_connect_sig.S
  with type 'a future := 'a Lwt.t
   and module Stream = Stream

module Make
  (RANDOM : Mirage_random.S)
  (TIME : Mirage_time.S)
  (MCLOCK : Mirage_clock.MCLOCK)
  (PCLOCK : Mirage_clock.PCLOCK)
  (STACK : Tcpip.Stack.V4V6) =
struct
  module Channel = Mirage_channel.Make (STACK.TCP)
  module Dns = Dns_client_mirage.Make (RANDOM) (TIME) (MCLOCK) (PCLOCK) (STACK)
  module TCP = Conduit_mirage.TCP (STACK)

  module System (Arg : sig val stack : STACK.t end) = struct
    include System_except_networking

    module Networking = struct

      type sockaddr =
        | Unix of string
        | Inet of string * int

      type in_channel = Channel.t
      type out_channel = Channel.t

      let open_connection sockaddr =
        let dns = Dns.create Arg.stack in
        let* client =
          (match sockaddr with
           | Unix _ ->
              Lwt.fail_with "Unix sockets are not available under MirageOS. "
           | Inet (host, port) ->
              (match Ipaddr.of_string host with
               | Ok ipaddr -> Lwt.return (`TCP (ipaddr, port))
               | Error _ ->
                  let host' = host
                    |> Domain_name.of_string_exn
                    |> Domain_name.host_exn
                  in
                  Dns.gethostbyname dns host'
                  >>= (function
                   | Ok ipaddr -> Lwt.return (`TCP (Ipaddr.V4 ipaddr, port))
                   | Error (`Msg msg) -> Lwt.fail_with msg)))
        in
        let+ flow = TCP.connect Arg.stack client in
        let ch = Channel.create flow in
        (ch, ch)

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

  module type CONNECT = Caqti_connect_sig.Connect
    with type 'a future := 'a future
     and type connection := connection
     and type ('a, 'e) pool := ('a, 'e) Pool.t
     and type 'a connect_fun :=
      ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
      ?tweaks_version: int * int ->
      Uri.t -> 'a

  let connect_stack stack =
    let module System = System (struct let stack = stack end) in
    let module Loader = struct
      module Platform_net = Caqti_platform_net.Driver_loader.Make (System)

      module type DRIVER = Platform_net.DRIVER

      let load_driver = Platform_net.load_driver
    end in
    (module Connector.Make_connect (System_common) (Loader) : CONNECT)

  let connect ?env ?tweaks_version stack uri =
    let module C = (val connect_stack stack) in
    C.connect ?env ?tweaks_version uri

  let with_connection ?env ?tweaks_version stack uri f =
    let module C = (val connect_stack stack) in
    C.with_connection ?env ?tweaks_version uri f

  let connect_pool
        ?max_size ?max_idle_size ?max_use_count
        ?post_connect ?env ?tweaks_version
        stack uri =
    let module C = (val connect_stack stack) in
    C.connect_pool
      ?max_size ?max_idle_size ?max_use_count ?post_connect ?env ?tweaks_version
      uri

end
