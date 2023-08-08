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

open Lwt.Infix

module System_core = struct
  include Caqti_lwt.System_core
  type stdenv = unit
end
include System_core

module Alarm = struct

  type t = {cancel: unit -> unit}

  let schedule ~sw:_ ~stdenv:() t f =
    let t_now = Mtime_clock.now () in
    let delay =
      if Mtime.is_later t ~than:t_now then
        Lwt.pause ()
      else
        Lwt_unix.sleep (Mtime.Span.to_float_ns (Mtime.span t t_now) *. 1e-9)
    in
    let task = delay >|= f in
    {cancel = (fun () -> Lwt.cancel task)}

  let unschedule alarm = alarm.cancel ()
end

module Stream = Caqti_lwt.Stream
module Pool = Caqti_platform.Pool.Make (System_core) (Alarm)

module Net = struct

  type socket =
    Lwt_unix.file_descr * Lwt_io.input_channel * Lwt_io.output_channel

  module type SOCKET_OPS = Caqti_platform.System_sig.SOCKET_OPS
    with type 'a fiber := 'a Lwt.t
     and type t = socket

  module Sockaddr = struct
    type t = Unix.sockaddr
    let unix s = Unix.ADDR_UNIX s
    let tcp (addr, port) =
      Unix.ADDR_INET (Unix.inet_addr_of_string (Ipaddr.to_string addr), port)
  end

  let getaddrinfo ~stdenv:() host port =
    Lwt.catch
      (fun () ->
        let opts = Unix.[AI_SOCKTYPE SOCK_STREAM] in
        Lwt_unix.getaddrinfo
          (Domain_name.to_string host) (string_of_int port) opts
          >|= List.map (fun ai -> ai.Unix.ai_addr) >|= Result.ok)
      (function
       | Not_found -> Lwt.return_ok []
       | Unix.Unix_error (code, _, _) ->
          Lwt.return_error
            (`Msg ("Cannot resolve host name: " ^ Unix.error_message code))
       | exn -> Lwt.fail exn)

  module Socket = struct
    type t = Lwt_unix.file_descr * Lwt_io.input_channel * Lwt_io.output_channel

    let output_char (_, _, oc) data = Lwt_io.write_char oc data
    let output_string (_, _, oc) data = Lwt_io.write oc data
    let flush (_, _, oc) = Lwt_io.flush oc
    let input_char (_, ic, _) = Lwt_io.read_char ic
    let really_input (_, ic, _) data offset length =
      Lwt_io.read_into_exactly ic data offset length
    let close (_, _, oc) = Lwt_io.close oc (* CHECKME *)
  end

  type tcp_flow = Lwt_unix.file_descr
  type tls_flow = Socket.t

  let connect_tcp ~sw:_ ~stdenv:() sockaddr =
    let domain = Unix.domain_of_sockaddr sockaddr in
    let fd = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
    Lwt.catch
      (fun () ->
        (try Lwt_unix.set_close_on_exec fd with _ -> ());
        Lwt_unix.connect fd sockaddr >|= fun () ->
        let ic = Lwt_io.(of_fd ~mode:input) fd in
        let oc = Lwt_io.(of_fd ~mode:output) fd in
        Ok (fd, ic, oc))
      (function
       | Unix.Unix_error (err, fn, _) ->
          Lwt_unix.close fd >|= fun () ->
          Error (`Msg (fn ^ ": " ^ Unix.error_message err))
       | exn ->
          Lwt_unix.close fd >>= fun () ->
          Lwt.fail exn)

  let tcp_flow_of_socket (fd, _, _) = Some fd

  let socket_of_tls_flow ~sw:_ = Fun.id

  module type TLS_PROVIDER = Caqti_platform.System_sig.TLS_PROVIDER
    with type 'a fiber := 'a Lwt.t
     and type tcp_flow := tcp_flow
     and type tls_flow := tls_flow

  let tls_providers : (module TLS_PROVIDER) list ref = ref []
end
