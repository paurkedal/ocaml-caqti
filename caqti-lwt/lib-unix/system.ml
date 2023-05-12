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

module System_core = struct
  include Caqti_lwt.System_core
  type connect_env = unit
end
include System_core

module Alarm = struct
  open Lwt.Infix

  type t = {cancel: unit -> unit}

  let schedule ~sw:_ ~connect_env:() t f =
    let t_now = Mtime_clock.now () in
    if Mtime.is_later t ~than:t_now then
      begin
        f ();
        {cancel = Fun.id}
      end
    else
      let task =
        Lwt_unix.sleep (Mtime.Span.to_float_ns (Mtime.span t t_now) *. 1e-9)
          >|= f
      in
      {cancel = (fun () -> Lwt.cancel task)}

  let unschedule alarm = alarm.cancel ()
end

module Stream = Caqti_lwt.Stream
module Pool = Caqti_platform.Pool.Make (System_core) (Alarm)

module Net = struct

  module Sockaddr = struct
    type t = Unix.sockaddr
    let unix s = Unix.ADDR_UNIX s
    let tcp (addr, port) =
      Unix.ADDR_INET (Unix.inet_addr_of_string (Ipaddr.to_string addr), port)
  end

  type in_channel = Lwt_io.input_channel
  type out_channel = Lwt_io.output_channel

  let getaddrinfo ~connect_env:() host port =
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

  let connect ~sw:_ ~connect_env:() sockaddr =
    Lwt.catch
      (fun () -> Lwt_io.open_connection sockaddr >|= Result.ok)
      (function
       | Unix.Unix_error (code, _, _) ->
          Lwt.return_error
            (`Msg ("Cannot connect: " ^ Unix.error_message code))
       | exn -> Lwt.fail exn)

  let output_char = Lwt_io.write_char
  let output_string = Lwt_io.write
  let flush = Lwt_io.flush
  let input_char = Lwt_io.read_char
  let really_input = Lwt_io.read_into_exactly
  let close_in = Lwt_io.close
end
