(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

module System = struct
  include Caqti_lwt.System

  module Net = struct

    module Sockaddr = struct
      type t = Unix.sockaddr
      let unix s = Unix.ADDR_UNIX s
      let tcp (addr, port) =
        Unix.ADDR_INET (Unix.inet_addr_of_string (Ipaddr.to_string addr), port)
    end

    type in_channel = Lwt_io.input_channel
    type out_channel = Lwt_io.output_channel

    let getaddrinfo host port =
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

    let connect sockaddr =
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

  module Preemptive = Lwt_preemptive

  module Unix = struct
    type file_descr = Lwt_unix.file_descr

    let wrap_fd f fd = f (Lwt_unix.of_unix_file_descr fd)

    let poll ?(read = false) ?(write = false) ?timeout fd =
      let choices = []
        |> (fun acc -> if read then Lwt_unix.wait_read fd :: acc else acc)
        |> (fun acc -> if write then Lwt_unix.wait_write fd :: acc else acc)
        |> Option.fold
            ~none:Fun.id ~some:(fun t acc -> Lwt_unix.timeout t :: acc) timeout
      in
      if choices = [] then
        Lwt.fail_invalid_arg "Caqti_lwt.Unix.poll: No operation specified."
      else
      Lwt.catch
        (fun () -> Lwt.choose choices >|= fun _ -> false)
        (function
         | Lwt_unix.Timeout -> Lwt.return_true
         | exn -> Lwt.fail exn)
        >|= fun timed_out ->
      (Lwt_unix.readable fd, Lwt_unix.writable fd, timed_out)
  end
end

module Loader = struct
  module Platform_unix = Caqti_platform_unix.Driver_loader.Make (System)
  module Platform_net = Caqti_platform_net.Driver_loader.Make (System)

  module type DRIVER = Platform_unix.DRIVER

  let load_driver ~uri scheme =
    (match Platform_net.load_driver ~uri scheme with
     | Ok _ as r -> r
     | Error (`Load_rejected _) as r -> r
     | Error (`Load_failed _) ->
        (* TODO: Summarize errors. *)
        Platform_unix.load_driver ~uri scheme)
end

include Connector.Make_connect (Caqti_lwt.System) (Loader)
