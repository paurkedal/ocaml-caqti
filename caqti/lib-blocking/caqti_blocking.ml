(* Copyright (C) 2018--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

module Future = struct
  type 'a future = 'a
  let (>>=) x f = f x
  let (>|=) x f = f x
  let return x = x

  let catch f g = try f () with exn -> g exn

  let finally f g =
    (match f () with
     | y -> g (); y
     | exception exn -> g (); raise exn)
end

module Stream = Caqti_platform.Stream.Make (Future)

module System_core = struct
  include Future

  let cleanup f g = try f () with exn -> g (); raise exn

  module Switch = Caqti_platform.Switch.Make (Future)

  let async ~sw:_ f = f ()

  module Stream = Stream

  module Semaphore = struct
    type t = bool ref
    let create () = ref false
    let release v = v := true
    let acquire v =
      if not !v then
        failwith "Cannot acquire unreleased semaphore in blocking context."
  end

  module Log = struct
    type 'a log = 'a Logs.log
    let err ?(src = Logging.default_log_src) = Logs.err ~src
    let warn ?(src = Logging.default_log_src) = Logs.warn ~src
    let info ?(src = Logging.default_log_src) = Logs.info ~src
    let debug ?(src = Logging.default_log_src) = Logs.debug ~src
  end

  type connect_env = unit

  module Sequencer = struct
    type 'a t = 'a
    let create m = m
    let enqueue m f = f m
  end
end

module Pool = Caqti_platform.Pool.Make_without_alarm (System_core)

module System = struct
  include System_core

  module Net = struct
    module Sockaddr = struct
      type t = Unix.sockaddr
      let unix s = Unix.ADDR_UNIX s
      let tcp (addr, port) =
        Unix.ADDR_INET (Unix.inet_addr_of_string (Ipaddr.to_string addr), port)
    end

    type nonrec in_channel = in_channel
    type nonrec out_channel = out_channel

    let getaddrinfo ~connect_env:() host port =
      try
        let opts = Unix.[AI_SOCKTYPE SOCK_STREAM] in
        Unix.getaddrinfo (Domain_name.to_string host) (string_of_int port) opts
          |> List.map (fun ai -> ai.Unix.ai_addr) |> Result.ok
      with
       | Not_found -> Ok []
       | Unix.Unix_error (code, _, _) ->
          Error (`Msg ("Cannot resolve host name: " ^ Unix.error_message code))

    let connect ~sw:_ ~connect_env:() sockaddr =
      try Ok (Unix.open_connection sockaddr) with
       | Unix.Unix_error (code, _, _) ->
          Error (`Msg ("Cannot connect: " ^ Unix.error_message code))

    let output_char = output_char
    let output_string = output_string
    let flush = flush
    let input_char = input_char
    let really_input = really_input
    let close_in = close_in
  end

end

module System_unix = struct

  module Unix = struct
    type file_descr = Unix.file_descr
    let wrap_fd f fd = f fd
    let poll ~connect_env:()
             ?(read = false) ?(write = false) ?(timeout = -1.0) fd =
      let read_fds = if read then [fd] else [] in
      let write_fds = if write then [fd] else [] in
      let read_fds, write_fds, _ = Unix.select read_fds write_fds [] timeout in
      (read_fds <> [], write_fds <> [], read_fds = [] && write_fds = [])
  end

  module Preemptive = struct
    let detach f x = f x
    let run_in_main f = f ()
  end

end

module Loader = Caqti_platform_unix.Driver_loader.Make (System) (System_unix)

include Connector.Make (System) (Pool) (Loader)

open System

let connect ?env ?tweaks_version uri =
  let sw = Switch.create () in
  connect ?env ?tweaks_version ~sw ~connect_env:() uri

let with_connection = with_connection ~connect_env:()

let connect_pool
      ?max_size ?max_idle_size ?max_idle_age ?max_use_count
      ?post_connect ?env ?tweaks_version uri =
  let sw = Switch.create () in
  connect_pool
    ?max_size ?max_idle_size ?max_idle_age ?max_use_count
    ?post_connect ?env ?tweaks_version ~sw ~connect_env:() uri

let or_fail = function
 | Ok x -> x
 | Error (#Caqti_error.t as err) -> raise (Caqti_error.Exn err)
