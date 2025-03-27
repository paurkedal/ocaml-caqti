(* Copyright (C) 2018--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

type Caqti_error.msg += Msg_unix of Unix.error * string * string

let () =
  let pp ppf = function
   | Msg_unix (err, func, arg) ->
      Format.fprintf ppf "%s in %s(%S)" (Unix.error_message err) func arg
   | _ -> assert false
  in
  Caqti_error.define_msg ~pp [%extension_constructor Msg_unix]

module Fiber = struct
  type 'a t = 'a
  module Infix = struct
    let (>>=) x f = f x
    let (>|=) x f = f x
  end
  let return x = x

  let catch f g = try f () with exn -> g exn

  let finally f g =
    (match f () with
     | y -> g (); y
     | exception exn -> g (); raise exn)

  let cleanup f g = try f () with exn -> g (); raise exn
end

module Stream = Caqti_platform.Stream.Make (Fiber)

module System_core = struct
  module Fiber = Fiber

  module Switch = Caqti_platform.Switch.Make (Fiber)

  let async ~sw:_ f = f ()

  module Stream = Stream

  module Mutex = Stdlib.Mutex

  module Condition = Stdlib.Condition

  module Log = struct
    type 'a log = 'a Logs.log
    let err ?(src = Logging.default_log_src) = Logs.err ~src
    let warn ?(src = Logging.default_log_src) = Logs.warn ~src
    let info ?(src = Logging.default_log_src) = Logs.info ~src
    let debug ?(src = Logging.default_log_src) = Logs.debug ~src
  end

  type stdenv = unit

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

    let getaddrinfo ~stdenv:() host port =
      try
        let opts = Unix.[AI_SOCKTYPE SOCK_STREAM] in
        Unix.getaddrinfo (Domain_name.to_string host) (string_of_int port) opts
          |> List.map (fun ai -> ai.Unix.ai_addr) |> Result.ok
      with
       | Not_found -> Ok []
       | Unix.Unix_error (code, _, _) ->
          Error (`Msg ("Cannot resolve host name: " ^ Unix.error_message code))

    let convert_io_exception = function
     | Unix.Unix_error (err, fn, arg) -> Some (Msg_unix (err, fn, arg))
     | _ -> None

    module Socket = struct
      type t = Tcp of in_channel * out_channel

      let output_char (Tcp (_, oc)) = output_char oc
      let output_string (Tcp (_, oc)) = output_string oc
      let flush (Tcp (_, oc)) = flush oc
      let input_char (Tcp (ic, _)) = input_char ic
      let really_input (Tcp (ic, _)) = really_input ic
      let close (Tcp (_, oc)) = close_out oc
    end

    type tcp_flow = Socket.t
    type tls_flow = Socket.t

    let connect_tcp ~sw:_ ~stdenv:() sockaddr =
      try
        let ic, oc = Unix.open_connection sockaddr in
        Ok (Socket.Tcp (ic, oc))
      with
       | Unix.Unix_error (err, func, arg) -> Error (Msg_unix (err, func, arg))

    let tcp_flow_of_socket _ = None
    let socket_of_tls_flow ~sw:_ = Fun.id

    module type TLS_PROVIDER = System_sig.TLS_PROVIDER
      with type 'a fiber := 'a
       and type tcp_flow := Socket.t
       and type tls_flow := Socket.t

    let tls_providers_r : (module TLS_PROVIDER) list ref = ref []

    let register_tls_provider p = tls_providers_r := p :: !tls_providers_r

    let tls_providers _ =
      (* Try to load caqti-tls.unix here if/when implemented. *)
      !tls_providers_r
  end

end

module System_unix = struct

  module Unix = struct
    type file_descr = Unix.file_descr
    let wrap_fd f fd = f fd
    let poll ~stdenv:()
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

module type CONNECTION = Caqti_connection_sig.S
  with type 'a fiber := 'a
   and type ('a, 'e) stream := ('a, 'e) Stream.t

type connection = (module CONNECTION)

let connect ?subst ?env ?config ?tweaks_version uri =
  let sw = Switch.create () in
  connect ?subst ?env ?config ?tweaks_version ~sw ~stdenv:() uri

let with_connection = with_connection ~stdenv:()

let connect_pool
      ?pool_config ?post_connect ?subst ?env ?config ?tweaks_version uri =
  let sw = Switch.create () in
  connect_pool
    ?pool_config ?post_connect ?subst ?env ?config ?tweaks_version
    ~sw ~stdenv:() uri

let or_fail = function
 | Ok x -> x
 | Error (#Caqti_error.t as err) -> raise (Caqti_error.Exn err)
