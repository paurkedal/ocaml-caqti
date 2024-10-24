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

let error_msgf fmt = Format.kasprintf (fun msg -> Error (`Msg msg)) fmt

module type FLOW = Caqti_platform.System_sig.SOCKET_OPS with type 'a fiber = 'a

type ocaml = | and system = |

type 'a impl =
  | OCaml : (module FLOW with type t = 'a) * 'a -> ocaml impl
  | System : Buffer.t * Miou_unix.file_descr -> system impl

type socket = Socket : 'a impl -> socket [@@unboxed]

type Caqti_error.msg +=
  | Msg_unix of Unix.error * string * string

let () =
  let pp ppf = function
    | Msg_unix (err, f, v) ->
        Format.fprintf ppf "%s(%s): %s" f v (Unix.error_message err)
    | _ -> assert false
  in
  Caqti_error.define_msg ~pp [%extension_constructor Msg_unix]

external reraise : exn -> 'a = "%reraise"

module System_core = struct
  include Caqti_miou.System_core
  type stdenv = unit
end
include System_core

module Alarm = struct
  type t = Miou.Condition.t * Miou.Mutex.t

  let schedule ~sw ~stdenv:_ t fn =
    let t_now = Mtime_clock.now () in
    let mutex = Miou.Mutex.create () and condition = Miou.Condition.create () in
    let delay =
      if Mtime.is_later t ~than:t_now then 0.0
      else Mtime.Span.to_float_ns (Mtime.span t t_now) *. 1e-9
    in
    Logs.debug (fun m -> m "schedule an alarm");
    let _ =
      async ~sw @@ fun () ->
      Logs.debug (fun m -> m "really schedule an alarm");
      let sleeper = Miou.async @@ fun () ->
        Logs.debug (fun m -> m "Sleep %fs" delay);
        Miou_unix.sleep delay;
        Logs.debug (fun m -> m "Ring the alarm");
        `Continue in
      let canceller =
        Miou.async @@ fun () ->
        Miou.Condition.wait condition mutex;
        `Cancel
      in
      match Miou.await_first [ sleeper; canceller ] with
      | Ok `Continue -> fn ()
      | Ok `Cancel -> ()
      | Error _exn -> ()
    in
    (condition, mutex)

  let unschedule (condition, mutex) =
    Miou.Mutex.protect mutex @@ fun () -> Miou.Condition.signal condition
end

module Stream = Caqti_miou.Stream
module Pool = Caqti_platform.Pool.Make (System_core) (Alarm)

module Net = struct
  module Sockaddr = struct
    type t = Unix.sockaddr

    let unix v = Unix.ADDR_UNIX v
    let tcp (addr, port) = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr addr, port)
  end

  let getaddrinfo ~stdenv:() host port =
    let opts = Unix.[ AI_SOCKTYPE SOCK_STREAM ] in
    match
      Unix.getaddrinfo (Domain_name.to_string host) (string_of_int port) opts
    with
    | lst -> Ok (List.map (fun ai -> ai.Unix.ai_addr) lst)
    | exception Not_found -> Ok []
    | exception Unix.Unix_error (err, f, v) ->
        error_msgf "%s(%s): %s" f v (Unix.error_message err)

  let convert_io_exception = function
    | Unix.Unix_error (err, f, v) -> Some (Msg_unix (err, f, v))
    | _ -> None

  type tcp_flow = Miou_unix.file_descr
  type tls_flow = ocaml impl

  module Socket = struct
    type t = socket

    let output_char (Socket impl) chr = match impl with
      | System (buf, _) -> Buffer.add_char buf chr
      | OCaml ((module Flow), fd) -> Flow.output_char fd chr

    let output_string (Socket impl) str = match impl with
      | System (buf, _) -> Buffer.add_string buf str
      | OCaml ((module Flow), fd) -> Flow.output_string fd str

    let flush (Socket impl) = match impl with
      | System (buf, fd) ->
          let str = Buffer.contents buf in
          Buffer.clear buf;
          if String.length str > 0 then Miou_unix.write fd str
      | OCaml ((module Flow), fd) -> Flow.flush fd

    let input_char (Socket impl) = match impl with
      | System (_, fd) ->
        let buf = Bytes.make 1 '\000' in
        let len = Miou_unix.read fd buf in
        if len = 0 then raise End_of_file else Bytes.get buf 0
      | OCaml ((module Flow), fd) ->
        Flow.input_char fd

    let really_input (Socket impl) buf off len =
      match impl with
      | System (_, fd) ->
        let rec go off len =
          if len > 0 then
            let len' = Miou_unix.read fd buf ~off ~len in
            go (off + len') (len - len')
        in
        go off len
      | OCaml ((module Flow), fd) ->
        Flow.really_input fd buf off len

    let close = function
      | Socket (System (_, fd)) -> Miou_unix.close fd
      | Socket (OCaml ((module Flow), fd)) -> Flow.close fd
  end

  let socket = function
    | Unix.ADDR_UNIX _ ->
        let fd = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
        Ok (Miou_unix.of_file_descr ~non_blocking:true fd)
    | Unix.ADDR_INET (inet_addr, _) when Unix.is_inet6_addr inet_addr ->
        Ok (Miou_unix.tcpv6 ())
    | _ -> Ok (Miou_unix.tcpv4 ())

  let connect_tcp ~sw:_ ~stdenv:_ sockaddr =
    let ( >>= ) = Result.bind in
    socket sockaddr >>= fun socket ->
    match Miou_unix.connect socket sockaddr with
    | () -> Ok (Socket (System (Buffer.create 0x7ff, socket)))
    | exception Unix.Unix_error (err, f, v) ->
        Miou_unix.close socket;
        Error (Msg_unix (err, f, v))
    | exception exn -> Miou_unix.close socket; raise exn

  let tcp_flow_of_socket (Socket impl) = match impl with
    | System (_, fd) -> Some fd
    | OCaml _ -> None

  let socket_of_tls_flow : sw:_ -> tls_flow -> Socket.t =
   fun ~sw:_ -> function
     | OCaml _ as impl -> Socket impl

  module type TLS_PROVIDER =
    Caqti_platform.System_sig.TLS_PROVIDER
      with type 'a fiber := 'a
       and type tcp_flow := tcp_flow
       and type tls_flow := tls_flow

  let tls_providers_r : (module TLS_PROVIDER) list ref = ref []
  let register_tls_provider p = tls_providers_r := p :: !tls_providers_r

  let tls_providers config =
    if Caqti_connect_config.mem_name "tls" config then begin
      match Caqti_platform.Connector.load_library "caqti-tls-miou" with
      | Ok () -> ()
      | Error msg -> Log.warn (fun m -> m "TLS configured, but missing caqti-tls-miou: %s" msg)
    end;
    !tls_providers_r
end
