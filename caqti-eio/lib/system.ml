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

open Caqti_platform

module Core = struct
  type 'a future = 'a
  let (>>=) x f = f x
  let (>|=) x f = f x
  let return x = x

  let catch f g = try f () with exn -> g exn

  let finally f g =
    (match f () with
     | y -> g (); y
     | exception exn -> g (); raise exn)

  let cleanup f g = try f () with exn -> g (); raise exn

  module Semaphore = struct
    type t = Eio.Semaphore.t

    let create () = Eio.Semaphore.make 0
    let release = Eio.Semaphore.release
    let acquire = Eio.Semaphore.acquire
  end

  module Log = struct
    type 'a log = 'a Logs.log
    let err ?(src = Logging.default_log_src) = Logs.err ~src
    let warn ?(src = Logging.default_log_src) = Logs.warn ~src
    let info ?(src = Logging.default_log_src) = Logs.info ~src
    let debug ?(src = Logging.default_log_src) = Logs.debug ~src
  end

  type connect_env = {
    stdenv: Eio.Stdenv.t;
    sw: Eio.Switch.t;
  }

  (* TODO: Log error. *)
  let async ~connect_env:{sw; _} f =
    Eio.Fiber.fork_sub ~sw ~on_error:(fun _ -> ()) (fun _sw -> f ())
end
include Core

module Alarm = struct
  type t = Eio.Cancel.t

  exception Unscheduled

  let schedule ~connect_env:{stdenv; sw} t f =
    let alarm = ref None in
    Eio.Fiber.fork ~sw begin fun () ->
      Eio.Cancel.sub begin fun cctx ->
        alarm := Some cctx;
        Eio.Time.Mono.sleep_until stdenv#mono_clock t;
        f ()
      end
    end;
    Option.get !alarm

  let unschedule alarm =
    Eio.Cancel.cancel alarm Unscheduled
end

module Stream = Caqti_platform.Stream.Make (Core)
module Pool = Caqti_platform.Pool.Make (Core) (Alarm)

module Sequencer = struct
  type 'a t = 'a * Eio.Mutex.t

  let create x = (x, Eio.Mutex.create ())

  let enqueue (x, mutex) f =
    (* Using Eio.Mutex.use_rw without handling exceptions poisons the mutex,
     * preventing recovery from e.g. a statement timeout. *)
    Eio.Mutex.lock mutex;
    (match f x with
     | y -> Eio.Mutex.unlock mutex; y
     | exception exn -> Eio.Mutex.unlock mutex; raise exn)
end

module Net = struct

  module Sockaddr = struct
    type t = Eio.Net.Sockaddr.stream

    let tcp ((addr : Ipaddr.t), port) =
      let addr_octets =
        (match addr with
         | V4 addr4 -> Ipaddr.V4.to_octets addr4
         | V6 addr6 -> Ipaddr.V6.to_octets addr6)
      in
      `Tcp (Eio.Net.Ipaddr.of_raw addr_octets, port)

    let unix path = `Unix path
  end

  type in_channel = <Eio.Flow.source; Eio.Flow.close>
  type out_channel = Eio.Flow.sink

  let getaddrinfo ~connect_env:{stdenv; _} host port =
    try
      Eio.Net.getaddrinfo_stream stdenv#net
        ~service:(string_of_int port) (Domain_name.to_string host)
        >|= Result.ok
    with Eio.Exn.Io _ as exn ->
      Error (`Msg (Format.asprintf "%a" Eio.Exn.pp exn))

  let connect ~connect_env:{stdenv; sw} sockaddr =
    try
      let socket_flow = Eio.Net.connect ~sw stdenv#net sockaddr in
      Ok ((socket_flow :> in_channel), (socket_flow :> out_channel))
    with Eio.Exn.Io _ as exn ->
      Error (`Msg (Format.asprintf "%a" Eio.Exn.pp exn))

  let output_char sink c = Eio.Flow.copy_string (String.make 1 c) sink
  let output_string sink s = Eio.Flow.copy_string s sink
  let flush _sink = ()

  let input_char source =
    let buf = Cstruct.create 1 in
    Eio.Flow.read_exact source buf;
    Cstruct.get_char buf 0

  let really_input source buf i n =
    let cbuf = Cstruct.create n in
    Eio.Flow.read_exact source cbuf;
    Cstruct.blit_to_bytes cbuf 0 buf i n

  (* TODO: Check shutdown semantics, or maybe leave it to the switch. *)
  let close_in source = Eio.Flow.close source
end
