(* Copyright (C) 2022--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

type Caqti_error.msg += Msg_io of Eio.Exn.err * Eio.Exn.context

let () =
  let pp ppf = function
   | Msg_io (err, ctx) -> Eio.Exn.pp ppf (Eio.Exn.Io (err, ctx))
   | _ -> assert false
  in
  Caqti_error.define_msg ~pp [%extension_constructor Msg_io]

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

type stdenv = <
  net : [`Generic] Eio.Net.ty Eio.Std.r;
  clock : float Eio.Time.clock_ty Eio.Std.r;
  mono_clock : Eio.Time.Mono.ty Eio.Std.r;
>

module Switch = struct
  include Eio.Switch
  let run f = run f (* avoids compatibilty issues due to optional arguments *)
end

(* TODO: Log error. *)
let async = Eio.Fiber.fork

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

module Alarm = struct
  type t = Eio.Cancel.t

  exception Unscheduled

  let schedule ~sw ~stdenv t f =
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

  let getaddrinfo ~stdenv host port =
    try
      Eio.Net.getaddrinfo_stream stdenv#net
        ~service:(string_of_int port) (Domain_name.to_string host)
        |> Result.ok
    with Eio.Exn.Io _ as exn ->
      Error (`Msg (Format.asprintf "%a" Eio.Exn.pp exn))

  module Socket = struct
    type t = {
      flow: [Eio.Flow.two_way_ty | Eio.Resource.close_ty] Eio.Resource.t;
      ic: Eio.Buf_read.t;
      oc: Eio.Buf_write.t;
    }

    let output_char {oc; _} c = Eio.Buf_write.char oc c
    let output_string {oc; _} s = Eio.Buf_write.string oc s
    let flush {oc; _} = Eio.Buf_write.flush oc

    let input_char {ic; _} =
      Eio.Buf_read.ensure ic 1;
      let ch = Cstruct.get_char (Eio.Buf_read.peek ic) 0 in
      Eio.Buf_read.consume ic 1;
      ch

    let really_input {ic; _} buf i n =
      Eio.Buf_read.ensure ic n;
      Cstruct.blit_to_bytes (Eio.Buf_read.peek ic) 0 buf i n;
      Eio.Buf_read.consume ic n

    let close {flow; oc; _} =
      Log.debug (fun m -> m "Closing socket.");
      Eio.Buf_write.close oc;
      Eio.Flow.shutdown flow `Send;
      Eio.Flow.close flow
  end

  type tcp_flow = [Eio.Flow.two_way_ty | Eio.Resource.close_ty] Eio.Resource.t
  type tls_flow = [Eio.Flow.two_way_ty | Eio.Resource.close_ty] Eio.Resource.t

  let tcp_flow_of_socket {Socket.flow; ic; oc} =
    Log.debug (fun m -> m "Enabling TLS.");
    assert (Eio.Buf_write.pending_bytes oc = 0);
    assert (Eio.Buf_read.buffered_bytes ic = 0);
    Eio.Buf_write.close oc;
    Some flow

  let start_writer ~which oc flow =
    Log.debug (fun m -> m "%s writer started." which);
    try
      while true; do
        let iovecs = Eio.Buf_write.await_batch oc in
        let n = Eio.Flow.single_write flow iovecs in
        Eio.Buf_write.shift oc n
      done
    with End_of_file ->
      Log.debug (fun m -> m "%s writer finished." which)

  let socket_of_flow ~which ~sw flow =
    let ic = Eio.Buf_read.of_flow ~max_size:4096 flow in
    let oc = Eio.Buf_write.create 4096 in
    Eio.Fiber.fork ~sw (fun () -> start_writer ~which oc flow);
    {Socket.flow; ic; oc}

  let socket_of_tls_flow ~sw flow = socket_of_flow ~which:"TLS" ~sw flow

  let connect_tcp ~sw ~stdenv sockaddr =
    try
      let flow =
        (Eio.Net.connect ~sw stdenv#net sockaddr
          :> [Eio.Flow.two_way_ty | Eio.Resource.close_ty] Eio.Resource.t)
      in
      Ok (socket_of_flow ~which:"TCP" ~sw flow)
    with Eio.Exn.Io (err, ctx) ->
      Error (Msg_io (err, ctx))

  module type TLS_PROVIDER = Caqti_platform.System_sig.TLS_PROVIDER
    with type 'a fiber := 'a
     and type tcp_flow := tcp_flow
     and type tls_flow := tls_flow

  let tls_providers_r : (module TLS_PROVIDER) list ref = ref []

  let register_tls_provider p = tls_providers_r := p :: !tls_providers_r

  let tls_providers config =
    if Caqti_connect_config.mem_name "tls" config then
      (match Caqti_platform.Connector.load_library "caqti-tls-eio" with
       | Ok () -> ()
       | Error msg ->
          Log.warn (fun p ->
            p "TLS configured, but missing caqti-tls-eio: %s" msg));
    !tls_providers_r

end
