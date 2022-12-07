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

open Eio.Std
open Caqti_platform

let ( let/? ) r f = Result.bind r f
let ( let-? ) r f = Result.map f r

module type STDENV = sig
  val stdenv : Eio.Stdenv.t
  val sw : Eio.Switch.t
end

module System_common = struct
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

  module Stream = Caqti_platform.Stream.Make (struct
    type 'a future = 'a
    let (>>=) x f = f x
    let (>|=) x f = f x
    let return x = x
  end)
end

module Make_system (Stdenv : STDENV) = struct
  open Stdenv

  include System_common

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

    let getaddrinfo host port =
      try
        Eio.Net.getaddrinfo_stream stdenv#net
          ~service:(string_of_int port) (Domain_name.to_string host)
          >|= Result.ok
      with Eio.Net.Connection_failure exn ->
        Error (`Msg (Printexc.to_string exn))

    let connect sockaddr =
      try
        let socket_flow = Eio.Net.connect ~sw stdenv#net sockaddr in
        Ok ((socket_flow :> in_channel), (socket_flow :> out_channel))
      with Eio.Net.Connection_failure exn ->
        Error (`Msg (Printexc.to_string exn))

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

  module Unix = struct
    type file_descr = Unix.file_descr

    let wrap_fd f fd = f fd

    let poll ?(read = false) ?(write = false) ?timeout fd =
      let f () =
        (match read, write with
         | false, false ->
            (false, false, false)
         | true, true ->
            Eio.Fiber.first
              (fun () -> Eio_unix.await_readable fd; (true, false, false))
              (fun () -> Eio_unix.await_writable fd; (false, true, false))
         | true, false ->
            Eio_unix.await_readable fd; (true, false, false)
         | false, true ->
            Eio_unix.await_writable fd; (false, true, false))
      in
      (match timeout with
       | None -> f ()
       | Some t ->
          (match Eio.Time.with_timeout stdenv#clock t (fun () -> Ok (f ())) with
           | Ok r -> r
           | Error `Timeout -> (false, false, true)))
  end

  module Preemptive = struct
    let detach f x = Eio_unix.run_in_systhread (fun () -> f x)
    let run_in_main f = f () (* FIXME *)
  end
end

module Make (Stdenv : STDENV) = struct
  module System = Make_system (Stdenv)
  module Platform_unix = Caqti_platform_unix.Driver_loader.Make (System)
  module Platform_net = Caqti_platform_net.Driver_loader.Make (System)

  module Loader = struct
    module type DRIVER = Platform_unix.DRIVER
    let load_driver ~uri scheme =
      (match Platform_net.load_driver ~uri scheme with
       | Ok _ as r -> r
       | Error (`Load_rejected _) as r -> r
       | Error (`Load_failed _) ->
          (* TODO: Summarize errors. *)
          Platform_unix.load_driver ~uri scheme)
  end

  include Connector.Make_connect (System_common) (Loader)
end

include Connector.Make_without_connect (System_common)

let connect ?env ?tweaks_version stdenv ~sw uri =
  Switch.check sw;
  let module Connector = Make (struct let stdenv = stdenv let sw = sw end) in
  let-? connection = Connector.connect ?env ?tweaks_version uri in
  let module C = (val connection : CONNECTION) in
  Switch.on_release sw C.disconnect;
  connection

let connect_pool
      ?max_size ?max_idle_size ?max_use_count
      ?post_connect ?env ?tweaks_version
      stdenv ~sw uri =
  Switch.check sw;
  let module Connector = Make (struct let stdenv = stdenv let sw = sw end) in
  let-? pool =
    Connector.connect_pool
      ?max_size ?max_idle_size ?max_use_count ?post_connect ?env ?tweaks_version
      uri
  in
  Switch.on_release sw (fun () -> Pool.drain pool);
  pool

let with_connection ?env ?tweaks_version stdenv uri f =
  Switch.run begin fun sw ->
    let/? connection = connect ?env ?tweaks_version stdenv ~sw uri in
    f connection
  end

let or_fail = function
 | Ok x -> x
 | Error (#Caqti_error.t as err) -> raise (Caqti_error.Exn err)
