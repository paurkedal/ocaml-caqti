(* Copyright (C) 2014--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(* This is partly based on https://github.com/janestreet/lwt-async *)

open Async_kernel
open Async_unix
open Caqti_platform
open Core

module Fiber = struct
  type 'a t = 'a Deferred.t

  module Infix = struct
    let (>>=) m f = Deferred.bind m ~f
    let (>|=) = Deferred.(>>|)
  end
  open Infix

  let return = Deferred.return

  let catch f g =
    try_with ~extract_exn:true f >>= function
     | Ok y -> return y
     | Error exn -> g exn

  let finally f g =
    try_with ~extract_exn:true f >>= function
     | Ok y -> g () >|= fun () -> y
     | Error exn -> g () >|= fun () -> Error.raise (Error.of_exn exn)

  let cleanup f g =
    try_with ~extract_exn:true f >>= function
     | Ok y -> return y
     | Error exn -> g () >|= fun () -> Error.raise (Error.of_exn exn)
end
open Fiber.Infix

module Stream = Caqti_platform.Stream.Make (Fiber)

module System_core = struct
  module Fiber = Fiber

  type connect_env = unit

  let async ~sw:_ f = don't_wait_for (f ())

  module Stream = Stream

  module Semaphore = struct
    type t = unit Ivar.t
    let create = Ivar.create
    let release v = Ivar.fill v ()
    let acquire v = Ivar.read v
  end

  module Switch = Caqti_platform.Switch.Make (Fiber)

  module Log = struct
    type 'a log = ('a, unit Deferred.t) Logs.msgf -> unit Deferred.t

    (* Based on Logs_lwt.kmsg. *)
    let kmsg ?(src = Logs.default) level msgf =
      let count_it () =
        (match level with
         | Logs.Error -> Logs.incr_err_count ()
         | Logs.Warning -> Logs.incr_warn_count ()
         | _ -> ()) in
      (match Logs.Src.level src with
       | None -> return ()
       | Some level' when Poly.(level > level') ->
          count_it ();
          return ()
       | Some _ ->
          count_it ();
          let ivar = Ivar.create () in
          let k () = Ivar.read ivar in
          let over () = Ivar.fill ivar () in
          Logs.report src level ~over k msgf)

    let err ?(src = Logging.default_log_src) msgf = kmsg ~src Logs.Error msgf
    let warn ?(src = Logging.default_log_src) msgf = kmsg ~src Logs.Warning msgf
    let info ?(src = Logging.default_log_src) msgf = kmsg ~src Logs.Info msgf
    let debug ?(src = Logging.default_log_src) msgf = kmsg ~src Logs.Debug msgf
  end

  (* Cf. pgx_async. *)
  module Sequencer = struct
    type 'a t = 'a Sequencer.t
    let create t = Sequencer.create ~continue_on_error:true t
    let enqueue = Throttle.enqueue
  end

end

module Alarm = struct
  type t = unit

  let schedule ~sw:_ ~connect_env:() t f =
    let t_now = Mtime_clock.now () in
    if Mtime.is_later t ~than:t_now then f () else
    let dt_ns = Mtime.Span.to_uint64_ns (Mtime.span t t_now) in
    (match Int63.of_int64 dt_ns with
     | None -> failwith "Arithmetic overflow while computing scheduling time."
     | Some dt_ns -> Clock_ns.run_after (Time_ns.Span.of_int63_ns dt_ns) f ())

  let unschedule () = ()
end

module Pool = Caqti_platform.Pool.Make (System_core) (Alarm)

module System = struct
  include System_core
  module Stream = Stream

  module Net = struct

    module Sockaddr = struct
      type t = Async_unix.Unix.sockaddr

      let unix s = Async_unix.Unix.ADDR_UNIX s
      let tcp (addr, port) =
        Async_unix.Unix.ADDR_INET
          (Core_unix.Inet_addr.of_string (Ipaddr.to_string addr), port)
    end

    type in_channel = Reader.t
    type out_channel = Writer.t

    let getaddrinfo ~connect_env:() host port =
      let module Ai = Async_unix.Unix.Addr_info in
      let extract ai = ai.Ai.ai_addr in
      Ai.get ~host:(Domain_name.to_string host) ~service:(string_of_int port)
             Ai.[AI_SOCKTYPE SOCK_STREAM]
      >|= List.map ~f:extract >|= (fun addrs -> Ok addrs)

    let connect ~sw:_ ~connect_env:() = function
     | Async_unix.Unix.ADDR_INET (addr, port) ->
        Conduit_async.V3.(connect (Inet (`Inet (addr, port))))
          >|= fun (_socket, ic, oc) -> Ok (ic, oc)
     | Async_unix.Unix.ADDR_UNIX path ->
        Conduit_async.V3.(connect (Unix (`Unix path)))
          >|= fun (_socket, ic, oc) -> Ok (ic, oc)

    let output_char oc c = return (Writer.write_char oc c)
    let output_string oc s = return (Writer.write oc s)

    let flush = Writer.flushed

    let input_char ic =
      Reader.read_char ic
      >|= function `Ok c -> c | `Eof -> raise End_of_file

    let really_input ic s pos len =
      Reader.really_read ic ~pos ~len s
      >|= function `Ok -> () | `Eof _ -> raise End_of_file

    let close_in = Reader.close
  end
end

module System_unix = struct

  module Unix = struct
    type file_descr = Async_unix.Fd.t

    let fdinfo = Info.of_string "Caqti_async file descriptor"

    let wrap_fd f ufd =
      let fd = Fd.create (Fd.Kind.Socket `Active) ufd fdinfo in
      let open Deferred in
      f fd >>= fun r ->
      Fd.(close ~file_descriptor_handling:Do_not_close_file_descriptor) fd
        >>= fun () ->
      return r

    let poll ~connect_env:() ?(read = false) ?(write = false) ?timeout fd =
      let wait_read =
        if read then Async_unix.Fd.ready_to fd `Read else Deferred.never () in
      let wait_write =
        if write then Async_unix.Fd.ready_to fd `Write else Deferred.never () in
      let wait_timeout =
        (match timeout with
         | Some t -> Clock.after (Time.Span.of_sec t)
         | None -> Deferred.never ()) in
      let did_read, did_write, did_timeout = ref false, ref false, ref false in
      let is_ready = function
        | `Ready -> true
        | `Bad_fd | `Closed -> false in
      Deferred.enabled [
        Deferred.choice wait_read (fun st -> did_read := is_ready st);
        Deferred.choice wait_write (fun st -> did_write := is_ready st);
        Deferred.choice wait_timeout (fun () -> did_timeout := true);
      ] >>|
      (fun f ->
        ignore (f ());
        (!did_read, !did_write, !did_timeout))
  end

  module Preemptive = struct
    let detach f x = In_thread.run (fun () -> f x)
    let run_in_main f = Thread_safe.block_on_async_exn f
  end

end

module Loader = Caqti_platform_unix.Driver_loader.Make (System) (System_unix)

include Connector.Make (System) (Pool) (Loader)

open System

let connect = connect ~sw:Switch.eternal ~connect_env:()
let with_connection = with_connection ~connect_env:()
let connect_pool = connect_pool~sw:Switch.eternal  ~connect_env:()
