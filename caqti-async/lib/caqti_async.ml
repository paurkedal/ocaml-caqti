(* Copyright (C) 2014--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

type Caqti_error.msg += Msg_unix of Core_unix.Error.t * string * string

let () =
  let pp ppf = function
   | Msg_unix (err, func, arg) ->
      Format.fprintf ppf "%s in %s(%S)" (Core_unix.Error.message err) func arg
   | _ -> assert false
  in
  Caqti_error.define_msg ~pp [%extension_constructor Msg_unix]

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

  type stdenv = unit

  let async ~sw:_ f = don't_wait_for (f ())

  module Stream = Stream

  module Mutex = struct
    type t = (unit, Core.read_write) Mvar.t
    let create = Mvar.create
    let lock m = Mvar.put m ()
    let unlock m = Mvar.take_now_exn m
  end

  module Condition = struct
    include Async_kernel.Condition
    type nonrec t = unit t
    let wait c m = Mutex.unlock m; wait c >>= fun () -> Mutex.lock m
    let signal c = signal c ()
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
          let over () = Ivar.fill_exn ivar () in
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

  let schedule ~sw:_ ~stdenv:() t f =
    let t_now = Mtime_clock.now () in
    let dt_ns =
      if Mtime.is_later t ~than:t_now then 0L else
      Mtime.Span.to_uint64_ns (Mtime.span t t_now)
    in
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

    let getaddrinfo ~stdenv:() host port =
      let module Ai = Async_unix.Unix.Addr_info in
      let extract ai = ai.Ai.ai_addr in
      Ai.get ~host:(Domain_name.to_string host) ~service:(string_of_int port)
             Ai.[AI_SOCKTYPE SOCK_STREAM]
      >|= List.map ~f:extract >|= (fun addrs -> Ok addrs)

    module Socket = struct
      type t = Reader.t * Writer.t

      let output_char (_, oc) c = return (Writer.write_char oc c)
      let output_string (_, oc) s = return (Writer.write oc s)

      let flush (_, oc) = Writer.flushed oc

      let input_char (ic, _) =
        Reader.read_char ic
        >|= function `Ok c -> c | `Eof -> raise End_of_file

      let really_input (ic, _) s pos len =
        Reader.really_read ic ~pos ~len s
        >|= function `Ok -> () | `Eof _ -> raise End_of_file

      let close (_ic, oc) = Writer.close oc
    end

    type tcp_flow = Socket.t
    type tls_flow = Socket.t

    let convert_io_exception exn =
      (match Async_kernel.Monitor.extract_exn exn with
       | Core_unix.Unix_error (err, func, arg) ->
          Some (Msg_unix (err, func, arg))
       | _ -> None)

    let intercept_exceptions f =
      Async_kernel.Monitor.try_with f >|= function
       | Ok _ as r -> r
       | Error exn ->
          (match convert_io_exception exn with
           | Some msg -> Error msg
           | None -> raise exn)

    let connect_tcp ~sw:_ ~stdenv:() addr =
      intercept_exceptions @@ fun () ->
      (match addr with
       | Async_unix.Unix.ADDR_INET (addr, port) ->
          Async_unix.Tcp.connect
            (Tcp.Where_to_connect.of_inet_address (`Inet (addr, port)))
          >|= fun (_, ic, oc) -> (ic, oc)
       | Async_unix.Unix.ADDR_UNIX path ->
          Async_unix.Tcp.connect
            (Tcp.Where_to_connect.of_unix_address (`Unix path))
          >|= fun (_, ic, oc) -> (ic, oc))

    let tcp_flow_of_socket socket = Some socket
    let socket_of_tls_flow ~sw:_ socket = socket

    module type TLS_PROVIDER = Caqti_platform.System_sig.TLS_PROVIDER
      with type 'a fiber := 'a Deferred.t
       and type tcp_flow := tcp_flow
       and type tls_flow := tls_flow

    let tls_providers_r : (module TLS_PROVIDER) list ref = ref []

    let tls_providers config =
      if Caqti_connect_config.mem_name "tls" config then
        (match Caqti_platform.Connector.load_library "caqti-tls-async" with
         | Ok () -> ()
         | Error msg ->
            Logs.warn ~src:Logging.default_log_src (fun p ->
              p "TLS configured but caqti-tls-async not available: %s" msg));
      !tls_providers_r

    let register_tls_provider p = tls_providers_r := p :: !tls_providers_r
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

    let poll ~stdenv:() ?(read = false) ?(write = false) ?timeout fd =
      let wait_read =
        if read then Async_unix.Fd.ready_to fd `Read else Deferred.never () in
      let wait_write =
        if write then Async_unix.Fd.ready_to fd `Write else Deferred.never () in
      let wait_timeout =
        (match timeout with
         | Some t -> Clock.after (Time_float.Span.of_sec t)
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

module type CONNECTION = Caqti_connection_sig.S
  with type 'a fiber := 'a Deferred.t
   and type ('a, 'e) stream := ('a, 'e) Stream.t

type connection = (module CONNECTION)

let connect = connect ~sw:Switch.eternal ~stdenv:()
let with_connection = with_connection ~stdenv:()
let connect_pool = connect_pool~sw:Switch.eternal  ~stdenv:()
