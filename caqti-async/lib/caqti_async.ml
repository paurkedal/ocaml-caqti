(* Copyright (C) 2014--2021  Petter A. Urkedal <paurkedal@gmail.com>
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
open Caqti_common_priv
open Core_kernel

module System = struct
  type 'a future = 'a Deferred.t
  let (>>=) m f = Deferred.bind m ~f
  let (>|=) = Deferred.(>>|)
  let return = Deferred.return

  let finally f g =
    (match f () with
     | m -> m >>= fun y -> g () >|= fun () -> y
     | exception exn -> g () >|= fun () -> raise exn)

  let cleanup f g =
    (try f () with
     | exn ->
        g () >>= fun () -> raise exn)

  let join = Deferred.all_unit

  module Mvar = struct
    type 'a t = 'a Ivar.t
    let create = Ivar.create
    let store x v = Ivar.fill v x
    let fetch v = Ivar.read v
  end

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

    let poll ?(read = false) ?(write = false) ?timeout fd =
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

    let err   ?(src = default_log_src) msgf = kmsg ~src Logs.Error   msgf
    let warn  ?(src = default_log_src) msgf = kmsg ~src Logs.Warning msgf
    let info  ?(src = default_log_src) msgf = kmsg ~src Logs.Info    msgf
    let debug ?(src = default_log_src) msgf = kmsg ~src Logs.Debug   msgf
  end

  module Preemptive = struct
    let detach f x = In_thread.run (fun () -> f x)
    let run_in_main f = Thread_safe.block_on_async_exn f
  end

  module Stream = Caqti_stream.Make (struct
    type 'a future = 'a Deferred.t
    let (>>=) m f = Deferred.bind m ~f
    let (>|=) = Deferred.(>>|)
    let return = Deferred.return
  end)
end

include Caqti_connect.Make_unix (System)
