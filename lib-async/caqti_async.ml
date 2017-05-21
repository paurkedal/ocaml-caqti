(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

(* This is partly based on https://github.com/janestreet/lwt-async *)

open Core.Std
open Async.Std

include Caqti.Make (struct
  type 'a io = 'a Deferred.Or_error.t
  let (>>=) m f = Deferred.Or_error.bind m ~f
  let (>|=) = Deferred.Or_error.(>>|)
  let return = Deferred.Or_error.return
  let fail = Deferred.Or_error.of_exn
  let join = Deferred.Or_error.all_ignore

  let catch f g =
    let open Deferred in
    f () >>=
    function
    | Error err -> g (Error.to_exn err)
    | Ok _ as r -> return r

  module Mvar = struct
    type 'a t = 'a Ivar.t
    let create = Ivar.create
    let store x v = Ivar.fill v x
    let fetch v =
      let open Deferred in
      Ivar.read v >>= fun x -> Deferred.Or_error.return x
  end

  module Unix = struct
    type file_descr = Async_unix.Fd.t

    let fdinfo = Info.of_string "Caqti_async file descriptor"

    let wrap_fd f ufd =
      let fd = Fd.create (Fd.Kind.Socket `Active) ufd fdinfo in
      let open Deferred in
      f fd >>= fun r ->
      Fd.(close ~file_descriptor_handling:Do_not_close_file_descriptor) fd >>= fun () ->
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
      Deferred.enabled [
        Deferred.choice wait_read (fun st -> did_read := st = `Ready);
        Deferred.choice wait_write (fun st -> did_write := st = `Ready);
        Deferred.choice wait_timeout (fun () -> did_timeout := true);
      ] >>|
      (fun f ->
        ignore (f ());
        Ok (!did_read, !did_write, !did_timeout))
  end

  module Log = struct
    let log_f level fmt =
      ksprintf
        (fun s -> Log.string ~level (Lazy.force Log.Global.log) s; return ())
        fmt
    let error_f   fmt = log_f `Error fmt
    let warning_f fmt = log_f `Info  fmt
    let info_f    fmt = log_f `Info  fmt
    let debug_f   fmt = log_f `Debug fmt

    (* TODO: Check how log filtering works in async. *)
    let debug_query_enabled () = false
    let debug_tuple_enabled () = false

    let debug_query qi params =
      begin match qi with
      | `Oneshot qs -> log_f `Debug "Sent query: %s" qs
      | `Prepared (qn, qs) -> log_f `Debug "Sent query %s: %s" qn qs
      end >>= fun () ->
      if params = [] then
        return ()
      else
        log_f `Debug "with parameters: %s" (String.concat ~sep:", " params)

    let debug_tuple tuple =
      log_f `Debug "Received tuple: %s" (String.concat ~sep:", " tuple)
  end

  module Preemptive = struct
    let detach f x = In_thread.run (fun () -> Or_error.try_with (fun () -> f x))
    let run_in_main f = Or_error.ok_exn (Thread_safe.block_on_async_exn f)
  end
end)
