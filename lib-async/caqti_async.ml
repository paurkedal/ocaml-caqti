(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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
  let (>>=) = Deferred.Or_error.bind
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
      Fd.close ~should_close_file_descriptor:false fd >>= fun () ->
      return r

    let wait_for op fd =
      Deferred.bind
	(Async_unix.Fd.ready_to fd op)
	begin function
	| `Bad_fd | `Closed -> assert false
	| `Ready -> Deferred.Or_error.return ()
	end
    let wait_read = wait_for `Read
    let wait_write = wait_for `Write
  end

  module Log = struct
    let log_f level fmt =
      ksprintf
	(fun s -> Log.string ~level (Lazy.force Log.Global.log) s; return ())
	fmt
    let error_f   q fmt = log_f `Error fmt
    let warning_f q fmt = log_f `Info  fmt
    let info_f    q fmt = log_f `Info  fmt
    let debug_f   q fmt = log_f `Debug fmt

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
