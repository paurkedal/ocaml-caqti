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

module Future = struct
  type 'a future = 'a Lwt.t
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
  let return = Lwt.return
end

module Stream = Caqti_platform.Stream.Make (Future)

module System_core = struct
  include Future

  let catch = Lwt.catch
  let finally = Lwt.finalize
  let cleanup f g = Lwt.catch f (fun exn -> g () >>= fun () -> Lwt.fail exn)
  let async ~connect_env:() = Lwt.async

  module Stream = Stream

  module Semaphore = struct
    type t = unit Lwt_mvar.t
    let create = Lwt_mvar.create_empty
    let release v = Lwt.async (Lwt_mvar.put v)
    let acquire = Lwt_mvar.take
  end

  module Log = struct
    type 'a log = 'a Logs_lwt.log
    let err ?(src = Logging.default_log_src) = Logs_lwt.err ~src
    let warn ?(src = Logging.default_log_src) = Logs_lwt.warn ~src
    let info ?(src = Logging.default_log_src) = Logs_lwt.info ~src
    let debug ?(src = Logging.default_log_src) = Logs_lwt.debug ~src
  end

  (* Cf. pgx_lwt. *)
  module Sequencer = struct
    type 'a t = 'a * Lwt_mutex.t
    let create m = (m, Lwt_mutex.create ())
    let enqueue (m, mutex) f = Lwt_mutex.with_lock mutex (fun () -> f m)
  end
end

module type CONNECTION = Caqti_connection_sig.S
  with type 'a future := 'a Lwt.t
   and type ('a, 'e) stream := ('a, 'e) Stream.t

type connection = (module CONNECTION)

let or_fail = function
 | Ok x -> Lwt.return x
 | Error (#Caqti_error.t as err) -> Lwt.fail (Caqti_error.Exn err)
