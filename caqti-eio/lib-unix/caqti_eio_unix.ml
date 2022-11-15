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

open Caqti_platform

module type STDENV = sig
  val stdenv : Eio.Stdenv.t
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

(*
  module Sequencer = struct
    type 'a t = 'a * Eio.Mutex.t
    let create x = (x, Eio.Mutex.create ())
    let enqueue (x, mutex) f =
      Eio.Mutex.use_rw ~protect:true mutex (fun () -> f x)
  end
*)

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
  module Loader = struct
    module type DRIVER = Platform_unix.DRIVER
    let load_driver = Platform_unix.load_driver
  end
  module Connect = Connector.Make_connect (System_common) (Loader)
end

include Connector.Make_without_connect (System_common)

let connect ?env ?tweaks_version stdenv uri =
  let module C = Make (struct let stdenv = stdenv end) in
  C.Connect.connect ?env ?tweaks_version uri

let with_connection ?env ?tweaks_version stdenv uri f =
  let module C = Make (struct let stdenv = stdenv end) in
  C.Connect.with_connection ?env ?tweaks_version uri f

let connect_pool
      ?max_size ?max_idle_size ?max_use_count
      ?post_connect ?env ?tweaks_version
      stdenv uri =
  let module C = Make (struct let stdenv = stdenv end) in
  C.Connect.connect_pool
    ?max_size ?max_idle_size ?max_use_count ?post_connect ?env ?tweaks_version
    uri

let or_fail = function
 | Ok x -> x
 | Error (#Caqti_error.t as err) -> raise (Caqti_error.Exn err)
