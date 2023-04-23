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

include Caqti_eio.System

module Unix = struct
  type file_descr = Unix.file_descr

  let wrap_fd f fd = f fd

  let poll
        ~connect_env:{stdenv; _}
        ?(read = false) ?(write = false) ?timeout fd =
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
