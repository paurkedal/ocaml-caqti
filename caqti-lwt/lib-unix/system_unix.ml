(* Copyright (C) 2023  Petter A. Urkedal <paurkedal@gmail.com>
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

open Lwt.Infix

module Preemptive = Lwt_preemptive

module Unix = struct
  type file_descr = Lwt_unix.file_descr

  let wrap_fd f fd = f (Lwt_unix.of_unix_file_descr fd)

  let poll ~connect_env:() ?(read = false) ?(write = false) ?timeout fd =
    let choices = []
      |> (fun acc -> if read then Lwt_unix.wait_read fd :: acc else acc)
      |> (fun acc -> if write then Lwt_unix.wait_write fd :: acc else acc)
      |> Option.fold
          ~none:Fun.id ~some:(fun t acc -> Lwt_unix.timeout t :: acc) timeout
    in
    if choices = [] then
      Lwt.fail_invalid_arg "Caqti_lwt.Unix.poll: No operation specified."
    else
    Lwt.catch
      (fun () -> Lwt.choose choices >|= fun _ -> false)
      (function
       | Lwt_unix.Timeout -> Lwt.return_true
       | exn -> Lwt.fail exn)
      >|= fun timed_out ->
    (Lwt_unix.readable fd, Lwt_unix.writable fd, timed_out)
end
