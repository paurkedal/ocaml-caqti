(* Copyright (C) 2023--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

module Unix = struct
  type file_descr = Miou_unix.file_descr

  let wrap_fd f fd = f (Miou_unix.of_file_descr ~non_blocking:true fd)

  exception Timeout
  let or_raise = function Ok v -> v | Error exn -> raise exn

  let poll ~stdenv:() ?(read= false) ?(write= false) ?timeout fd =
    let fn () = match read, write with
      | false, false -> (false, false, false)
      | true, true ->
        let reader = Miou.async @@ fun () ->
          Miou_unix.(blocking_read (to_file_descr fd));
          (true, false, false) in
        let writer = Miou.async @@ fun () ->
          Miou_unix.(blocking_write (to_file_descr fd));
          (false, true, false) in
        Miou.await_first [ reader; writer ] |> or_raise
      | true, false ->
        Miou_unix.(blocking_read (to_file_descr fd)); (true, false, false)
      | false, true ->
        Miou_unix.(blocking_write (to_file_descr fd)); (false, true, false) in
    match timeout with
    | None -> fn ()
    | Some t ->
      let sleep = Miou.async @@ fun () -> Miou_unix.sleep t; raise Timeout in
      match Miou.await_first [ sleep; Miou.async fn ] with
      | Ok v -> v
      | Error Timeout -> (false, false, true)
      | Error exn -> raise exn
end

module Preemptive = struct
  let detach f x =
    let fn () = f x in
    let prm =
      if Miou.Domain.available () > 0
      then Miou.call fn
      else Miou.async fn in
    Miou.await_exn prm

  let run_in_main fn = fn ()
end
