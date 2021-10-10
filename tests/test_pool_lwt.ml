(* Copyright (C) 2014--2020  Petter A. Urkedal <paurkedal@gmail.com>
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

module Pool = Caqti_lwt.Pool

module Resource = struct
  let ht = Hashtbl.create 17
  let c = ref 0
  let create () =
    if Random.int 4 = 0 then Lwt.return_error () else
    begin
      c := succ !c;
      Hashtbl.add ht !c ();
      Lwt.return_ok !c
    end
  let free i = assert (Hashtbl.mem ht i); Hashtbl.remove ht i; Lwt.return_unit
end

let test n =
  let max_idle_size = Random.int 11 in
  let max_size = max 1 (max_idle_size + Random.int 5) in
  let pool =
    Pool.create ~max_idle_size ~max_size Resource.create Resource.free
  in
  let wakers = Array.make n None in
  let wait_count = ref 0 in
  let wait_count_cond = Lwt_condition.create () in
  let wake j u = Lwt.wakeup u (); wakers.(j) <- None in
  for _ = 0 to 3 * n - 1 do
    let j = Random.int n in
    assert (Pool.size pool = Hashtbl.length Resource.ht);
    (match wakers.(j) with
     | None ->
        let waiter, waker = Lwt.wait () in
        incr wait_count;
        let task _ =
          waiter >|= fun () ->
          decr wait_count;
          Lwt_condition.signal wait_count_cond ();
          Ok ()
        in
        Lwt.async begin fun () ->
          Pool.use task pool >>=
          (function
           | Ok () -> Lwt.return_unit
           | Error () ->
              waiter >|= fun () ->
              decr wait_count;
              Lwt_condition.signal wait_count_cond ())
        end;
        wakers.(j) <- Some waker
     | Some u -> wake j u)
  done;
  for j = 0 to n - 1 do
    (match wakers.(j) with
     | None -> ()
     | Some u -> wake j u)
  done;
  let rec wait_for_all () =
    if !wait_count = 0 then Lwt.return_unit else
    Lwt_condition.wait wait_count_cond >>= wait_for_all
  in
  Lwt_unix.with_timeout 2.0 wait_for_all >>= fun () ->
  assert (Pool.size pool <= max_idle_size);
  assert (!wait_count = 0);
  Pool.drain pool >|= fun () ->
  assert (Pool.size pool = 0);
  assert (Hashtbl.length Resource.ht = 0)

let () =
  Lwt_main.run begin
    test 0 >>= fun () ->
    test 1 >>= fun () ->
    let rec loop n_it =
      if n_it = 0 then Lwt.return_unit else
      test (Random.int (1 lsl Random.int 12)) >>= fun () ->
      loop (n_it - 1)
    in
    loop 500
  end
