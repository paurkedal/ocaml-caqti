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

open Lwt.Infix

module Pool = Caqti_pool.Make (Caqti_lwt.System)

module Resource = struct
  let ht = Hashtbl.create 17
  let c = ref 0
  let create () = c := succ !c; Hashtbl.add ht !c (); Lwt.return !c
  let free i = assert (Hashtbl.mem ht i); Hashtbl.remove ht i; Lwt.return_unit
end

let test n =
  let pool = Pool.create Resource.create Resource.free in
  let a = Array.make n None in
  let wait_count = ref 0 in
  let wake j u = Lwt.wakeup u (); a.(j) <- None in
  for _ = 0 to n - 1 do
    let j = Random.int n in
    assert (Pool.size pool = Hashtbl.length Resource.ht);
    (match a.(j) with
     | None ->
        let waiter, waker = Lwt.wait () in
        wait_count := succ !wait_count;
        Lwt.async
          (fun () ->
            Pool.use
              (fun _ -> waiter >>= fun () ->
                        wait_count := pred !wait_count; Lwt.return_unit)
              pool);
        a.(j) <- Some waker
     | Some u -> wake j u)
  done;
  for j = 0 to n - 1 do
    (match a.(j) with
     | None -> ()
     | Some u -> wake j u)
  done;
  assert (!wait_count = 0);
  Pool.drain pool >|= fun () ->
  assert (Pool.size pool = 0);
  assert (Hashtbl.length Resource.ht = 0)

let () =
  Lwt_main.run begin
    test 0 >>
    test 1 >>
    test 3 >>
    test 8 >>
    test 12 >>
    test 16 >>
    test 64 >>
    test 128 >>
    test 1024
  end
