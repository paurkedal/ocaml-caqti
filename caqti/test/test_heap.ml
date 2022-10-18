(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

module H =
  Caqti_private.Heap.Make (struct type t = int let compare = compare end)

let test_push_pop_n n =
  let a = Array.init n (fun _ -> Random.int n) in
  let h = Array.fold_right H.push a H.empty in
  Array.sort (fun i j -> compare j i) a;
  let check_pop x h =
    let x', h' = H.pop_e h in
    assert (x = x'); h' in
  let h' = Array.fold_right check_pop a h in
  assert (H.is_empty h')

let test_push_pop () = for i = 0 to 599 do test_push_pop_n i done

let test_cases = [
  "push, pop", `Quick, test_push_pop;
]
