(* Copyright (C) 2019  Petter A. Urkedal <paurkedal@gmail.com>
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

module Query = Caqti_query

let random_letter () = Char.chr (Char.code 'a' + Random.int 26)

let rec random_query n =
  if n <= 1 then
    if Random.bool ()
      then Query.P (Random.int 8)
      else Query.L (String.init (Random.int 3) (fun _ -> random_letter ()))
  else
    Query.S (random_queries n)
and random_queries n =
  if n = 0 then [] else
  if Random.bool () then [random_query n] else
  let m = Random.int (n + 1) in
  random_queries m @ random_queries (n - m)

let test () =
  let q1 = random_query (Random.int 8 + Random.int (1 lsl Random.int 8)) in
  let q2 = random_query (Random.int 8 + Random.int (1 lsl Random.int 8)) in
  let s1 = Query.show q1 in
  let s2 = Query.show q2 in
  if Query.equal q1 q2 then assert (Query.hash q1 = Query.hash q2);
  assert ((s1 = s2) = (Query.(equal (normal q1) (normal q2))))

let () =
  try
    for _ = 0 to 9999 do test () done
  with Failure msg ->
    Printf.eprintf "%s\n" msg;
    exit 1
