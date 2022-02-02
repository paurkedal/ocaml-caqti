(* Copyright (C) 2015--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

let create_q = Caqti_request.exec Caqti_type.unit
  "CREATE TABLE test_parallel (x int NOT NULL, y int NOT NULL)"
let drop_q = Caqti_request.exec Caqti_type.unit
  "DROP TABLE IF EXISTS test_parallel"
let insert_q = Caqti_request.exec Caqti_type.(tup2 int int)
  "INSERT INTO test_parallel VALUES (?, ?)"
let delete_q = Caqti_request.exec Caqti_type.int
  "DELETE FROM test_parallel WHERE x = ?"
let select_1_q = Caqti_request.collect Caqti_type.int Caqti_type.int
  "SELECT y FROM test_parallel WHERE x < ?"
let select_2_q = Caqti_request.find Caqti_type.int Caqti_type.(option int)
  "SELECT sum(a.y*b.y) \
   FROM test_parallel a JOIN test_parallel b ON a.x < b.x \
   WHERE b.x < ?"

let random_int () = Random.int (1 + Random.int 16)

module Make (Ground : Testlib.Sig.Ground) = struct
  open Ground

  let do_query pool =
    pool |> Caqti_sys.Pool.use @@ fun (module C : Caqti_sys.CONNECTION) ->
    (match Random.int 4 with
     | 0 ->
        C.exec insert_q (random_int (), random_int ()) >>=? fun () ->
        return (Ok 0)
     | 1 ->
        C.exec delete_q (random_int ()) >>=? fun () ->
        return (Ok 0)
     | 2 ->
        C.fold select_1_q (fun x acc -> x + acc) (random_int ()) 0
     | 3 ->
        C.find select_2_q (random_int ())
          >|=? (function None -> 0 | Some i -> i)
     | _ ->
        assert false)

  let rec list_diff f = function
   | x0 :: x1 :: xs -> f x1 x0 :: list_diff f (x1 :: xs)
   | [_] -> []
   | [] -> invalid_arg "list_diff"

  let reduce f xs acc =
    let rec loop = function
     | [] -> fun acc -> return (Ok acc)
     | mx :: mxs -> fun acc -> mx >>=? fun x -> loop mxs (f x acc)
    in
    loop xs acc

  let rec test_parallel' pool n =
    if n = 0 then return (Ok 0) else
    if n = 1 then do_query pool else
    let thread_count = Random.int n * (Random.int n + 1) / n + 1 in
    let ns = Array.init thread_count (fun _ -> Random.int n)
      |> Array.to_list |> (fun xs -> n :: xs)
      |> List.sort compare
      |> list_diff (-)
      |> List.filter ((<>) 0)
    in
    let xs = List.map (test_parallel' pool) ns in
    reduce (+) xs 0

  let test_parallel pool =
    begin
      Caqti_sys.Pool.use
        (fun (module C : Caqti_sys.CONNECTION) ->
          C.exec drop_q () >>=? fun () ->
          C.exec create_q ())
        pool >>=? fun () ->
      test_parallel' pool 1000
    end >|= function
     | Ok _ -> ()
     | Error err -> Alcotest.failf "%a" Caqti_error.pp err

  let test_cases = [
    "parallel", `Slow, test_parallel;
  ]

end
