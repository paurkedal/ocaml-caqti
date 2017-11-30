(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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
  "SELECT sum(a.y*b.y) FROM test_parallel a JOIN test_parallel b ON a.x < b.x \
    WHERE b.x < ?"

let random_int () = Random.int (1 + Random.int 16)

let (>>=?) m f = m >>= (function Ok x -> f x | Error _ as r -> Lwt.return r)
let (>|=?) m f = m >|= (function Ok x -> f x | Error _ as r -> r)

let report_error = function
 | Ok y -> Lwt.return y
 | Error err ->
    Lwt_io.eprintf "Test failed: %s\n" (Caqti_error.to_string_hum err) >|= fun () ->
    exit 69

let do_query pool =
  pool |> Caqti_lwt.V2.Pool.use @@ fun (module C : Caqti_lwt.V2.CONNECTION) ->
  (match Random.int 4 with
   | 0 ->
      C.exec insert_q (random_int (), random_int ()) >>=? fun () ->
      Lwt.return_ok 0
   | 1 ->
      C.exec delete_q (random_int ()) >>=? fun () ->
      Lwt.return_ok 0
   | 2 ->
      C.fold select_1_q (fun x acc -> x + acc) (random_int ()) 0
   | 3 ->
      C.find select_2_q (random_int ())
      >|=? (function None -> Ok 0 | Some i -> Ok i)
   | _ ->
      assert false)

let rec list_diff f = function
  | x0 :: x1 :: xs -> f x1 x0 :: list_diff f (x1 :: xs)
  | [_] -> []
  | [] -> invalid_arg "list_diff"

let merge f xs acc =
  let rec loop = function
    | [] -> Lwt.return_ok
    | x :: xs -> fun acc -> x >>=? fun y -> loop xs (f y acc) in
  loop xs acc

let rec test2 pool n =
  if n = 0 then Lwt.return_ok 0 else
  if n = 1 then do_query pool else
  let ns = Array.init (Random.int n * (Random.int n + 1) / n + 1)
                      (fun _ -> Random.int n)
    |> Array.to_list |> (fun xs -> n :: xs)
    |> List.sort compare
    |> list_diff (-)
    |> List.filter ((<>) 0) in
  let xs = List.map (test2 pool) ns in
  merge (+) xs 0

let test uri =
  let n_r = ref 1000 in
  let max_size = if Uri.scheme uri = Some "sqlite3" then 1 else 4 in
  Lwt.return (Caqti_lwt.V2.connect_pool ~max_size uri) >>=? fun pool ->
  Caqti_lwt.V2.Pool.use
    (fun (module C : Caqti_lwt.V2.CONNECTION) ->
      C.exec drop_q () >>=? fun () ->
      C.exec create_q ())
    pool >>=? fun () ->
  test2 pool !n_r >>=? fun _ ->
  Caqti_lwt.V2.Pool.use
    (fun (module C : Caqti_lwt.V2.CONNECTION) -> C.exec drop_q ())
    pool

let () = Lwt_main.run begin
  Lwt_list.iter_s (fun uri -> test uri >>= report_error)
    (Testkit.parse_common_args ())
end
