(* Copyright (C) 2021--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

open Testlib
open Testlib_blocking

module Req = Caqti_template.Create

let drop_req =
  Req.(static T.(unit -->. unit)) "DROP TABLE IF EXISTS test_sqlite3"

let create_req =
  Req.(direct T.(unit -->. unit))
    "CREATE TABLE test_sqlite3 (integer primary key not null)"

let bad_insert_req =
  Req.(static T.(unit -->! unit)) "INSERT INTO test_sqlite3 VALUES (1), (1)"

let test_error (module C : Caqti_blocking.CONNECTION) =
  C.exec drop_req ()
    |> Result.iter_error (Alcotest.failf "%a" Caqti_error.pp);
  C.exec create_req ()
    |> Result.iter_error (Alcotest.failf "%a" Caqti_error.pp);
  (match C.find bad_insert_req () with
   | Ok () -> Alcotest.fail "unexpected ok from bad_insert"
   | Error (`Request_failed
        {msg = Caqti_driver_sqlite3.Error_msg {errcode; _}; _}) ->
      Alcotest.(check string) "result code"
        "CONSTRAINT" (Sqlite3.Rc.to_string errcode)
   | Error err ->
      Alcotest.failf "unexpected error from bad_insert: %a" Caqti_error.pp err)

let trim_req =
  Req.(static T.(string -->! string) "SELECT trim(?)")

let test_fun (module C : Caqti_blocking.CONNECTION) =
  (match C.driver_connection with
   | Some Caqti_driver_sqlite3.Driver_connection db ->
      Sqlite3.create_fun1 db "trim" begin function
       | TEXT s -> TEXT (String.trim s)
       | x -> x
      end;
      assert (C.find trim_req "  . " = Ok ".")
   | Some _ -> assert false
   | None -> assert false)

let test_cases_on_connection = [
  "test_error", `Quick, test_error;
  "test_fun", `Quick, test_fun;
]

let mk_test (name, pool) =
  let pass_conn (name, speed, f) =
    let f' () =
      Caqti_blocking.Pool.use (fun c -> Ok (f c)) pool |> function
       | Ok () -> ()
       | Error err -> Alcotest.failf "%a" Caqti_error.pp err
    in
    (name, speed, f')
  in
  let test_cases = List.map pass_conn test_cases_on_connection in
  (name, test_cases)

let mk_tests {uris; connect_config = config} =
  let connect_pool uri =
    let pool_config = Caqti_pool_config.create ~max_size:1 () in
    (match Caqti_blocking.connect_pool uri ~pool_config ~config with
     | Ok pool -> (test_name_of_uri uri, pool)
     | Error err -> raise (Caqti_error.Exn err))
  in
  let is_sqlite3 uri = Uri.scheme uri = Some "sqlite3" in
  let pools = List.map connect_pool (List.filter is_sqlite3 uris) in
  List.map mk_test pools

let () =
  Alcotest_cli.run_with_args_dependency "test_sqlite3" (common_args ()) mk_tests
