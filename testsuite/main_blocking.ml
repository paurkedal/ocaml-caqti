(* Copyright (C) 2018--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

module Test_error_cause = Test_error_cause.Make (Testlib_blocking)
module Test_param = Test_param.Make (Testlib_blocking)
module Test_sql = Test_sql.Make (Testlib_blocking)
module Test_failure = Test_failure.Make (Testlib_blocking)

let mk_test (name, pool) =
  let pass_conn pool (name, speed, f) =
    let f' () =
      Caqti_blocking.Pool.use (fun c -> Ok (f c)) pool |> function
       | Ok () -> ()
       | Error err -> Alcotest.failf "%a" Caqti_error.pp err
    in
    (name, speed, f')
  in
  let pass_pool pool (name, speed, f) = (name, speed, (fun () -> f pool)) in
  let test_cases =
    List.map (pass_conn pool) Test_sql.connection_test_cases @
    List.map (pass_conn pool) Test_error_cause.test_cases @
    List.map (pass_conn pool) Test_param.test_cases @
    List.map (pass_conn pool) Test_failure.test_cases @
    List.map (pass_pool pool) Test_sql.pool_test_cases
  in
  (name, test_cases)

let post_connect conn =
  List_result_future.iter_s (fun f -> f conn) [
    Test_sql.post_connect;
  ]

let env =
  let (&) f g di var = try f di var with Not_found -> g di var in
  Test_sql.env & Test_error_cause.env

let mk_tests {uris; tweaks_version} =
  let connect_pool uri =
    (match Caqti_blocking.connect_pool uri
            ~max_size:1 ~post_connect ?tweaks_version ~env with
     | Ok pool -> (test_name_of_uri uri, pool)
     | Error err -> raise (Caqti_error.Exn err))
  in
  let pools = List.map connect_pool uris in
  List.map mk_test pools

let () =
  Alcotest_cli.run_with_args_dependency "test_sql_blocking"
    Testlib.common_args mk_tests
