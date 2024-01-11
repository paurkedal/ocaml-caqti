(* Copyright (C) 2018--2024  Petter A. Urkedal <paurkedal@gmail.com>
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
open Testlib
open Testlib_lwt_unix

module Test_error_cause = Test_error_cause.Make (Testlib_lwt_unix)
module Test_parallel = Test_parallel.Make (Testlib_lwt_unix)
module Test_param = Test_param.Make (Testlib_lwt_unix)
module Test_sql = Test_sql.Make (Testlib_lwt_unix)
module Test_failure = Test_failure.Make (Testlib_lwt_unix)
module Test_connect = Test_connect.Make (Testlib_lwt_unix)

let mk_test (name, connect, pool) =
  let pass_connect (name, speed, f) = (name, speed, (fun () -> f connect)) in
  let pass_conn (name, speed, f) =
    let f' () =
      Caqti_lwt_unix.Pool.use (fun c -> Lwt_result.ok (f c)) pool >|= function
       | Ok () -> ()
       | Error err -> Alcotest.failf "%a" Caqti_error.pp err
    in
    (name, speed, f')
  in
  let pass_pool (name, speed, f) = (name, speed, (fun () -> f pool)) in
  let test_cases =
    List.map pass_conn Test_sql.connection_test_cases @
    List.map pass_conn Test_error_cause.test_cases @
    List.map pass_pool Test_parallel.test_cases @
    List.map pass_conn Test_param.test_cases @
    List.map pass_conn Test_failure.test_cases @
    List.map pass_pool Test_sql.pool_test_cases @
    List.map pass_connect Test_connect.test_cases
  in
  (name, test_cases)

let post_connect conn =
  List_result_fiber.iter_s (fun f -> f conn) [
    Test_sql.post_connect;
  ]

let env =
  let (&) f g di var = try f di var with Not_found -> g di var in
  Test_sql.env & Test_error_cause.env

let mk_tests {uris; connect_config} =
  let pool_config = Caqti_pool_config.create ~max_size:16 () in
  let create_target uri =
    let connect () = Caqti_lwt_unix.connect ~config:connect_config ~env uri in
    (match Caqti_lwt_unix.connect_pool uri
            ~pool_config ~post_connect ~config:connect_config ~env with
     | Ok pool -> (test_name_of_uri uri, connect, pool)
     | Error err -> raise (Caqti_error.Exn err))
  in
  let targets = List.map create_target uris in
  List.map mk_test targets

let () = Lwt_main.run begin
  Alcotest_cli.run_with_args_dependency "test_sql_lwt_unix"
    Testlib.common_args mk_tests
end
