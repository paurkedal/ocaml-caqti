(* Copyright (C) 2022--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eio.Std

open Testlib
open Testlib_eio_unix

module Test_error_cause = Test_error_cause.Make (Testlib_eio_unix)
module Test_param = Test_param.Make (Testlib_eio_unix)
module Test_sql = Test_sql.Make (Testlib_eio_unix)
module Test_failure = Test_failure.Make (Testlib_eio_unix)
module Test_connect = Test_connect.Make (Testlib_eio_unix)

let (%) f g x = f (g x)

let mk_test (name, connect, pool) =
  let pass_connect (name, speed, f) = (name, speed, (fun () -> f connect)) in
  let pass_conn (name, speed, f) =
    let f' () =
      Caqti_eio.Pool.use (fun c -> Ok (f c)) pool |> function
       | Ok () -> ()
       | Error err -> Alcotest.failf "%a" Caqti_error.pp err
    in
    (name, speed, f')
  in
  let pass_pool (name, speed, f) = (name, speed, (fun () -> f pool)) in
  let test_cases =
    List.map pass_conn Test_sql.connection_test_cases @
    List.map pass_conn Test_error_cause.test_cases @
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

let mk_tests (stdenv, sw) {uris; connect_config} =
  let pool_config = Caqti_pool_config.create ~max_size:16 () in
  let create_target uri =
    let connect () =
      Eio.Switch.check sw;
      Caqti_eio_unix.connect ~sw ~stdenv ~config:connect_config ~env uri
    in
    (match Caqti_eio_unix.connect_pool ~sw ~stdenv uri
            ~pool_config ~post_connect ~config:connect_config ~env with
     | Ok pool -> (test_name_of_uri uri, connect, pool)
     | Error err -> raise (Caqti_error.Exn err))
  in
  List.map (mk_test % create_target) uris

let () =
  Mirage_crypto_rng_unix.use_default ();
  Eio_main.run @@ fun stdenv ->
  Switch.run @@ fun sw ->
  Alcotest_cli.run_with_args_dependency "test_sql_eio"
    (Testlib.common_args ())
    (mk_tests ((stdenv :> Caqti_eio.stdenv), sw))
