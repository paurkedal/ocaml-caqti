(* Copyright (C) 2026  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_schema_test
open Testlib
open Testlib_blocking

module Test_discography = Test_discography.Make (Testlib_blocking)

let mk_test (name, pool) =
  let pass_conn (name, speed, f) =
    let f' () =
      Caqti_blocking.Pool.use (fun c -> Ok (f c)) pool |> function
       | Ok () -> ()
       | Error err -> Alcotest.failf "%a" Caqti.Error.pp err
    in
    (name, speed, f')
  in
(*
  let pass_pool (name, speed, f) = (name, speed, (fun () -> f pool)) in
*)
  let test_cases = List.map pass_conn Test_discography.test_cases in
  (name, test_cases)

let mk_tests {uris; connect_config} =
  let pool_config = Caqti.Pool.Config.create ~max_size:1 () in
  let create_target uri =
    (match Caqti_blocking.connect_pool uri ~pool_config ~config:connect_config with
     | Ok pool -> (test_name_of_uri uri, pool)
     | Error err -> raise (Caqti.Error.Exn err))
  in
  let targets = List.map create_target uris in
  List.map mk_test targets

let () =
  Alcotest_cli.run_with_args_dependency "test_discography"
    (Testlib.common_args ()) mk_tests
