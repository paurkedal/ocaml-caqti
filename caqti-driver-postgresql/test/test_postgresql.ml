(* Copyright (C) 2021--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

module Req = struct
  include Caqti_type.Std
  include Caqti_request.Infix
end

let bad_select_req =
  Req.(unit -->! unit @:- "SELECT not_defined")

let test_error (module C : Caqti_blocking.CONNECTION) =
  (match C.find bad_select_req () with
   | Ok () -> Alcotest.fail "unexpected ok from bad_select"
   | Error (`Request_failed
        {msg = Caqti_driver_postgresql.Result_error_msg {sqlstate; _}; _}) ->
      assert (sqlstate = "42703")
   | Error err ->
      Alcotest.failf "unexpected error from bad_select: %a" Caqti_error.pp err)

let test_cases_on_connection = [
  "test_error", `Quick, test_error;
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
  let is_postgresql uri = Uri.scheme uri = Some "postgresql" in
  let pools = List.map connect_pool (List.filter is_postgresql uris) in
  List.map mk_test pools

let () =
  Alcotest_cli.run_with_args_dependency "test_postgresql"
    (common_args ()) mk_tests
