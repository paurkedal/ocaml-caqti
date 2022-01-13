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

open Lwt.Infix

open Caqti_common_priv
open Testkit

module Ground = struct

  type 'a future = 'a Lwt.t
  let return = Lwt.return
  let catch = Lwt.catch
  let fail = Lwt.fail
  let or_fail = Caqti_lwt.or_fail
  let (>>=) = Lwt.Infix.(>>=)
  let (>|=) = Lwt.Infix.(>|=)
  let (>>=?) = Lwt_result.Infix.(>>=)
  let (>|=?) = Lwt_result.Infix.(>|=)

  module Caqti_sys = Caqti_lwt

  module Alcotest_cli = Testkit.Make_alcotest_cli (Alcotest.Unix_platform) (Lwt)

end

module Test_parallel = Test_parallel.Make (Ground)
module Test_param = Test_param.Make (Ground)
module Test_sql = Test_sql.Make (Ground)
module Test_failure = Test_failure.Make (Ground)

let mk_test (name, pool) =
  let pass_conn pool (name, speed, f) =
    let f' () =
      Caqti_lwt.Pool.use (fun c -> Lwt_result.ok (f c)) pool >|= function
       | Ok () -> ()
       | Error err -> Alcotest.failf "%a" Caqti_error.pp err
    in
    (name, speed, f')
  in
  let pass_pool pool (name, speed, f) = (name, speed, (fun () -> f pool)) in
  let test_cases =
    List.map (pass_conn pool) Test_sql.connection_test_cases @
    List.map (pass_pool pool) Test_parallel.test_cases @
    List.map (pass_conn pool) Test_param.test_cases @
    List.map (pass_conn pool) Test_failure.test_cases @
    List.map (pass_pool pool) Test_sql.pool_test_cases
  in
  (name, test_cases)

let mk_tests uris =
  let connect_pool uri =
    (match
        Caqti_lwt.connect_pool uri
          ~max_size:16
          ~post_connect:Test_sql.post_connect
          ~env:Test_sql.env
     with
     | Ok pool -> (test_name_of_uri uri, pool)
     | Error err -> raise (Caqti_error.Exn err))
  in
  let pools = List.map connect_pool uris in
  List.map mk_test pools

let () = Lwt_main.run begin
  Ground.Alcotest_cli.run_with_args_dependency "test_sql_lwt"
    Testkit.common_args mk_tests
end
