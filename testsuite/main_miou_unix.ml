open Testlib
open Testlib_miou_unix

module Test_error_cause = Test_error_cause.Make (Testlib_miou_unix)
module Test_parallel = Test_parallel.Make (Testlib_miou_unix)
module Test_param = Test_param.Make (Testlib_miou_unix)
module Test_sql = Test_sql.Make (Testlib_miou_unix)
module Test_failure = Test_failure.Make (Testlib_miou_unix)
module Test_connect = Test_connect.Make (Testlib_miou_unix)

let mk_test (name, connect, pool) =
  let pass_connect (name, speed, f) = (name, speed, (fun () -> f connect)) in
  let pass_conn (name, speed, f) =
    let f' () =
      match Caqti_miou_unix.Pool.use (fun c -> Ok (f c)) pool with
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

let mk_tests sw {uris; connect_config} =
  let pool_config = Caqti_pool_config.create ~max_size:16 () in
  let create_target uri =
    let connect () = Caqti_miou_unix.connect ~sw ~config:connect_config ~env uri in
    (match Caqti_miou_unix.connect_pool ~sw uri
            ~pool_config ~post_connect ~config:connect_config ~env with
     | Ok pool -> (test_name_of_uri uri, connect, pool)
     | Error err -> raise (Caqti_error.Exn err))
  in
  let targets = List.map create_target uris in
  List.map mk_test targets

let () = Miou_unix.run @@ fun () ->
  let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
  let finally () = Mirage_crypto_rng_miou_unix.kill rng in
  Fun.protect ~finally @@ fun () ->
  Caqti_miou.Switch.run @@ fun sw ->
  Alcotest_cli.run_with_args_dependency "test_sql_miou_unix"
    (Testlib.common_args ()) (mk_tests sw)
