#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
#require "unix"
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let with_async = Conf.with_pkg "async"
let with_lwt = Conf.with_pkg "lwt"
let with_postgresql = Conf.with_pkg "postgresql"
let with_sqlite3 = Conf.with_pkg "sqlite3"
let with_mariadb = Conf.with_pkg "mariadb"

(* Make locally built backends available for testing. *)
let () = Unix.putenv "OCAMLPATH" "tests"

let opams = [
  Pkg.opam_file ~lint_deps_excluding:(Some ["dynlink"; "findlib"]) "opam"
]

let () = Pkg.describe ~licenses ~opams "caqti" @@ fun c ->
  let have_async = Conf.value c with_async in
  let have_lwt = Conf.value c with_lwt in
  let have_postgresql = Conf.value c with_postgresql in
  let have_sqlite3 = Conf.value c with_sqlite3 in
  let have_mariadb = Conf.value c with_mariadb in
  Ok [
    Pkg.mllib "lib/caqti.mllib";
    Pkg.mllib ~cond:have_postgresql ~api:[] "lib/caqtus-postgresql.mllib";
    Pkg.mllib ~cond:have_sqlite3 ~api:[] "lib/caqtus-sqlite3.mllib";
    Pkg.mllib ~cond:have_mariadb ~api:[] "lib/caqtus-mariadb.mllib";
    Pkg.mllib ~cond:have_async "lib-async/caqti-async.mllib";
    Pkg.mllib ~cond:have_lwt "lib-lwt/caqti-lwt.mllib";
    Pkg.test "tests/test_heap";
    Pkg.test ~cond:have_lwt "tests/test_parallel_lwt";
    Pkg.test ~cond:have_lwt "tests/test_pool_lwt";
    Pkg.test ~cond:have_lwt "tests/test_sql_lwt";
    Pkg.test ~cond:have_async "tests/test_sql_async";
    Pkg.test ~cond:have_lwt "examples/bikereg";
    Pkg.bin ~cond:have_lwt ~dst:"caqtiq" "bin/query";
  ]
