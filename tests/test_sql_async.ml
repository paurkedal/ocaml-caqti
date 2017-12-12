(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Core
open Async

module Q = struct
  let create_tmp =
    Caqti_request.create_p Caqti_type.unit Caqti_type.unit Caqti_mult.zero @@
    (function di -> match Caqti_driver_info.dialect_tag di with
     | `Pgsql ->
        "CREATE TEMPORARY TABLE caqti_sql_async \
           (id SERIAL NOT NULL, i INTEGER NOT NULL, s VARCHAR(80) NOT NULL)"
     | `Mysql ->
        "CREATE TEMPORARY TABLE caqti_sql_async \
           (id INTEGER NOT NULL, i INTEGER NOT NULL, s VARCHAR(80) NOT NULL)"
     | `Sqlite ->
        "CREATE TABLE caqti_sql_async \
           (id INTEGER PRIMARY KEY, i INTEGER NOT NULL, s VARCHAR(80) NOT NULL)"
     | _ -> failwith "Unimplemented.")

  let drop_tmp =
    Caqti_request.exec Caqti_type.unit
    "DROP TABLE caqti_sql_async"

  let insert_into_tmp =
    Caqti_request.exec Caqti_type.(tup2 int string)
    "INSERT INTO caqti_sql_async (i, s) VALUES (?, ?)"

  let select_from_tmp =
    Caqti_request.collect Caqti_type.unit Caqti_type.(tup2 int string)
    "SELECT i, s FROM caqti_sql_async"
end

let or_fail = function
 | Ok x -> return x
 | Error err -> Error.raise (Error.of_exn (Caqti_error.Exn err))

let test (module Db : Caqti_async.CONNECTION) =
  let open Deferred in

  (* Create, insert, select *)
  Db.exec Q.create_tmp () >>= or_fail >>= fun () ->
  Db.exec Q.insert_into_tmp (2, "two") >>= or_fail >>= fun () ->
  Db.exec Q.insert_into_tmp (3, "three") >>= or_fail >>= fun () ->
  Db.exec Q.insert_into_tmp (5, "five") >>= or_fail >>= fun () ->
  Db.fold Q.select_from_tmp
    (fun (i, s) (i_acc, s_acc) -> i_acc + i, s_acc ^ "+" ^ s)
    () (0, "zero") >>= or_fail >>= fun (i_acc, s_acc) ->
  assert (i_acc = 10);
  assert (s_acc = "zero+two+three+five");

(*
  (* Describe *)
  (match Db.describe,
         Caqti_driver_info.describe_has_typed_fields Db.driver_info with
   | None, true -> assert false
   | _, false -> return ()
   | Some describe, true ->
      describe Q.select_from_tmp >>= fun qd ->
      assert (qd.querydesc_params = [||]);
      assert (qd.querydesc_fields = [|"i", `Int; "s", `String|]);
      return ()) >>= fun () ->
*)

  (* Drop *)
  Db.exec Q.drop_tmp ()

let test_pool = Caqti_async.Pool.use test
let test_and_close (module Db : Caqti_async.CONNECTION) =
  test (module Db) >>= or_fail >>= fun () -> Db.disconnect ()

let main uris () =
  Shutdown.don't_finish_before begin
    let open Deferred in
    let rec loop = function
     | [] -> return ()
     | uri :: uris ->
        Caqti_async.connect uri >>= or_fail >>= test_and_close >>= fun () ->
        (match Caqti_async.connect_pool uri with
         | Ok pool ->
            test_pool pool >>= or_fail >>= fun () ->
            loop uris
         | Error err ->
            Error.raise (Error.of_exn (Caqti_error.Exn err))) in
    loop uris
  end;
  Shutdown.shutdown 0

let () =
  let uris = Testkit.parse_common_args () in
  never_returns (Scheduler.go_main ~main:(main uris) ())
