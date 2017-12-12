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

open Caqti_prereq
open Lwt.Infix
open Printf

module Q = struct
  open Caqti_type

  let create_p tA tB mB f =
    Caqti_request.create_p tA tB mB (f % Caqti_driver_info.dialect_tag)

  let (-->!) tA (tR : unit t) f = create_p tA tR Caqti_mult.zero f
  let (-->) tA tR f = create_p tA tR Caqti_mult.one f

  let select_null_etc = Caqti_request.find
    (tup2 (option int) (option int))
    (tup2 bool (option int))
    "SELECT CAST(? AS integer) IS NULL, CAST(? AS integer)"

  let select_and = Caqti_request.find (tup2 bool bool) bool "SELECT ? AND ?"

  let select_plus_int = (tup2 int int --> int) @@ function
   | `Pgsql -> "SELECT ?::integer + ?::integer"
   | `Mysql | `Sqlite -> "SELECT CAST(? AS integer) + CAST(? AS integer)"
   | _ -> failwith "Unimplemented."
  let select_plus_int64 = (tup2 int64 int64 --> int64) @@ function
   | `Pgsql -> "SELECT ?::integer + ?::integer"
   | `Mysql | `Sqlite -> "SELECT CAST(? AS integer) + CAST(? AS integer)"
   | _ -> failwith "Unimplemented."
  let select_plus_float = (tup2 float float --> float) @@ function
   | `Pgsql -> "SELECT ?::float + ?::float"
   | `Mysql -> "SELECT CAST(? AS double) + CAST(? AS double)"
   | `Sqlite -> "SELECT CAST(? AS float) + CAST(? AS float)"
   | _ -> failwith "Unimplemented."
  let select_cat = (tup2 string string --> string) @@ function
   | `Pgsql | `Sqlite -> "SELECT ? || ?"
   | `Mysql -> "SELECT concat(?, ?)"
   | _ -> failwith "Unimplemented."

  let create_tmp = (unit -->! unit) @@ function
   | `Mysql | `Pgsql ->
      "CREATE TEMPORARY TABLE test_sql_lwt_v1 \
        (id SERIAL NOT NULL, i INTEGER NOT NULL, s TEXT NOT NULL)"
   | `Sqlite ->
      "CREATE TABLE test_sql_lwt_v1 \
        (id INTEGER PRIMARY KEY, i INTEGER NOT NULL, s TEXT NOT NULL)"
   | _ -> failwith "Unimplemented."
  let insert_into_tmp = Caqti_request.exec (tup2 int string)
    "INSERT INTO test_sql_lwt_v1 (i, s) VALUES (?, ?)"
  let select_from_tmp = Caqti_request.collect unit (tup2 int string)
    "SELECT i, s FROM test_sql_lwt_v1"
  let select_from_tmp_where_i_lt = Caqti_request.collect int (tup2 int string)
    "SELECT i, s FROM test_sql_lwt_v1 WHERE i < ?"

  let select_current_time = Caqti_request.find unit ptime
    "SELECT current_timestamp"
  let select_given_time = (ptime --> ptime) @@ function
    | `Pgsql  -> "SELECT ?::timestamp"
    | `Sqlite -> "SELECT ?"
    | `Mysql  -> "SELECT CAST(? AS datetime)"
    | _ -> failwith "Unimplemented."
  let compare_to_known_time = (ptime --> bool) @@ function
    | `Pgsql  -> "SELECT ? = CAST('2017-01-29T12:00:00' AS timestamp)"
    | `Sqlite -> "SELECT ? = '2017-01-29 12:00:00'"
    | `Mysql  -> "SELECT ? = CAST('2017-01-29T12:00:00' AS datetime)"
    | _ -> failwith "Unimplemented."
end

let repeat n f =
  let rec loop i =
    if i = n then Lwt.return_unit else
    f i >>= fun () -> loop (i + 1) in
  loop 0

let or_fail = Caqti_lwt.of_result

let test_expr (module Db : Caqti_lwt.CONNECTION) =

  (* Non-prepared. *)
  repeat 200 (fun i ->
    let req = Caqti_request.find ~oneshot:true
      Caqti_type.unit
      Caqti_type.(tup2 int string)
      (sprintf "SELECT %d, '%s'" i (string_of_int i)) in
    Db.find req () >>= or_fail >|= fun (j, s) ->
    assert (i = j);
    assert (i = int_of_string s);
  ) >>= fun () ->

  (* Prepared: null *)
  repeat 3 (fun _ ->
    Db.find Q.select_null_etc (None, None) >>= or_fail >>= fun (c1, c2) ->
    assert (c1 && c2 = None);
    Lwt.return_unit
  ) >>= fun () ->

  (* Prepared: bool *)
  let ck_and a b =
    Db.find Q.select_and (a, b) >>= or_fail >|= fun c ->
    assert (c = (a && b)) in
  ck_and false false >>= fun () -> ck_and false true >>= fun () ->
  ck_and true  false >>= fun () -> ck_and true  true >>= fun () ->

  (* Prepared: int *)
  let ck_plus_int i j =
    Db.find Q.select_plus_int (i, j) >>= or_fail >>= fun k ->
    assert (k = (i + j)); Lwt.return_unit in
  repeat 200 (fun _ ->
    let i, j = Random.int (1 lsl 29), Random.int (1 lsl 29) in
    ck_plus_int i j
  ) >>= fun () ->

  (* Prepared: int64 *)
  let ck_plus_int64 i j =
    Db.find Q.select_plus_int64 (i, j) >>= or_fail >|= fun k ->
    assert (k = Int64.add i j) in
  repeat 200 (fun _ ->
    let i = Random.int64 Int64.(shift_left one 29) in
    let j = Random.int64 Int64.(shift_left one 29) in
    ck_plus_int64 i j
  ) >>= fun () ->

  (* Prepared: float *)
  let ck_plus_float x y =
    Db.find Q.select_plus_float (x, y) >>= or_fail >>= fun z ->
    assert (abs_float (z -. (x +. y)) < 1e-6 *. (x +. y));
    Lwt.return_unit in
  repeat 200 (fun _ ->
    let i, j = Random.float 1e8, Random.float 1e8 in
    ck_plus_float i j
  ) >>= fun () ->

  (* Prepared: string *)
  let ck_string x y =
    Db.find Q.select_cat (x, y) >>= or_fail >>= fun s ->
    assert (s = x ^ y); Lwt.return_unit in
  repeat 200 (fun _ ->
    let x = sprintf "%x" (Random.int (1 lsl 29)) in
    let y = sprintf "%x" (Random.int (1 lsl 29)) in
    ck_string x y
  ) >>= fun () ->

  (* Prepared: time *)
  begin
    let t0 = Ptime_clock.now () in
    Db.find Q.select_current_time () >>= or_fail >>= fun t ->
    let t1 = Ptime_clock.now () in
    assert (Ptime.to_float_s t0 -. 1.1 <= Ptime.to_float_s t &&
            Ptime.to_float_s t <= Ptime.to_float_s t1 +. 1.1);
    Db.find Q.select_given_time t >>= or_fail >>= fun t' ->
    assert (Ptime.Span.to_float_s (Ptime.Span.abs (Ptime.diff t t')) < 1.1);
    Db.find Q.compare_to_known_time (Ptime.v (17195, 43200_000_000_000_000L))
             >>= or_fail >>= fun r ->
    Lwt.return (assert r)
  end

(*
let dump_querydesc qn qd =
  let pns = Array.map string_of_typedesc qd.querydesc_params in
  let fns = Array.map (fun (pn, pt) -> pn ^ " : " ^ string_of_typedesc pt)
                      qd.querydesc_fields in
  Lwt_io.printf "%s : %s -> {%s}\n" qn
    (String.concat " * " (Array.to_list pns))
    (String.concat "; " (Array.to_list fns))
*)

let test_table (module Db : Caqti_lwt.CONNECTION) =

  (* Create, insert, select *)
  Db.exec Q.create_tmp () >>= or_fail >>= fun () ->
  begin
    if Caqti_driver_info.can_transact Db.driver_info then
      Db.start () >>= or_fail >>= fun () ->
      Db.exec Q.insert_into_tmp (1, "one") >>= or_fail >>= fun () ->
      Db.rollback () >>= or_fail
    else
      Lwt.return_unit
  end >>= fun () ->
  Db.start () >>= or_fail >>= fun () ->
  Db.exec Q.insert_into_tmp (2, "two") >>= or_fail >>= fun () ->
  Db.exec Q.insert_into_tmp (3, "three") >>= or_fail >>= fun () ->
  Db.exec Q.insert_into_tmp (5, "five") >>= or_fail >>= fun () ->
  Db.commit () >>= or_fail >>= fun () ->
  Db.fold Q.select_from_tmp
    (fun (i, s) (i_acc, s_acc) -> i_acc + i, s_acc ^ "+" ^ s)
    () (0, "zero") >>= or_fail >>= fun (i_acc, s_acc) ->
  assert (i_acc = 10);
  assert (s_acc = "zero+two+three+five");

(*
  (* Describe *)
  (match Db.describe with
   | None -> Lwt.return_unit
   | Some describe ->
      describe Q.select_from_tmp_where_i_lt >>= fun qd ->
      dump_querydesc "select_from_tmp_where_i_lt" qd >>= fun () ->
      if Caqti_driver_info.describe_has_typed_params Db.driver_info then
        assert (qd.querydesc_params = [|`Int|]);
      if Caqti_driver_info.describe_has_typed_fields Db.driver_info then
        assert (qd.querydesc_fields = [|"i", `Int; "s", `String|]);
      Lwt.return_unit)
*)
  Lwt.return_unit

let test (module Db : Caqti_lwt.CONNECTION) =
  test_expr (module Db) >>= fun () ->
  test_table (module Db) >>= fun () ->
  Db.disconnect ()

let test_pool pool =
  Caqti_lwt.Pool.use
    begin fun (module Db : Caqti_lwt.CONNECTION) ->
      test_expr (module Db) >>= fun () ->
      test_table (module Db) >|= fun () ->
      Ok ()
    end
    pool >>= or_fail >>= fun () ->
  Caqti_lwt.Pool.drain pool

let () = Lwt_main.run begin
  Lwt_list.iter_s
    (fun uri ->
      Lwt.catch
        (fun () ->
          Caqti_lwt.connect uri >>= or_fail >>= test >>= fun () ->
          (match Caqti_lwt.connect_pool uri with
           | Error err -> raise (Caqti_error.Exn err)
           | Ok pool -> test_pool pool))
        (function
         | Caqti_error.Exn err ->
            eprintf "%s\n" (Caqti_error.show err);
            exit 2
         | exn ->
            eprintf "%s raised during test on %s\n"
              (Printexc.to_string exn) (Uri.to_string uri);
            exit 2))
    (Testkit.parse_common_args ())
end
