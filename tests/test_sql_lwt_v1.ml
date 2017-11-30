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

open Caqti_describe
open Caqti_query
open Printf

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

module Q = struct

  let _q sql = prepare_fun @@ function
    | `Pgsql ->
      let i = ref 1 and buf = Buffer.create (String.length sql + 8) in
      String.iter
        (function '?' -> bprintf buf "$%d" !i; i := succ !i
                | c -> Buffer.add_char buf c)
        sql;
      Buffer.contents buf
    | `Mysql | `Sqlite -> sql
    | _ -> failwith "Unimplemented."

  let select_null_etc = prepare_sql_p
    "SELECT CAST(? AS integer) IS NULL, CAST(? AS integer)"

  let select_and = _q "SELECT ? AND ?"
  let select_plus_int = prepare_fun @@ function
    | `Pgsql -> "SELECT $1::integer + $2::integer"
    | `Mysql | `Sqlite -> "SELECT CAST(? AS integer) + CAST(? AS integer)"
    | _ -> failwith "Unimplemented."
  let select_plus_float = prepare_fun @@ function
    | `Pgsql -> "SELECT $1::float + $2::float"
    | `Mysql | `Sqlite -> "SELECT ? + ?"
    | _ -> failwith "Unimplemented."
  let select_cat = prepare_fun @@ function
    | `Pgsql -> "SELECT $1 || $2"
    | `Mysql -> "SELECT concat(?, ?)"
    | `Sqlite -> "SELECT ? || ?"
    | _ -> failwith "Unimplemented."

  let create_tmp = prepare_fun @@ function
    | `Mysql | `Pgsql ->
        "CREATE TEMPORARY TABLE caqti_test \
          (id SERIAL NOT NULL, i INTEGER NOT NULL, s TEXT NOT NULL)"
    | `Sqlite ->
        "CREATE TABLE caqti_test \
          (id INTEGER PRIMARY KEY, i INTEGER NOT NULL, s TEXT NOT NULL)"
    | _ -> failwith "Unimplemented."
  let insert_into_tmp = prepare_fun @@ function
    | `Pgsql -> "INSERT INTO caqti_test (i, s) VALUES ($1, $2)"
    | `Mysql | `Sqlite -> "INSERT INTO caqti_test (i, s) VALUES (?, ?)"
    | _ -> failwith "Unimplemented."
  let select_from_tmp = prepare_fun @@ function
    | `Mysql | `Pgsql | `Sqlite -> "SELECT i, s FROM caqti_test"
    | _ -> failwith "Unimplemented."
  let select_from_tmp_where_i_lt = prepare_fun @@ function
    | `Pgsql -> "SELECT i, s FROM caqti_test WHERE i < $1"
    | `Mysql | `Sqlite -> "SELECT i, s FROM caqti_test WHERE i < ?"
    | _ -> failwith "Unimplemented."

  let select_current_time = prepare_sql
    "SELECT current_timestamp"
  let select_given_time = prepare_fun @@ function
    | `Pgsql  -> "SELECT $1::timestamp"
    | `Sqlite -> "SELECT ?"
    | `Mysql  -> "SELECT CAST(? AS datetime)"
    | _ -> raise Caqti_query.Missing_query_string
  let compare_to_known_time = prepare_fun @@ function
    | `Pgsql  -> "SELECT $1 = CAST('2017-01-29T12:00:00' AS timestamp)"
    | `Sqlite -> "SELECT ? = '2017-01-29 12:00:00'"
    | `Mysql  -> "SELECT ? = CAST('2017-01-29T12:00:00' AS datetime)"
    | _ -> raise Caqti_query.Missing_query_string
end

let repeat n f =
  let rec loop i =
    if i = n then Lwt.return_unit else
    f i >>= fun () -> loop (i + 1) in
  loop 0

let test_expr (module Db : Caqti1_lwt.CONNECTION) =

  (* Non-prepared. *)
  repeat 200 (fun i ->
    let qs = sprintf "SELECT %d, '%s'" i (string_of_int i) in
    Db.find (oneshot_sql qs) Db.Tuple.(fun u -> int 0 u, string 1 u) [||] >>=
      fun (j, s) ->
    assert (i = j);
    assert (i = int_of_string s);
    Lwt.return_unit
  ) >>= fun () ->

  (* Prepared: null *)
  repeat 3 (fun _ ->
    Db.find Q.select_null_etc Db.Tuple.(fun u -> bool 0 u, is_null 1 u)
            Db.Param.([|null; null|]) >>= fun (c1, c2) ->
    assert (c1 && c2);
    Lwt.return_unit
  ) >>= fun () ->

  (* Prepared: bool *)
  let ck_and a b =
    Db.find Q.select_and
            Db.Tuple.(fun u -> bool 0 u) Db.Param.([|bool a; bool b|])
      >>= fun c ->
    assert (c = (a && b)); Lwt.return_unit in
  ck_and false false >>= fun () -> ck_and false true >>= fun () ->
  ck_and true  false >>= fun () -> ck_and true  true >>= fun () ->

  (* Prepared: int *)
  let ck_plus_int i j =
    Db.find Q.select_plus_int
            Db.Tuple.(fun u -> int 0 u) Db.Param.([|int i; int j|]) >>= fun k ->
    assert (k = (i + j)); Lwt.return_unit in
  repeat 200 (fun _ ->
    let i, j = Random.int (1 lsl 29), Random.int (1 lsl 29) in
    ck_plus_int i j
  ) >>= fun () ->

  (* Prepared: int64 *)
  let ck_plus_int64 i j =
    Db.find Q.select_plus_int
            Db.Tuple.(fun u -> int64 0 u) Db.Param.([|int64 i; int64 j|])
      >>= fun k ->
    assert (k = Int64.add i j); Lwt.return_unit in
  repeat 200 (fun _ ->
    let i = Random.int64 Int64.(shift_left one 29) in
    let j = Random.int64 Int64.(shift_left one 29) in
    ck_plus_int64 i j
  ) >>= fun () ->

  (* Prepared: float *)
  let ck_plus_float x y =
    Db.find Q.select_plus_float
            Db.Tuple.(fun u -> float 0 u) Db.Param.([|float x; float y|])
      >>= fun z ->
    assert (abs_float (z -. (x +. y)) < 1e-6 *. (x +. y));
    Lwt.return_unit in
  repeat 200 (fun _ ->
    let i, j = Random.float 1e8, Random.float 1e8 in
    ck_plus_float i j
  ) >>= fun () ->

  (* Prepared: string *)
  let ck_string x y =
    Db.find Q.select_cat
            Db.Tuple.(fun u -> string 0 u)
            Db.Param.([|string x; string y|]) >>= fun s ->
    assert (s = x ^ y); Lwt.return_unit in
  repeat 200 (fun _ ->
    let x = sprintf "%x" (Random.int (1 lsl 29)) in
    let y = sprintf "%x" (Random.int (1 lsl 29)) in
    ck_string x y
  ) >>= fun () ->

  (* Prepared: date *)
  begin
    let t0 = Unix.time () in
    Db.find Q.select_current_time Db.Tuple.(utc_float 0) [||] >>= fun t ->
    let t1 = Unix.time () in
    Lwt.return (assert (t0 -. 1.1 <= t && t <= t1 +. 1.1)) >>= fun () ->
    Db.find Q.select_given_time
            Db.Tuple.(utc_float 0) Db.Param.[|utc_float t|] >>= fun t' ->
    Lwt.return (assert (abs_float (t' -. t) < 1.1)) >>= fun () ->
    Db.find Q.compare_to_known_time Db.Tuple.(bool 0)
            Db.Param.[|utc_float 1485691200.0|] >>= fun r ->
    Lwt.return (assert r)
  end

let dump_querydesc qn qd =
  let pns = Array.map string_of_typedesc qd.querydesc_params in
  let fns = Array.map (fun (pn, pt) -> pn ^ " : " ^ string_of_typedesc pt)
                      qd.querydesc_fields in
  Lwt_io.printf "%s : %s -> {%s}\n" qn
    (String.concat " * " (Array.to_list pns))
    (String.concat "; " (Array.to_list fns))

let test_table (module Db : Caqti1_lwt.CONNECTION) =

  (* Create, insert, select *)
  Db.exec Q.create_tmp [||] >>= fun () ->
  begin
    if Caqti_driver_info.can_transact Db.driver_info then
      Db.start () >>= fun () ->
      Db.exec Q.insert_into_tmp Db.Param.([|int 1; string "one"|]) >>= fun () ->
      Db.rollback ()
    else
      Lwt.return_unit
  end >>= fun () ->
  Db.start () >>= fun () ->
  Db.exec Q.insert_into_tmp Db.Param.([|int 2; string "two"|]) >>= fun () ->
  Db.exec Q.insert_into_tmp Db.Param.([|int 3; string "three"|]) >>= fun () ->
  Db.exec Q.insert_into_tmp Db.Param.([|int 5; string "five"|]) >>= fun () ->
  Db.commit () >>= fun () ->
  Db.fold Q.select_from_tmp
    Db.Tuple.(fun t (i_acc, s_acc) -> i_acc + int 0 t, s_acc ^ "+" ^ string 1 t)
    [||] (0, "zero") >>= fun (i_acc, s_acc) ->
  assert (i_acc = 10);
  assert (s_acc = "zero+two+three+five");

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

let test (module Db : Caqti1_lwt.CONNECTION) =
  test_expr (module Db) >>= fun () ->
  test_table (module Db) >>= fun () ->
  Db.disconnect ()

let test_pool pool =
  Caqti1_lwt.Pool.use
    begin fun (module Db : Caqti1_lwt.CONNECTION) ->
      test_expr (module Db) >>= fun () ->
      test_table (module Db)
    end
    pool >>= fun () ->
  Caqti1_lwt.Pool.drain pool

let () = Lwt_main.run begin
  Lwt_list.iter_s
    (fun uri ->
      Caqti1_lwt.connect uri >>= test >>= fun () ->
      test_pool (Caqti1_lwt.connect_pool uri))
    (Testkit.parse_common_args ())
end
