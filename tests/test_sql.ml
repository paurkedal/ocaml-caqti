(* Copyright (C) 2014--2019  Petter A. Urkedal <paurkedal@gmail.com>
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
      "CREATE TEMPORARY TABLE test_sql \
        (id SERIAL NOT NULL, i INTEGER NOT NULL, s TEXT NOT NULL)"
   | `Sqlite ->
      "CREATE TABLE test_sql \
        (id INTEGER PRIMARY KEY, i INTEGER NOT NULL, s TEXT NOT NULL)"
   | _ -> failwith "Unimplemented."
  let drop_tmp = (unit -->! unit) @@ function
   | _ -> "DROP TABLE test_sql"
  let insert_into_tmp = Caqti_request.exec (tup2 int string)
    "INSERT INTO test_sql (i, s) VALUES (?, ?)"
  let select_from_tmp = Caqti_request.collect unit (tup2 int string)
    "SELECT i, s FROM test_sql"
  let select_from_tmp_where_i_lt = Caqti_request.collect int (tup2 int string)
    "SELECT i, s FROM test_sql WHERE i < ?"

  let select_current_time = Caqti_request.find unit ptime
    "SELECT current_timestamp"
  let select_given_time = (ptime --> ptime) @@ function
    | `Pgsql  -> "SELECT ?::timestamp"
    | `Sqlite -> "SELECT ?"
    | `Mysql  -> "SELECT CAST(? AS datetime)"
    | _ -> failwith "Unimplemented."
  let compare_to_known_time = (ptime --> bool) @@ function
    | `Pgsql  -> "SELECT ? = CAST('2017-01-29T12:00:00.001002' AS timestamp)"
    | `Sqlite -> "SELECT ? = '2017-01-29 12:00:00.001'"
    | `Mysql  -> "SELECT CAST(? AS datetime) \
                       = CAST('2017-01-29T12:00:00.001002' AS datetime)"
    | _ -> failwith "Unimplemented."
  let select_interval = (ptime_span --> ptime_span) @@ function
    | `Pgsql -> "SELECT CAST(? AS interval)"
    | `Sqlite -> "SELECT ?"
    | `Mysql -> "SELECT CAST(? AS double)"
    | _ -> failwith "Unimplemented"
end

module type Sys = sig
  type +'a future
  val or_fail : ('a, [< Caqti_error.t]) result -> 'a future
  val return : 'a -> 'a future
  module Infix : sig
    val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
    val (>|=) : 'a future -> ('a -> 'b) -> 'b future
  end
end

module Make
    (Sys : Sys)
    (Caqti_sys : Caqti_connect_sig.S with type 'a future := 'a Sys.future) =
struct

  open Sys.Infix

  let repeat n f =
    let rec loop i =
      if i = n then Sys.return () else
      f i >>= fun () -> loop (i + 1) in
    loop 0

  let test_expr (module Db : Caqti_sys.CONNECTION) =

    let maybe_deallocate q =
      if Random.int 50 = 0 then
        Db.deallocate q >>= Sys.or_fail
      else
        Sys.return ()
    in

    (* Non-prepared. *)
    repeat 200 (fun i ->
      let req = Caqti_request.find ~oneshot:true
        Caqti_type.unit
        Caqti_type.(tup2 int string)
        (sprintf "SELECT %d, '%s'" i (string_of_int i)) in
      Db.find req () >>= Sys.or_fail >|= fun (j, s) ->
      assert (i = j);
      assert (i = int_of_string s);
    ) >>= fun () ->

    (* Prepared: null *)
    repeat 3 (fun _ ->
      maybe_deallocate Q.select_plus_int >>= fun () ->
      Db.find Q.select_null_etc (None, None) >>= Sys.or_fail >>= fun (c1, c2) ->
      assert (c1 && c2 = None);
      Sys.return ()
    ) >>= fun () ->

    (* Prepared: bool *)
    let ck_and a b =
      maybe_deallocate Q.select_plus_int >>= fun () ->
      Db.find Q.select_and (a, b) >>= Sys.or_fail >|= fun c ->
      assert (c = (a && b)) in
    ck_and false false >>= fun () -> ck_and false true >>= fun () ->
    ck_and true  false >>= fun () -> ck_and true  true >>= fun () ->

    (* Prepared: int *)
    let ck_plus_int i j =
      maybe_deallocate Q.select_plus_int >>= fun () ->
      Db.find Q.select_plus_int (i, j) >>= Sys.or_fail >>= fun k ->
      assert (k = (i + j)); Sys.return () in
    repeat 200 (fun _ ->
      let i, j = Random.int (1 lsl 29), Random.int (1 lsl 29) in
      ck_plus_int i j
    ) >>= fun () ->

    (* Prepared: int64 *)
    let ck_plus_int64 i j =
      maybe_deallocate Q.select_plus_int >>= fun () ->
      Db.find Q.select_plus_int64 (i, j) >>= Sys.or_fail >|= fun k ->
      assert (k = Int64.add i j) in
    repeat 200 (fun _ ->
      let i = Random.int64 Int64.(shift_left one 29) in
      let j = Random.int64 Int64.(shift_left one 29) in
      ck_plus_int64 i j
    ) >>= fun () ->

    (* Prepared: float *)
    let ck_plus_float x y =
      maybe_deallocate Q.select_plus_int >>= fun () ->
      Db.find Q.select_plus_float (x, y) >>= Sys.or_fail >>= fun z ->
      assert (abs_float (z -. (x +. y)) < 1e-6 *. (x +. y));
      Sys.return () in
    repeat 200 (fun _ ->
      let i, j = Random.float 1e8, Random.float 1e8 in
      ck_plus_float i j
    ) >>= fun () ->

    (* Prepared: string *)
    let ck_string x y =
      maybe_deallocate Q.select_plus_int >>= fun () ->
      Db.find Q.select_cat (x, y) >>= Sys.or_fail >>= fun s ->
      assert (s = x ^ y); Sys.return () in
    repeat 200 (fun _ ->
      let x = sprintf "%x" (Random.int (1 lsl 29)) in
      let y = sprintf "%x" (Random.int (1 lsl 29)) in
      ck_string x y
    ) >>= fun () ->

    (* Prepared: time *)
    begin
      let t0 = Ptime_clock.now () in
      Db.find Q.select_current_time () >>= Sys.or_fail >>= fun t ->
      let t1 = Ptime_clock.now () in
      assert (Ptime.to_float_s t0 -. 1.1 <= Ptime.to_float_s t &&
              Ptime.to_float_s t <= Ptime.to_float_s t1 +. 1.1);
      Db.find Q.select_given_time t >>= Sys.or_fail >>= fun t' ->
      assert (Ptime.Span.to_float_s (Ptime.Span.abs (Ptime.diff t t')) < 1.1);
      Db.find Q.compare_to_known_time (Ptime.v (17195, 43200_001_002_000_000L))
               >>= Sys.or_fail >>= fun r ->
      assert r;
      let rec test_times = function
       | [] -> Sys.return ()
       | tf :: tfs ->
          let t =
            (match Ptime.Span.of_float_s tf with
             | Some t -> t
             | None -> assert false) in
          Db.find Q.select_interval t >>= Sys.or_fail >>= fun t' ->
          assert Ptime.Span.(equal (round ~frac_s:6 t) (round ~frac_s:6 t'));
          test_times tfs in
      test_times [0.0; -1.2e-5; 1.23e-3; -1.001; 1.23e2; -1.23e5]
    end

  let test_table (module Db : Caqti_sys.CONNECTION) =

    (* Create, insert, select *)
    Db.exec Q.create_tmp () >>= Sys.or_fail >>= fun () ->
    begin
      if Caqti_driver_info.can_transact Db.driver_info then
        Db.start () >>= Sys.or_fail >>= fun () ->
        Db.exec Q.insert_into_tmp (1, "one") >>= Sys.or_fail >>= fun () ->
        Db.rollback () >>= Sys.or_fail
      else
        Sys.return ()
    end >>= fun () ->
    Db.start () >>= Sys.or_fail >>= fun () ->
    Db.exec Q.insert_into_tmp (2, "two") >>= Sys.or_fail >>= fun () ->
    Db.exec Q.insert_into_tmp (3, "three") >>= Sys.or_fail >>= fun () ->
    Db.exec Q.insert_into_tmp (5, "five") >>= Sys.or_fail >>= fun () ->
    Db.commit () >>= Sys.or_fail >>= fun () ->
    Db.fold Q.select_from_tmp
      (fun (i, s) (i_acc, s_acc) -> i_acc + i, s_acc ^ "+" ^ s)
      () (0, "zero") >>= Sys.or_fail >>= fun (i_acc, s_acc) ->
    assert (i_acc = 10);
    assert (s_acc = "zero+two+three+five");
    Db.exec Q.drop_tmp () >>= Sys.or_fail

  let test_stream (module Db : Caqti_sys.CONNECTION) =
    let assert_stream_is expected =
      Db.call
        ~f:(fun response ->
            let open Db.Response in
            Caqti_sys.Stream.to_list @@ to_stream response >>= fun actual ->
            assert (actual = expected);
            Sys.return (Ok ()))
        Q.select_from_tmp
        ()
    in
    Db.exec Q.create_tmp () >>= Sys.or_fail >>= fun () ->
    assert_stream_is (Ok []) >>= Sys.or_fail >>= fun () ->
    Db.exec Q.insert_into_tmp (1, "one") >>= Sys.or_fail >>= fun () ->
    assert_stream_is (Ok [(1, "one")]) >>= Sys.or_fail >>= fun () ->
    Db.exec Q.insert_into_tmp (2, "two") >>= Sys.or_fail >>= fun () ->
    assert_stream_is (Ok [(1, "one"); (2, "two")]) >>= Sys.or_fail >>= fun () ->
    Db.exec Q.drop_tmp () >>= Sys.or_fail

  let test_stream_both_ways (module Db : Caqti_sys.CONNECTION) =
    let assert_stream_both_ways expected =
      let input_stream = Caqti_sys.Stream.of_list expected in
      Db.exec Q.create_tmp () >>= Sys.or_fail >>= fun () ->
      Db.populate
        ~table:"test_sql"
        ~columns:["i"; "s"]
        Caqti_type.(tup2 int string)
        input_stream
      >|= Caqti_error.uncongested >>= Sys.or_fail >>= fun () ->
      Db.collect_list Q.select_from_tmp () >>= Sys.or_fail >>= fun actual ->
      assert (actual = expected);
      Db.exec Q.drop_tmp ()
    in
    assert_stream_both_ways [] >>= Sys.or_fail >>= fun () ->
    assert_stream_both_ways [(1, "one")] >>= Sys.or_fail >>= fun () ->
    assert_stream_both_ways [(1, "one"); (2, "two")] >>= Sys.or_fail

  let run (module Db : Caqti_sys.CONNECTION) =
    test_expr (module Db) >>= fun () ->
    test_table (module Db) >>= fun () ->
    test_stream (module Db) >>= fun () ->
    test_stream_both_ways (module Db) >>= fun () ->
    Db.disconnect ()

  let run_pool pool =
    Caqti_sys.Pool.use
      begin fun (module Db : Caqti_sys.CONNECTION) ->
        test_expr (module Db) >>= fun () ->
        test_table (module Db) >|= fun () ->
        Ok ()
      end
      pool >>= Sys.or_fail >>= fun () ->
    Caqti_sys.Pool.drain pool

end
