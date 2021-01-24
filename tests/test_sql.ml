(* Copyright (C) 2014--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_common_priv
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

  let select_plus_int = (tup2 int int --> int) @@ fun _ ->
    "SELECT CAST(? AS integer) + CAST(? AS integer)"
  let select_plus_int64 = (tup2 int64 int64 --> int64) @@ fun _ ->
    "SELECT CAST(? AS integer) + CAST(? AS integer)"
  let select_plus_float = (tup2 float float --> float) @@ function
   | `Mysql -> "SELECT CAST(? AS double) + CAST(? AS double)"
   | _ -> "SELECT CAST(? AS double precision) + CAST(? AS double precision)"
  let select_cat = (tup2 string string --> string) @@ function
   | `Mysql -> "SELECT concat(?, ?)"
   | _ -> "SELECT ? || ?"
  let select_octets_identity = (octets --> octets) @@ function
   | `Mysql -> "SELECT CAST(? AS binary)"
   | `Pgsql -> "SELECT CAST(? AS bytea)"
   | `Sqlite -> "SELECT CAST(? AS blob)"
   | _ -> failwith "Unimplemented."

  let create_tmp = (unit -->! unit) @@ function
   | `Pgsql ->
      "CREATE TEMPORARY TABLE test_sql \
         (id SERIAL NOT NULL, \
          i INTEGER NOT NULL, \
          s TEXT NOT NULL, \
          o BYTEA NOT NULL)"
   | `Mysql ->
      "CREATE TEMPORARY TABLE test_sql \
         (id SERIAL NOT NULL, \
          i INTEGER NOT NULL, \
          s TEXT NOT NULL, \
          o BLOB NOT NULL)"
   | `Sqlite ->
      "CREATE TEMPORARY TABLE test_sql \
         (id INTEGER PRIMARY KEY, \
          i INTEGER NOT NULL, \
          s TEXT NOT NULL, \
          o BLOB NOT NULL)"
   | _ -> failwith "Unimplemented."
  let create_tmp_nullable = (unit -->! unit) @@ function
   | `Pgsql ->
      "CREATE TEMPORARY TABLE test_sql \
        (id SERIAL NOT NULL, i INTEGER NOT NULL, s TEXT, o BYTEA)"
   | `Mysql ->
      "CREATE TEMPORARY TABLE test_sql \
        (id SERIAL NOT NULL, i INTEGER NOT NULL, s TEXT, o BLOB)"
   | `Sqlite ->
      "CREATE TEMPORARY TABLE test_sql \
        (id INTEGER PRIMARY KEY, i INTEGER NOT NULL, s TEXT, o BLOB)"
   | _ -> failwith "Unimplemented."
  let create_tmp_binary = (unit -->! unit) @@ function
   | `Pgsql ->
      "CREATE TEMPORARY TABLE test_sql \
        (id SERIAL NOT NULL, data BYTEA NOT NULL)"
   | `Mysql ->
      "CREATE TEMPORARY TABLE test_sql \
        (id SERIAL NOT NULL, data BLOB NOT NULL)"
   | `Sqlite ->
      "CREATE TEMPORARY TABLE test_sql \
        (id INTEGER PRIMARY KEY, data BLOB NOT NULL)"
   | _ -> failwith "Unimplemented"
  let drop_tmp = (unit -->! unit) @@ function
   | _ -> "DROP TABLE test_sql"
  let insert_into_tmp = Caqti_request.exec (tup3 int string octets)
    "INSERT INTO test_sql (i, s, o) VALUES (?, ?, ?)"
  let update_in_tmp_where_i = Caqti_request.exec (tup2 octets int)
    "UPDATE test_sql SET o = ? WHERE i = ?"
  let update_in_tmp = Caqti_request.exec unit
    "UPDATE test_sql SET s = 'ZERO'"
  let delete_from_tmp_where_i = Caqti_request.exec int
    "DELETE FROM test_sql WHERE i = ?"
  let delete_from_tmp = Caqti_request.exec unit
    "DELETE FROM test_sql"
  let select_from_tmp = Caqti_request.collect unit (tup3 int string octets)
    "SELECT i, s, o FROM test_sql ORDER BY i ASC"
  let select_from_tmp_where_i_lt =
    Caqti_request.collect
      int
      (tup3 int string octets)
      "SELECT i, s, o FROM test_sql WHERE i < ?"
  let select_from_tmp_nullable =
    Caqti_request.collect
      unit
      (tup3 int (option string) (option octets))
      "SELECT i, s, o FROM test_sql"
  let select_from_tmp_binary = Caqti_request.collect unit octets
    "SELECT data FROM test_sql"

  let select_current_time = Caqti_request.find unit ptime
    "SELECT current_timestamp"
  let select_given_time = (ptime --> ptime) @@ function
    | `Pgsql  -> "SELECT CAST(? AS timestamp)"
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

    (* Non-prepared and prepared with non-linear parameters and quotes. *)
    repeat 254 (fun i ->
      let i = i mod 127 + 1 in
      let oneshot = i < 127 in
      let s1 = String.make 1 (Char.chr i) in
      let s2 = String.make i '\'' in
      let req = Caqti_request.create ~oneshot
        Caqti_type.(tup2 int int)
        Caqti_type.(tup4 int int (tup4 int string string string) int)
        Caqti_mult.one
        Caqti_query.(fun di ->
          let quote = match Caqti_driver_info.dialect_tag di with
           | `Mysql -> fun x -> S [L "CAST("; Q x; L" AS CHAR)"]
           | _ -> fun x -> Q x
          in
          S[
            L "SELECT ";
            P 1; L " + 10, ";     (* last parameter first *)
            P 1; L " + 20, ";     (* and duplicated *)
            L (string_of_int i); L ", ";
            quote s1; L ", ";     (* first quote *)
            L "'"; L (string_of_int i); L "'"; L ", ";
            quote s2; L ", ";     (* second quote *)
            P 0; L " + 10";       (* first paramater last *)
          ])
      in
      Db.find req (i + 1, i + 2) >>= Sys.or_fail
        >>= fun (i12, i22, (i', s1', si', s2'), i11) ->
      (if oneshot then Sys.return () else Db.deallocate req >>= Sys.or_fail)
        >|= fun () ->
      assert (i12 = i + 12);
      assert (i22 = i + 22);
      assert (i11 = i + 11);
      assert (i' = i);
      assert (s1' = s1);
      assert (s2' = s2);
      assert (si' = string_of_int i)
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

    (* Prepared: octets *)
    let ck_octets x =
      maybe_deallocate Q.select_cat >>= fun () ->
      Db.find Q.select_octets_identity x >>= Sys.or_fail >>= fun s ->
      assert (s = x); Sys.return () in
    repeat 256 (fun i ->
      let x = sprintf "%c" (Char.chr i) in
      ck_octets x
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
          test_times tfs
      in
      test_times [0.0; -1.2e-5; 1.23e-3; -1.001; 1.23e2; -1.23e5]
    end

  let test_table (module Db : Caqti_sys.CONNECTION) =

    (* Create, insert, select *)
    Db.exec Q.create_tmp () >>= Sys.or_fail >>= fun () ->
    begin
      if Caqti_driver_info.can_transact Db.driver_info then
        Db.start () >>= Sys.or_fail >>= fun () ->
        Db.exec Q.insert_into_tmp (1, "one", "one")
        >>= Sys.or_fail >>= fun () ->
        Db.rollback () >>= Sys.or_fail
      else
        Sys.return ()
    end >>= fun () ->
    Db.start () >>= Sys.or_fail >>= fun () ->
    Db.exec Q.insert_into_tmp (2, "two", "two")
    >>= Sys.or_fail >>= fun () ->
    Db.exec Q.insert_into_tmp (3, "three", "three")
    >>= Sys.or_fail >>= fun () ->
    Db.exec Q.insert_into_tmp (5, "five", "five")
    >>= Sys.or_fail >>= fun () ->
    Db.commit () >>= Sys.or_fail >>= fun () ->
    Db.fold Q.select_from_tmp
      (fun (i, s, o) (i_acc, s_acc, o_acc) ->
        i_acc + i, s_acc ^ "+" ^ s, o_acc ^ "+" ^ o)
      ()
      (0, "zero", "zero")
    >>= Sys.or_fail >>= fun (i_acc, s_acc, o_acc) ->
    assert (i_acc = 10);
    assert (s_acc = "zero+two+three+five");
    assert (o_acc = "zero+two+three+five");
    Db.exec Q.drop_tmp () >>= Sys.or_fail

  let test_affected_count (module Db : Caqti_sys.CONNECTION) =
    let select_all exp_i exp_s exp_o =
      Db.fold Q.select_from_tmp
        (fun (i, s, o) (i_acc, s_acc, o_acc) ->
          i_acc + i, s_acc ^ "+" ^ s, o_acc ^ "+" ^ o)
        ()
        (0, "zero", "zero")
      >>= Sys.or_fail >>= fun (i_acc, s_acc, o_acc) ->
      assert (i_acc = exp_i);
      assert (s_acc = exp_s);
      assert (o_acc = exp_o);
      Sys.return ()
    in
    (* prepare db *)
    Db.exec Q.create_tmp () >>= Sys.or_fail >>= fun () ->
    Db.start () >>= Sys.or_fail >>= fun () ->
    Db.exec_with_affected_count Q.insert_into_tmp (2, "two", "X")
    >>= Sys.or_fail >>= fun nrows ->
    assert (nrows = 1);
    Db.exec Q.insert_into_tmp (3, "three", "Y")
    >>= Sys.or_fail >>= fun () ->
    Db.exec Q.insert_into_tmp (5, "five", "Z")
    >>= Sys.or_fail >>= fun () ->
    select_all 10 "zero+two+three+five" "zero+X+Y+Z" >>= fun () ->

    (* update where i = 1 -> 0 affected rows *)
    Db.exec_with_affected_count Q.update_in_tmp_where_i ("null", 0)
    >>= Sys.or_fail >>= fun nrows ->
    assert (nrows = 0);
    select_all 10 "zero+two+three+five" "zero+X+Y+Z" >>= fun () ->

    (* update where i = 3 -> 1 affected row*)
    Db.exec_with_affected_count Q.update_in_tmp_where_i ("drei", 3)
    >>= Sys.or_fail >>= fun nrows ->
    assert (nrows = 1);
    select_all 10 "zero+two+three+five" "zero+X+drei+Z" >>= fun () ->

    (* update w/o id -> 3 affected rows *)
    Db.exec_with_affected_count Q.update_in_tmp ()
    >>= Sys.or_fail >>= fun nrows ->
    assert (nrows = 3);
    select_all 10 "zero+ZERO+ZERO+ZERO" "zero+X+drei+Z" >>= fun () ->

    (* delete where i = 1 -> no affected rows *)
    Db.exec_with_affected_count Q.delete_from_tmp_where_i 1
    >>= Sys.or_fail >>= fun nrows ->
    assert (nrows = 0);
    select_all 10 "zero+ZERO+ZERO+ZERO" "zero+X+drei+Z" >>= fun () ->

    (* delete where i = 3 -> one affected row *)
    Db.exec_with_affected_count Q.delete_from_tmp_where_i 3
    >>= Sys.or_fail >>= fun nrows ->
    assert (nrows = 1);
    select_all 7 "zero+ZERO+ZERO" "zero+X+Z" >>= fun () ->

    (* delete where i = 3 -> no affected rows *)
    Db.exec_with_affected_count Q.delete_from_tmp_where_i 3
    >>= Sys.or_fail >>= fun nrows ->
    assert (nrows = 0);
    select_all 7 "zero+ZERO+ZERO" "zero+X+Z" >>= fun () ->

    (* delete w/o condition -> 2 affected rows *)
    Db.exec_with_affected_count Q.delete_from_tmp ()
    >>= Sys.or_fail >>= fun nrows ->
    assert (nrows = 2);
    select_all 0 "zero" "zero" >>= fun () ->
    Db.commit () >>= Sys.or_fail >>= fun () ->
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
    Db.exec Q.insert_into_tmp (1, "one", "one") >>= Sys.or_fail >>= fun () ->
    assert_stream_is (Ok [(1, "one", "one")]) >>= Sys.or_fail >>= fun () ->
    Db.exec Q.insert_into_tmp (2, "two", "two") >>= Sys.or_fail >>= fun () ->
    assert_stream_is (Ok [(1, "one", "one"); (2, "two", "two")])
    >>= Sys.or_fail >>= fun () ->
    Db.exec Q.drop_tmp () >>= Sys.or_fail

  let test_stream_both_ways (module Db : Caqti_sys.CONNECTION) =
    let show_string_option = function
      | None -> "None"
      | Some s -> "Some \"" ^ s ^ "\""
    in
    let assert_stream_both_ways expected =
      let input_stream = Caqti_sys.Stream.of_list expected in
      Db.exec Q.create_tmp_nullable () >>= Sys.or_fail >>= fun () ->
      Db.populate
        ~table:"test_sql"
        ~columns:["i"; "s"; "o"]
        Caqti_type.(tup3 int (option string) (option octets))
        input_stream
      >|= Caqti_error.uncongested >>= Sys.or_fail >>= fun () ->
      Db.collect_list Q.select_from_tmp_nullable ()
      >>= Sys.or_fail >>= fun actual ->
      if actual <> expected then
        begin
          let repr a = a
            |> List.map (fun (i, s, o) ->
                let repr_s = show_string_option s in
                let repr_o = show_string_option o in
                "(" ^ (string_of_int i) ^ "," ^ repr_s ^ "," ^ repr_o ^ ")")
            |> String.concat "; "
            |> (fun s -> "[" ^ s ^ "]")
          in
          eprintf "Expected: %s\nActual: %s\n" (repr expected) (repr actual)
        end;
      assert (actual = expected);
      Db.exec Q.drop_tmp ()
    in
    assert_stream_both_ways
      [] >>= Sys.or_fail >>= fun () ->
    assert_stream_both_ways
      [(1, Some "one", Some "one")] >>= Sys.or_fail >>= fun () ->
    assert_stream_both_ways
      [(1, Some "one", Some "one");
       (2, Some "two", Some "two")] >>= Sys.or_fail >>= fun () ->
    assert_stream_both_ways
      [(1, Some "bad1\"\"", Some "bad1\"\"");
       (2, Some "bad2,\"\n", Some "bad2,\"\n");
       (3, None, None);
       (4, Some "", Some "");
       (5, Some "\\\"", Some "\\\"")] >>= Sys.or_fail

  let test_stream_binary (module Db : Caqti_sys.CONNECTION) =
    (* Insert and retrieve all pairs of bytes as strings *)
    let all_bytes =
      Testkit.init_list 256 (fun c -> String.make 1 (Char.chr c))
    in
    let all_pairs = all_bytes
      |> List.map (fun a -> List.map (fun b -> a ^ b) all_bytes)
      |> List.flatten
    in
    let all_pairs_len = List.length all_pairs in
    assert (all_pairs_len = 65536);
    let input_stream = Caqti_sys.Stream.of_list all_pairs in
    Db.exec Q.create_tmp_binary () >>= Sys.or_fail >>= fun () ->
    Db.populate
      ~table:"test_sql"
      ~columns:["data"]
      Caqti_type.octets
      input_stream
    >|= Caqti_error.uncongested >>= Sys.or_fail >>= fun () ->
    Db.collect_list Q.select_from_tmp_binary ()
    >>= Sys.or_fail >>= fun actual ->
    if actual <> all_pairs then
      begin
        let actual_len = List.length actual in
        if actual_len <> all_pairs_len then
          eprintf
            "Expected length: %d\nActual length: %d\n"
            all_pairs_len
            actual_len
        else
          List.iteri
            (fun i (a, e) ->
              if a <> e then
                eprintf
                  "Element in position %d differs: Actual %s, Expected: %s\n"
                  i a (String.escaped e))
            (List.combine actual all_pairs)
      end;
    assert (actual = all_pairs);
    Db.exec Q.drop_tmp () >>= Sys.or_fail

  let run (module Db : Caqti_sys.CONNECTION) =
    test_expr (module Db) >>= fun () ->
    test_table (module Db) >>= fun () ->
    test_affected_count (module Db) >>= fun () ->
    test_stream (module Db) >>= fun () ->
    test_stream_both_ways (module Db) >>= fun () ->
    test_stream_binary (module Db) >>= fun () ->
    Db.disconnect ()

  let run_pool pool =
    Caqti_sys.Pool.use
      begin fun (module Db : Caqti_sys.CONNECTION) ->
        test_expr (module Db) >>= fun () ->
        test_table (module Db) >>= fun () ->
        test_affected_count (module Db) >>= fun () ->
        test_stream (module Db) >>= fun () ->
        test_stream_both_ways (module Db) >>= fun () ->
        test_stream_binary (module Db) >|= fun () ->
        Ok ()
      end
      pool >>= Sys.or_fail >>= fun () ->
    Caqti_sys.Pool.drain pool

end
