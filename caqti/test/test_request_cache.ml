(* Copyright (C) 2025  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_template
open Caqti_platform

module A = Alcotest.V1

module Cache = Request_cache.Make (struct type t = int let weight _ = 1 end)

let dialect =
  Dialect.create_unknown ~purpose:`Dummy () [@alert "-caqti_private"]

let static_select =
  let open Caqti_template.Create in
  static T.(unit -->! int) "SELECT 0"

let dynamic_select i =
  let open Caqti_template.Create in
  let query = Q.("SELECT " ^++ int i) in
  let request_type = T.(unit -->! int) in
  Request.create Dynamic request_type (fun _ -> query)

let is_promoted i = Hashtbl.hash (i : int) land 1 = 0
let is_retained i = Hashtbl.hash (i : int) land 2 = 0
let is_orphaned i = not (is_promoted i || is_retained i)

let test_hit_or_miss () =
  let n = 1000 in
  let all = List.init n Fun.id in
  let promoted = List.filter is_promoted all in
  let retained = List.filter is_retained all in
  let orphaned = List.filter is_orphaned all in
  let retained_requests = Queue.create () in
  let cache =
    Cache.create ~dynamic_capacity:(n - List.length orphaned) dialect
  in
  Cache.add cache static_select (-1);
  for i = 0 to n - 1 do
    A.(check int) "weight" i (Cache.dynamic_weight cache);
    let request = dynamic_select i in
    if is_retained i then Queue.add request retained_requests;
    Cache.add cache request i
  done;
  promoted |> List.iter begin fun i ->
    let elt = Cache.find_and_promote cache (dynamic_select i) in
    A.(check (option int)) "promote" (Some i) elt
  end;

  Gc.compact ();

  let trimmed, commit = Cache.trim ~max_promote_count:n cache in
  A.(check (list int)) "trim" orphaned (List.sort Int.compare trimmed);
  commit ();

  retained |> List.iter begin fun i ->
    A.(check (option int)) "retained dynamic hit" (Some i)
      (Cache.find_and_promote cache (dynamic_select i))
  end;

  promoted |> List.iter begin fun i ->
    A.(check (option int)) "dynamic hit" (Some i)
      (Cache.find_and_promote cache (dynamic_select i))
  end;

  orphaned |> List.iter begin fun i ->
    A.(check (option int)) "dynamic miss" None
      (Cache.find_and_promote cache (dynamic_select i))
  end;

  A.(check (option int)) "static hit" (Some (-1))
    (Cache.find_and_promote cache static_select);

  ignore (Queue.iter ignore retained_requests)

let test_cases = [
  A.test_case "hit or miss" `Quick test_hit_or_miss;
]
