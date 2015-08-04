(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

open Caqti_metadata

exception Missing_query_string

let format_query ?env sql lang =
  let n = String.length sql in
  let buf = Buffer.create n in
  let add_substring =
    match env with
    | None -> Buffer.add_substring buf sql
    | Some env ->
      fun i n -> Buffer.add_substitute buf (env lang) (String.sub sql i n) in
  let rec skip_quoted j =
    if j = n then invalid_arg ("format_query: Unmatched quote: " ^ sql) else
    if sql.[j] <> '\'' then skip_quoted (j + 1) else
    if j + 1 < n && sql.[j + 1] = '\'' then skip_quoted (j + 2) else
    j + 1 in
  let rec loop p i j =
    if j = n then add_substring i (j - i) else
    match sql.[j] with
    | '\'' ->
      add_substring i (j - i);
      let k = skip_quoted (j + 1) in
      Buffer.add_substring buf sql j (k - j);
      loop p k k
    | '?' when lang = `Pgsql ->
      add_substring i (j - i);
      Printf.bprintf buf "$%d" (p + 1);
      loop (p + 1) (j + 1) (j + 1)
    | _ ->
      loop p i (j + 1) in
  loop 0 0 0;
  Buffer.contents buf

type oneshot_query = backend_info -> string

type prepared_query = {
  pq_index : int;
  pq_name : string;
  pq_encode : backend_info -> string;
}

type query =
  | Oneshot of oneshot_query
  | Prepared of prepared_query

let oneshot_full f = Oneshot f
let oneshot_fun f = Oneshot (fun {bi_dialect_tag} -> f bi_dialect_tag)
let oneshot_any s = Oneshot (fun _ -> s)
let oneshot_sql s =
  oneshot_fun (function #sql_dialect_tag -> s | _ -> raise Missing_query_string)

let oneshot_sql_p ?env sql = oneshot_fun @@ fun lang ->
  match lang with
  | #sql_dialect_tag -> format_query ?env sql lang
  | _ -> raise Missing_query_string

let next_prepared_index = ref 0

let prepare_full ?name pq_encode =
  let pq_index = !next_prepared_index in
  next_prepared_index := succ !next_prepared_index;
  let pq_name =
    match name with
    | None -> "_s" ^ (string_of_int pq_index)
    | Some name -> name in
  Prepared {pq_index; pq_name; pq_encode}

let prepare_fun ?name f =
  prepare_full ?name (fun {bi_dialect_tag} -> f bi_dialect_tag)

let prepare_any ?name qs = prepare_full ?name (fun _ -> qs)

let prepare_sql ?name s =
  prepare_fun ?name
    (function #sql_dialect_tag -> s | _ -> raise Missing_query_string)

let prepare_sql_p ?name ?env sql = prepare_fun ?name @@ fun lang ->
  match lang with
  | #sql_dialect_tag -> format_query ?env sql lang
  | _ -> raise Missing_query_string

type query_info = [ `Oneshot of string | `Prepared of string * string ]

let make_query_info backend_info = function
  | Oneshot qsf -> `Oneshot (qsf backend_info)
  | Prepared {pq_name; pq_encode} -> `Prepared (pq_name, pq_encode backend_info)
