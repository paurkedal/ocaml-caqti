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

type query_language_tag = [`MySQL | `PostgreSQL | `SQLite | `Other]

type query_language = {
  query_language_index : int;
  query_language_name : string;
  query_language_tag : query_language_tag;
}

let next_query_language_index = ref 0

let create_query_language ~name ?(tag = `Other) () =
  let query_language_index = !next_query_language_index in
  next_query_language_index := succ !next_query_language_index;
  {query_language_index; query_language_name = name; query_language_tag = tag}

exception Unsupported

type prepared = {
  prepared_index : int;
  prepared_name : string;
  prepared_sql : query_language -> string;
}

type query =
  | Prepared of prepared
  | Oneshot of string

let next_prepared_index = ref 0

let prepare_full ?name prepared_sql =
  let prepared_index = !next_prepared_index in
  next_prepared_index := succ !next_prepared_index;
  let prepared_name =
    match name with
    | None -> "_s" ^ (string_of_int prepared_index)
    | Some name -> name in
  Prepared {prepared_index; prepared_name; prepared_sql}

let prepare_by_tag ?name f =
  prepare_full ?name (fun {query_language_tag} -> f query_language_tag)

let prepare_any ?name sql = prepare_full (fun _ -> sql)
