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

type query_language_tag = [`Mysql | `Pgsql | `Sqlite | `Other]
type sql_tag = [`Mysql | `Pgsql | `Sqlite]

type query_language = {
  query_language_index : int;
  query_language_name : string;
  query_language_tag : query_language_tag;
}

exception Missing_query_string

let next_query_language_index = ref 0

let create_query_language ~name ?(tag = `Other) () =
  let query_language_index = !next_query_language_index in
  next_query_language_index := succ !next_query_language_index;
  {query_language_index; query_language_name = name; query_language_tag = tag}

type prepared_query = {
  prepared_query_index : int;
  prepared_query_name : string;
  prepared_query_sql : query_language -> string;
}

type query =
  | Oneshot of string
  | Prepared of prepared_query

let oneshot s = Oneshot s

let next_prepared_index = ref 0

let prepare_full ?name prepared_query_sql =
  let prepared_query_index = !next_prepared_index in
  next_prepared_index := succ !next_prepared_index;
  let prepared_query_name =
    match name with
    | None -> "_s" ^ (string_of_int prepared_query_index)
    | Some name -> name in
  Prepared {prepared_query_index; prepared_query_name; prepared_query_sql}

let prepare_fun ?name f =
  prepare_full ?name (fun {query_language_tag} -> f query_language_tag)

let prepare_any ?name qs = prepare_full ?name (fun _ -> qs)

let prepare_sql ?name sql =
  prepare_fun ?name (function #sql_tag -> sql | _ -> raise Missing_query_string)
