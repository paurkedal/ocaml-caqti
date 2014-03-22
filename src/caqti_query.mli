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

(** Oneshot and prepared queries. *)

type query_language_tag = private
  [> `Mysql | `Pgsql | `Sqlite]

type query_language = private {
  query_language_index : int;
  query_language_name : string;
  query_language_tag : query_language_tag;
}

exception Missing_query_string

val create_query_language : name: string -> ?tag: query_language_tag ->
			    unit -> query_language

type prepared_query = {
  prepared_query_index : int;
  prepared_query_name : string;
  prepared_query_sql : query_language -> string;
}

type query =
  | Prepared of prepared_query
  | Oneshot of string

val prepare_full : ?name: string -> (query_language -> string) -> query
val prepare_fun : ?name: string -> (query_language_tag -> string) -> query
val prepare_any : ?name: string -> string -> query
