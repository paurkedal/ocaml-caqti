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
(** A tag used for easy dispatching between query languages. *)

type sql_tag = private [> `Mysql | `Pgsql | `Sqlite]

type query_language = private {
  query_language_index : int;
  (** A small integer unique for each backend, made available to backends for
      efficient caching of language dependent data. *)

  query_language_name : string;

  query_language_tag : query_language_tag;
  (** The language tag used for easy dispatching between known query
      languages. *)
}
(** Information about a query language. *)

exception Missing_query_string
(** The exception to raise in a prepare callback when a query language is not
    supported. *)

val create_query_language : name: string -> ?tag: query_language_tag ->
			    unit -> query_language
(** For use by backends to create a descriptor for their query languages. *)

type oneshot_query = query_language -> string
(** The type of one-shot queries. *)

type prepared_query = private {
  prepared_query_index : int;
  (** A relatively small integer unique to each query, made available to
      backends for efficient caching of language dependent data. *)

  prepared_query_name : string;
  (** A name to use for the prepared query. *)

  prepared_query_sql : query_language -> string;
  (** The SQL for each query language.
      @raise Missing_query_string if the language is not supported. *)
}
(** The type of prepared queries. *)

type query =
  | Oneshot of oneshot_query	(** A one-shot query. *)
  | Prepared of prepared_query	(** A prepared query. *)
(** The type of queries accepted by the CONNECTION API. *)

val oneshot_full : (query_language -> string) -> query
(** Create a one-shot family of query strings over full query language
    descriptors. *)

val oneshot_fun : (query_language_tag -> string) -> query
(** Create a one-shot family of query strings over language tags. *)

val oneshot_any : string -> query
(** Create a one-shot query string. *)

val oneshot_sql : string -> query
(** Create a one-shot query string expected to work with any SQL dialect. *)

val prepare_full : ?name: string -> (query_language -> string) -> query
(** Create a prepared statement dispatching on the full query language
    descriptor.  *)

val prepare_fun : ?name: string -> (query_language_tag -> string) -> query
(** Create a prepared statement dispatching on the language tag. *)

val prepare_any : ?name: string -> string -> query
(** Create a prepared statement accepted by any backend.  This only makes
    sense if backend are known to be restricted to a set sharing a common
    query string.  Consider using {!prepare_sql} or {!oneshot} instead. *)

val prepare_sql : ?name: string -> string -> query
(** Create a prepared statement expected to work with any SQL dialect. *)
