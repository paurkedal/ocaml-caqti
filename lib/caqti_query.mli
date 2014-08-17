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

open Caqti_metadata

exception Missing_query_string
(** The exception to raise in a prepare callback when a query language is not
    supported. *)

type oneshot_query = backend_info -> string
(** The type of one-shot queries. *)

type prepared_query = private {
  prepared_query_index : int;
  (** A relatively small integer unique to each query, made available to
      backends for efficient caching of language dependent data. *)

  prepared_query_name : string;
  (** A name to use for the prepared query. *)

  prepared_query_sql : backend_info -> string;
  (** The SQL for each query language.
      @raise Missing_query_string if the language is not supported. *)
}
(** The type of prepared queries. *)

type query =
  | Oneshot of oneshot_query	(** A one-shot query. *)
  | Prepared of prepared_query	(** A prepared query. *)
(** The type of queries accepted by the CONNECTION API. *)

val oneshot_full : (backend_info -> string) -> query
(** Create a one-shot family of query strings over full query language
    descriptors. *)

val oneshot_fun : (dialect_tag -> string) -> query
(** Create a one-shot family of query strings over language tags. *)

val oneshot_any : string -> query
(** Create a one-shot query string. *)

val oneshot_sql : string -> query
(** Create a one-shot query string expected to work with any SQL dialect. *)

val prepare_full : ?name: string -> (backend_info -> string) -> query
(** Create a prepared statement dispatching on the full query language
    descriptor.  *)

val prepare_fun : ?name: string -> (dialect_tag -> string) -> query
(** Create a prepared statement dispatching on the language tag. *)

val prepare_any : ?name: string -> string -> query
(** Create a prepared statement accepted by any backend.  This only makes
    sense if backend are known to be restricted to a set sharing a common
    query string.  Consider using {!prepare_sql} or {!oneshot} instead. *)

val prepare_sql : ?name: string -> string -> query
(** Create a prepared statement expected to work with any SQL dialect. *)
