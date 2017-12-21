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

(** (v1) One-shot and prepared queries.
    @deprecated Don't use this in new code. *)

exception Missing_query_string
(** The exception to raise in a callback passed to the various query functions
    below, to indicate that a query language is unsupported. *)

type oneshot_query = Caqti_driver_info.t -> string
(** The type of one-shot queries. *)

type prepared_query = private {
  pq_index : int;
  (** A relatively small integer unique to each query, made available to
      backends for efficient caching of language dependent data. *)

  pq_name : string;
  (** A name to use for the prepared query. *)

  pq_encode : Caqti_driver_info.t -> string;
  (** The SQL for each query language.
      @raise Missing_query_string if the language is not supported. *)
}
(** The type of prepared queries. *)

type query =
  | Oneshot of oneshot_query   (** A one-shot query. *)
  | Prepared of prepared_query (** A prepared query. *)
(** The type of queries accepted by the CONNECTION API. *)

val oneshot_full : (Caqti_driver_info.t -> string) -> query
(** Create a one-shot family of query strings indexed by full query language
    descriptors. *)

val oneshot_fun : (Caqti_driver_info.dialect_tag -> string) -> query
(** Create a one-shot family of query strings indexed by language tags. *)

val oneshot_any : string -> query
(** Create a one-shot query string indiscriminately passed to any backend. *)

val oneshot_sql : string -> query
(** Create a one-shot query string expected to work with any SQL dialect. *)

val oneshot_sql_p :
  ?env: (Caqti_driver_info.dialect_tag -> string -> string) -> string -> query
(** Create a one-shot query string expected to work with any SQL dialect after
    conversion of parameters.

    @param env
      If provided, parameters in the form accepted by {!Buffer.add_substitute}
      will be substituted using this environment. *)

val prepare_full : ?name: string -> (Caqti_driver_info.t -> string) -> query
(** Create a prepared statement dispatching on the full query language
    descriptor.
    @param name A fixed name for the query. *)

val prepare_fun :
  ?name: string -> (Caqti_driver_info.dialect_tag -> string) -> query
(** Create a prepared statement dispatching on the language tag.
    @param name A fixed name for the query. *)

val prepare_any : ?name: string -> string -> query
(** Create a prepared statement accepted by any backend.  This only makes
    sense if backend are known to be restricted to a set sharing a common
    query string.  Consider using {!prepare_sql} or {!oneshot} instead.
    @param name A fixed name for the query. *)

val prepare_sql : ?name: string -> string -> query
(** Create a prepared statement expected to work with any SQL dialect.
    @param name A fixed name for the query. *)

val prepare_sql_p :
  ?name: string -> ?env: (Caqti_driver_info.dialect_tag -> string -> string) ->
  string -> query
(** Create a prepared statement expected to work with any SQL dialect after
    conversion of parameters.

    @param name
      A fixed name for the query.
    @param env
      If provided, parameters in the form accepted by {!Buffer.add_substitute}
      will be substituted using this environment. *)

type query_info = [`Oneshot of string | `Prepared of string * string]
(** An informative analogue of {!query}, used for error reporting. For prepared
    queries, the first constructor argument is the name of the query. *)

val make_query_info : Caqti_driver_info.t -> query -> query_info
(** [make_query_info backend query] extracts the query string from [query] as
    seen by [backend], for use in logging and error reporting. *)
