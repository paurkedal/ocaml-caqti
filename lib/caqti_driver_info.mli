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

(** Information about a database, its driver, and its query language.

    This module provides descriptions supplied by the driver to aid the
    application in dealing with differences between database systems. *)

type dialect_tag = private [> `Mysql | `Pgsql | `Sqlite]
(** A tag used for easy dispatching between query languages. *)

type sql_dialect_tag = [`Mysql | `Pgsql | `Sqlite]
(** Subtype of the above which includes only known SQL dialects. *)

type parameter_style = private
  [> `None
  |  `Linear of string
  |  `Indexed of (int -> string) ]
(** How parameters are named.  This is useful for SQL since the difference
    between dialects typically have an intrusive effect on query strings.
    This may also be useful for non-SQL languages which support some form of
    variables or placeholders.
    - [`None] means that non of the following parameter styles apply, or that
      the driver does not support parameters at all.
    - [`Linear s] means that occurrences of [s] bind to successive parameters.
    - [`Indexed f] means that an occurrence of [f i] represents parameter
      number [i], counting from 0. *)

type t

val create :
  uri_scheme: string ->
  ?dialect_tag: dialect_tag ->
  ?parameter_style: parameter_style ->
  can_pool: bool ->
  can_concur: bool ->
  can_transact: bool ->
  unit -> t
(** Used by drivers to describe themselves.

    @param uri_scheme
      The URI scheme this backend binds to.
    @param dialect_tag
      The {!dialect_tag} indicating the SQL dialect or other query language,
      used for easy dispatching when constructing queries.  Can be omitted if
      non of the cases applies, but this means clients must inspect the
      backend-info to indentify the language.
    @param parameter_style
      How to represent parameters in query strings.
    @param default_max_pool_size
      The suggested pool size for this backend.  Unless the backend is special
      like preferring a single connection, leave this out.
    @param can_pool
      Whether overhead of establishing connections is high enough that it makes
      sense to pool connections for this driver.
    @param can_concur
      Whether the driver supports concurrent operation.  This is just a hint; it
      is up to the driver to serialize connections with locking primitives or
      other means.
    @param can_transact
      Whether the database and driver supports transactions. *)

val uri_scheme : t -> string
val dialect_tag : t -> dialect_tag
val parameter_style : t -> parameter_style
val can_pool : t -> bool
val can_concur : t -> bool
val can_transact : t -> bool
