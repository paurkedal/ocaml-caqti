(* Copyright (C) 2017--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Information about a database, its driver, and its query language.

    This module provides descriptions supplied by the driver to aid the
    application in dealing with differences between database systems. *)

type dialect_tag = private [> `Mysql | `Pgsql | `Sqlite]
(** A tag used for easy dispatching between query languages. *)

type sql_dialect_tag = [`Mysql | `Pgsql | `Sqlite]
(** Subtype of the above which includes only known SQL dialects. *)

type parameter_style = private [>
  | `None
  | `Linear of string
  | `Indexed of (int -> string)
]
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
  dummy_dialect: Caqti_template.Dialect.t ->
  unit -> t
(** The function used by drivers to construct a description of themselves.  For
    an explanation of the parameters, see the corresponding projections. *)

val dummy : t
(** A dummy driver info, useful for instantiating queries for inspection. *)

val uri_scheme : t -> string
(** The URI scheme this backend binds to. *)

val dialect_tag : t -> dialect_tag
(** A variant indicating the SQL dialect or other query language, used for easy
    dispatching when constructing queries.  Can be omitted if non of the cases
    applies, but this means clients must inspect the backend-info to identify
    the language. *)

val parameter_style : t -> parameter_style
(** How to represent parameters in query strings. *)

val can_pool : t -> bool
(** Whether it makes sense to keep connections around for later reuse when using
    this driver.  As hard requirements, the driver must clear any state which
    could affect subsequent operation, and it must reliably detect whether the
    connection is still in a usable state.  As a further indicator, the overhead
    of establishing and closing connections should be high enough that it pays
    of to keep connections around. *)

val can_concur : t -> bool
(** Whether the driver supports concurrent operation.  This is just a hint; it
    is up to the driver to serialize connections with locking primitives or
    other means. *)

val can_transact : t -> bool
(** Whether the database and driver supports transactions. *)

(**/**)
(* Needed to support the old interface. *)
val dummy_dialect : t -> Caqti_template.Dialect.t
val of_dialect : Caqti_template.Dialect.t -> t
