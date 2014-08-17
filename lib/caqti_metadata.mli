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

(** Information about a database, its backend, and its query language.  This
    is static data supplied by the backend to aid the application in dealing
    with differences between database systems. *)

type dialect_tag = private [> `Mysql | `Pgsql | `Sqlite]
(** A tag used for easy dispatching between query languages. *)

type sql_dialect_tag = [`Mysql | `Pgsql | `Sqlite]
(** Subtype of the above which includes only SQL dialects. *)

type parameter_style = private
  [> `None
  |  `Linear of string
  |  `Indexed of (int -> string) ]
(** How parameters are named.  This is useful for SQL since the difference
    between dialects typically have an intrusive effect on query strings.
    This may also be useful for non-SQL languages which support some form of
    variables or placeholders.
    - [`None] means that non the following parameter styles apply, or that the
      backend does not support parameters at all.
    - [`Linear s] means that occurrences of [s] bind to successive parameters.
    - [`Indexed f] means that an occurrence of [f i] represents parameter
      number [i], counting from 0. *)

type backend_info = private {
  bi_index : int;
  bi_uri_scheme : string;
  bi_dialect_tag : dialect_tag;
  bi_parameter_style : parameter_style;
  bi_default_max_pool_size : int;
  bi_describe_has_typed_parameters : bool;
  bi_describe_has_typed_fields : bool;
}
(** Information about database, backend, and query language. *)

val create_backend_info :
      uri_scheme: string ->
      ?dialect_tag: dialect_tag ->
      ?parameter_style: parameter_style ->
      ?default_max_pool_size: int ->
      describe_has_typed_parameters: bool ->
      describe_has_typed_fields: bool ->
      unit -> backend_info
(** For use by backends to create a descriptor for their query languages. *)
