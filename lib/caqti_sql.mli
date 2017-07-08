(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(** SQL utility functions.

    {b Note.} Since real databases generally do not implement the precise same
    escaping mechanisms, and discrepancies between the escape function and the
    database can lead to SQL injection, the standard SQL escaping functions
    provided below are hardly useful for real applications. *)

val bprint_sql_escaped : Buffer.t -> string -> unit
[@@ocaml.deprecated "Unsuitable for uses with many real databases."]
(** [bprint_sql_escaped buf s] adds an [s] to [buf] with any single quoted
    doubled according to the standard SQL escaping rule.
    @deprecated See note in module introduction. *)

val bprint_sql_quoted : Buffer.t -> string -> unit
[@@ocaml.deprecated "Unsuitable for uses with many real databases."]
(** [bprint_sql_quoted buf s] adds [s] to [buf] as an SQL-quoted string.  Same
    as [bprint_sql_escaped], except surrounded by single quotes.
    @deprecated See note in module introduction. *)

val sql_escaped : string -> string
[@@ocaml.deprecated "Unsuitable for uses with many real databases."]
(** [sql_escaped s] is [s] with single-quotes doubled according to the
    standard SQL escaping rule.
    @deprecated See note in module introduction. *)

val sql_quoted : string -> string
[@@ocaml.deprecated "Unsuitable for uses with many real databases."]
(** [sql_quoted s] is [sql_escaped s] between a pair of single quotes.
    @deprecated See note in module introduction. *)
