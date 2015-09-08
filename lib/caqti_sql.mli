(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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

(** SQL Utility Functions *)

val bprint_sql_escaped : Buffer.t -> string -> unit
(** [bprint_sql_escaped buf s] adds an [s] to [buf] with any single quoted
    doubled according to the standard SQL escaping rule. *)

val bprint_sql_quoted : Buffer.t -> string -> unit
(** [bprint_sql_quoted buf s] adds [s] to [buf] as an SQL-quoted string.  Same
    as [bprint_sql_escaped], except surrounded by single quotes. *)

val sql_escaped : string -> string
(** [sql_escaped s] is [s] with single-quotes doubled according to the
    standard SQL escaping rule. *)

val sql_quoted : string -> string
(** [sql_quoted s] is [sql_escaped s] between a pair of single quotes. *)
