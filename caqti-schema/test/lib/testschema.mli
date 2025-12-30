(* Copyright (C) 2026  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti.Template
open Caqti_schema

module User : sig
  type table
  val table : (table, table) Table.t

  val uid : (table, int) Column.t
  val uid_key : (table, int, Row_mult.zero_or_one) Subrow.t
  val name : (table, string) Column.t
  val shell : (table, string) Column.t
  val primary_gid : (table, int) Column.t
end

module Group : sig
  type table
  val table : (table, table) Table.t

  val gid : (table, int) Column.t
  val gid_key : (table, int, Row_mult.zero_or_one) Subrow.t
  val name : (table, string) Column.t
end

type schema
val schema : schema Schema.t

val user : (schema, User.table) Table.t
val group : (schema, Group.table) Table.t

val primary_group_fk : (schema, User.table, schema, Group.table, int) Reference.t
