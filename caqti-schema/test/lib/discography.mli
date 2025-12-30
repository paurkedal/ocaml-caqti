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

type schema_tag
val schema : schema_tag Schema.t

module Artist : sig
  type table_tag
  type id = private int64

  val id : (table_tag, id) Column.t
  val id_key : (table_tag, id, Row_mult.one) Subrow.t
  val name : (table_tag, string) Column.t

  val table : (schema_tag, table_tag) Table.t

  val insert : (string, id, Row_mult.one) Request.t

  type t = {
    id: id;
    name: string;
  }
  val fetch : (id, t, Row_mult.one) Request.t
end

module Album : sig
  type table_tag
  type id = private int64

  val id : (table_tag, id) Column.t
  val id_key : (table_tag, id, Row_mult.zero_or_one) Subrow.t
  val title : (table_tag, string) Column.t
  val pubyear : (table_tag, int) Column.t

  val table : (schema_tag, table_tag) Table.t

  val insert : (string * int, id, Row_mult.one) Request.t
end

module Album_artist_role : sig
  type table_tag

  val album_id : (table_tag, Album.id) Column.t
  val album_fkey :
    (schema_tag, table_tag, schema_tag, Album.table_tag, Album.id) Reference.t

  val artist_id : (table_tag, Artist.id) Column.t
  val artist_fkey :
    (schema_tag, table_tag, schema_tag, Artist.table_tag, Artist.id) Reference.t

  val album_artist_key :
    (table_tag, Album.id * Artist.id, Row_mult.zero_or_one) Subrow.t

  val role : (table_tag, string option) Column.t

  val table : (schema_tag, table_tag) Table.t

  val insert :
    (Album.id * Artist.id * string option, unit, Row_mult.zero) Request.t

  val merge :
    ((Album.id * Artist.id) * string option, unit, Row_mult.zero) Request.t

  val find :
    (Album.id * Artist.id, string option, Row_mult.zero_or_one) Request.t
end
