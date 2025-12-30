(* Copyright (C) 2025--2026  Petter A. Urkedal <paurkedal@gmail.com>
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

module Column_type : sig
  type _ t =
    | Not_nullable : 'a Field_type.t -> 'a t
    | Nullable : 'a Field_type.t -> 'a option t

  val row_type : 'a t -> 'a Row_type.t
end

module Column : sig
  type ('table, 'value) t

  type 'table any = Any : ('table, _) t -> 'table any

  val name : (_, _) t -> string
  val column_type : (_, 'value) t -> 'value Column_type.t
  val equal : ('table, _) t -> ('table, _) t -> bool
end

module Subrow : sig
  type ('table, 'value, 'mult) t constraint 'mult = [< Row_mult.zero_or_more]

  val row_type : ('table, 'value, 'mult) t -> 'value Row_type.t
  val columns : ('table, 'value, 'mult) t -> 'table Column.any list
  val length : ('table, 'value, 'mult) t -> int
  val equal : ('table, _, _) t -> ('table, _, _) t -> bool

  type (_, _, _) projections =
    | [] : ('table, 'r, 'r Constructor.return) projections
    | (::) :
        (('table, 'c) Column.t * ('r -> 'c)) * ('table, 'r, 'i) projections ->
        ('table, 'r, 'c -> 'i) projections

  val create :
    'i -> ('table, 'r, 'i) projections -> ('table, 'r, Row_mult.zero_or_more) t

  val of_column :
    ('table, 'c) Column.t -> ('table, 'c, Row_mult.zero_or_more) t

  val column :
    ('table, 'value, _) t -> ('table, 'c) Column.t -> ('value, 'c) Column.t
end

module Table : sig
  type ('schema, 'table) t
  type 'schema any = Any : ('schema, 'table) t -> 'schema any

  val name : ('schema, 'table) t -> string
  val columns : ('schema, 'table) t -> 'table Column.any list

  val init : ('schema, 'table) t -> (unit, unit, Row_mult.zero) Request.t
  val drop : ('schema, 'table) t -> (unit, unit, Row_mult.zero) Request.t

  val fetch :
    ('schema, 'table) t ->
    ('table, 'key, 'mult) Subrow.t ->
    ('table, 'value, _) Subrow.t ->
    ('key, 'value, 'mult) Request.t

  val insert :
    ('schema, 'table) t ->
    ('table, 'init, _) Subrow.t ->
    ('init, unit, Row_mult.zero) Request.t

  val insert_returning :
    ('schema, 'table) t ->
    ('table, 'init, _) Subrow.t ->
    ('table, 'value, _) Subrow.t ->
    ('init, 'value, Row_mult.one) Request.t

  val update :
    ('schema, 'table) t ->
    ('table, 'key, [< Row_mult.zero_or_one]) Subrow.t ->
    ('table, 'init, [< Row_mult.zero_or_more]) Subrow.t ->
    ('key * 'init, unit, Row_mult.zero) Request.t

  val merge :
    ('schema, 'table) t ->
    ('table, 'key, [< Row_mult.zero_or_one]) Subrow.t ->
    ('table, 'init, [< Row_mult.zero_or_more]) Subrow.t ->
    ('key * 'init, unit, Row_mult.zero) Request.t

  val delete :
    ('schema, 'table) t -> ('table, 'key, [< Row_mult.zero_or_one]) Subrow.t ->
    ('key, unit, Row_mult.zero) Request.t
end

module Reference : sig
  type ('schema, 'table, 'foreign_schema, 'foreign_table, 'key) t
  type 'schema any = Any : ('schema, _, _, _, _) t -> 'schema any
end

module Schema : sig
  type 'schema t

  val name : 'schema t -> string
  val tables : 'schema t -> 'schema Table.any list
  val references : 'schema t -> 'schema Reference.any list

  val init : 'schema t -> (unit, unit, Row_mult.zero) Request.t
  val drop : 'schema t -> (unit, unit, Row_mult.zero) Request.t
end

module New_table (Table_tag : sig type t end) : sig
  val column : string -> 'a Field_type.t -> (Table_tag.t, 'a) Column.t
  val column_opt : string -> 'a Field_type.t -> (Table_tag.t, 'a option) Column.t
  val serial : string -> 'a Field_type.t -> (Table_tag.t, 'a) Column.t

  val unique :
    'i -> (Table_tag.t, 'r, 'i) Subrow.projections ->
    (Table_tag.t, 'r, Row_mult.zero_or_one) Subrow.t

  val unique_and_present :
    'i -> (Table_tag.t, 'r, 'i) Subrow.projections ->
    (Table_tag.t, 'r, Row_mult.one) Subrow.t

  val finish :
    ?pk: (Table_tag.t, 'r, [< Row_mult.zero_or_one]) Subrow.t ->
    string ->
    (Table_tag.t, Table_tag.t) Table.t
end

module New_schema (Schema_tag : sig type t end) : sig

  val add_table : ('table, 'table) Table.t -> (Schema_tag.t, 'table) Table.t

  val foreign_key :
    (Schema_tag.t, 'table) Table.t ->
    ('table, 'key, _) Subrow.t ->
    ('foreign_schema, 'foreign_table) Table.t ->
    ('foreign_table, 'key, [< Row_mult.zero_or_one]) Subrow.t ->
    (Schema_tag.t, 'table, 'foreign_schema, 'foreign_table, 'key) Reference.t

  val finish : string -> Schema_tag.t Schema.t
end
