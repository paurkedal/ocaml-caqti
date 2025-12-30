(* Copyright (C) 2025  Petter A. Urkedal <paurkedal@gmail.com>
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

[@@@alert "-caqti_unstable"]
open Caqti_template

module Column_type = struct
  type _ t =
    | Required : 'a Field_type.t -> 'a t
    | Optional : 'a Field_type.t -> 'a option t
end

[@@@warning "-30"]
type ('table, 'value) column = {
  column_name: string;
  field_type: 'value Column_type.t;
}
and 'table any_column =
  Any : ('table, 'value) column -> 'table any_column
and ('schema, 'table) table = {
  schema: 'schema schema option;
  table_name: string;
  columns: 'table any_column list;
  primary_key: 'table any_column list option;
  unique_keys: 'table any_column list list;
}
and 'schema any_table =
  Any : ('schema, 'table) table -> 'schema any_table
and 'schema schema = {
  mutable schema_name: string;
  mutable tables: 'schema any_table list;
  mutable references: 'schema any_reference list;
}
and ('schema, 'table, 'foreign_schema, 'foreign_table, 'key) reference = {
  table: ('schema, 'table) table;
  subrow: ('table, 'key) common_subrow;
  foreign_table: ('foreign_schema, 'foreign_table) table;
  foreign_subrow: ('foreign_table, 'key) unique_subrow;
}
and 'schema any_reference =
  Any : ('schema, 'table, 'foreign_schema, 'foreign_table, 'key) reference ->
    'schema any_reference
and ('table, 'value, 'mult) subrow = {
  row_type: 'value Row_type.t;
  row_mult: 'mult Row_mult.t;
  columns: 'table any_column list;
}
and ('table, 'value) common_subrow =
  Common : ('table, 'value, [< Row_mult.zero_or_more]) subrow ->
    ('table, 'value) common_subrow
and ('table, 'value) unique_subrow =
  Unique : ('table, 'value, [< Row_mult.zero_or_one]) subrow ->
    ('table, 'value) unique_subrow

module Column = struct
  type ('table, 'value) t = ('table, 'value) column
  type 'table any = 'table any_column = Any : ('table, _) column -> 'table any

  let name col = col.column_name

  let type_ col = col.field_type

  let equal col col' = col.column_name = col'.column_name
  let equal_any (Any col) (Any col') = equal col col'
end

module Subrow = struct
  type ('table, 'value, 'mult) t = ('table, 'value, 'mult) subrow
    constraint 'mult = [< Row_mult.zero_or_more]

  let row_type subrow = subrow.row_type
  let row_mult subrow = subrow.row_mult
  let columns (subrow : (_, _, _) t) = subrow.columns
  let length (subrow : (_, _, _) t) = List.length subrow.columns
  let equal (subrow : (_, _, _) t) (subrow' : (_, _, _) t) =
    List.equal Column.equal_any subrow.columns subrow'.columns

  type (_, _, _) projections =
    | [] : ('table, 'r, 'r Constructor.return) projections
    | (::) :
        (('table, 'c) Column.t * ('r -> 'c)) * ('table, 'r, 'i) projections ->
        ('table, 'r, 'c -> 'i) projections

  let rec extract_product_type
      : type r i. (_, r, i) projections -> (i, r) Row_type.product =
    (function
     | [] ->
        Row_type.proj_end
     | (col, prj) :: cols ->
        let rt =
          (match col.field_type with
           | Required ft -> Row_type.field ft
           | Optional ft -> Row_type.(option (field ft)))
        in
        Row_type.proj rt prj (extract_product_type cols))

  let rec extract_columns
      : type r i. ('table, r, i) projections -> 'table any_column list =
    (function
     | [] -> []
     | (col, _) :: cols -> Column.Any col :: extract_columns cols)

  let create_unsafe row_mult intro projections =
    let row_type = Row_type.product intro (extract_product_type projections) in
    let columns = extract_columns projections in
    {row_type; row_mult; columns}

  let create intro projections =
    create_unsafe Row_mult.zero_or_more intro projections

  let of_column col = create Result.ok [col, Fun.id]
end

module Table = struct
  type ('schema, 'table) t = ('schema, 'table) table
  type 'schema any = 'schema any_table =
    Any : ('schema, 'table) t -> 'schema any

  let qname table =
    (match table.schema with
     | None -> table.table_name
     | Some schema -> schema.schema_name ^ "." ^ table.table_name)

  let columns (table : _ table) = table.columns

  let init (table : _ table) =
    let open Caqti_template.Create in
    let column_name_q (Column.Any col) = Q.lit col.column_name in
    let column_spec_q (Column.Any col) = Q.concat (
      Q.lit col.column_name ::
      Q.lit " " ::
      (match col.field_type with
       | Required ft -> [Q.lit (Field_type.to_string ft); Q.lit " NOT NULL"]
       | Optional ft -> [Q.lit (Field_type.to_string ft)])
    ) in
    let columns_qs = List.map column_spec_q table.columns in
    let unique_key_q columns =
      let is_primary =
        (match table.primary_key with
         | Some key when List.equal Column.equal_any columns key -> true
         | _ -> false)
      in
      Q.concat [
        Q.lit (if is_primary then "PRIMARY KEY (" else "UNIQUE (");
        Q.concat ~sep:", " (List.map column_name_q columns);
        Q.lit ")";
      ]
    in
    let unique_keys_qs = List.map unique_key_q table.unique_keys in
    let q = Q.concat [
      Q.litf "CREATE TABLE %s (" (qname table);
      Q.concat ~sep:", " (columns_qs @ unique_keys_qs);
      Q.lit ")";
    ] in
    direct_gen T.(unit -->. unit) (Fun.const q)

  let drop (table : _ table) =
    let open Caqti_template.Create in
    let q = Q.litf "DROP TABLE %s" (qname table) in
    direct_gen
      T.(unit -->. unit)
      (Fun.const q)

  let fetch table keyrow valuerow =
    let open Caqti_template.Create in
    let q_name (Column.Any col) = Q.lit (Column.name col) in
    let q_cond i (Column.Any col) = Q.concat [
      Q.lit (Column.name col);
      Q.lit " = ";
      Q.param i;
    ] in
    let q = Q.concat [
      Q.lit "SELECT ";
      Q.concat ~sep:", " (List.map q_name (Subrow.columns valuerow));
      Q.lit " FROM ";
      Q.lit (qname table);
      Q.lit " WHERE ";
      Q.concat ~sep:" AND " (List.mapi q_cond (Subrow.columns keyrow));
    ] in
    direct_gen
      (Subrow.row_type keyrow, Subrow.row_type valuerow, Subrow.row_mult keyrow)
      (Fun.const q)

  let insert table subrow =
    let open Caqti_template.Create in
    let q = Q.concat [
      Q.litf "INSERT INTO %s VALUES (" (qname table);
      Q.concat ~sep:", " (List.init (Subrow.length subrow) Q.param);
      Q.lit ")";
    ] in
    direct_gen
      T.(Subrow.row_type subrow -->. unit)
      (Fun.const q)

  let update table keyrow valuerow =
    let open Caqti_template.Create in
    let n = Subrow.length valuerow in
    let q_set i (Column.Any col) = Q.concat [
      Q.lit (Column.name col);
      Q.lit " = ";
      Q.param (n + i);
    ] in
    let q_cond i (Column.Any col) = Q.concat [
      Q.lit (Column.name col);
      Q.lit " = ";
      Q.param i;
    ] in
    let q = Q.concat [
      Q.lit "UPDATE ";
      Q.lit (qname table);
      Q.lit " SET ";
      Q.concat ~sep:", " (List.mapi q_set (Subrow.columns valuerow));
      Q.lit " WHERE ";
      Q.concat ~sep:" AND " (List.mapi q_cond (Subrow.columns keyrow));
    ] in
    direct_gen
      T.(t2 (Subrow.row_type keyrow) (Subrow.row_type valuerow) -->. unit)
      (Fun.const q)

  let delete table keyrow =
    let open Caqti_template.Create in
    let q_cond i (Column.Any col) = Q.concat [
      Q.lit (Column.name col);
      Q.lit " = ";
      Q.param i;
    ] in
    let q = Q.concat [
      Q.lit "DELETE FROM ";
      Q.lit (qname table);
      Q.lit " WHERE ";
      Q.concat ~sep:" AND " (List.mapi q_cond (Subrow.columns keyrow));
    ] in
    direct_gen
      T.(Subrow.row_type keyrow -->. unit)
      (Fun.const q)
end

module Reference = struct
  type ('schema, 'table, 'foreign_schema, 'foreign_table, 'key) t =
    ('schema, 'table, 'foreign_schema, 'foreign_table, 'key) reference

  type 'schema any = 'schema any_reference =
    Any : ('schema, _, _, _, _) t -> 'schema any

  let init reference =
    let open Caqti_template.Create in
    let q_colname (Column.Any col) = Q.lit (Column.name col) in
    let Common subrow = reference.subrow in
    let Unique foreign_subrow = reference.foreign_subrow in
    let q = Q.concat [
      Q.lit "ALTER TABLE ";
      Q.lit (Table.qname reference.table);
      Q.lit " ADD FOREIGN KEY (";
      Q.concat ~sep:", " (List.map q_colname (Subrow.columns subrow));
      Q.lit ") REFERENCES ";
      Q.lit (Table.qname reference.foreign_table);
      Q.lit " (";
      Q.concat ~sep:", " (List.map q_colname (Subrow.columns foreign_subrow));
      Q.lit ")";
    ] in
    direct_gen T.(unit -->. unit) (Fun.const q)
end

module Schema = struct
  type 'schema t = 'schema schema

  let name schema = schema.schema_name
  let tables schema = schema.tables
  let references schema = schema.references

  let init schema =
    let open Caqti_template.Create in
    let q = (Q.litf "CREATE SCHEMA %s" schema.schema_name) in
    let req = direct_gen T.(unit -->. unit) (Fun.const q) in
    req ::
    (List.map (fun (Table.Any table) -> Table.init table) schema.tables @
     List.map (fun (Reference.Any r) -> Reference.init r) schema.references)
end

module New_table (Table_tag : sig type t end) = struct
  let columns_acc = ref []
  let unique_keys_acc = ref []

  let column column_name field_type =
    let column = {column_name; field_type = Required field_type} in
    columns_acc := Column.Any column :: !columns_acc;
    column

  let column_opt column_name field_type =
    let column = {column_name; field_type = Optional field_type} in
    columns_acc := Column.Any column :: !columns_acc;
    column

  let unique intro projections =
    unique_keys_acc := Subrow.extract_columns projections :: !unique_keys_acc;
    Subrow.create_unsafe Row_mult.zero_or_one intro projections

  let finish ?pk table_name : (Table_tag.t, Table_tag.t) table = {
    schema = None;
    table_name;
    columns = List.rev !columns_acc;
    primary_key = Option.map Subrow.columns pk;
    unique_keys = List.rev !unique_keys_acc;
  }
end

module New_schema (Schema_tag : sig type t end) = struct

  let schema_acc = {
    schema_name = "";
    tables = [];
    references = [];
  }

  let add_table table : (Schema_tag.t, _) Table.t =
    let table = {table with schema = Some schema_acc} in
    schema_acc.tables <- Table.Any table :: schema_acc.tables;
    table

  let foreign_key table subrow foreign_table foreign_subrow =
    let r = {
      table;
      subrow = Common subrow;
      foreign_table;
      foreign_subrow = Unique foreign_subrow;
    } in
    schema_acc.references <- Reference.Any r :: schema_acc.references;
    r

  let finish schema_name =
    schema_acc.schema_name <- schema_name;
    schema_acc.tables <- List.rev schema_acc.tables;
    schema_acc.references <- List.rev schema_acc.references;
    schema_acc

end
