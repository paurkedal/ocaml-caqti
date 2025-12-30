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

[@@@alert "-caqti_unstable"]
open Caqti.Template

let string_of_field_type : type a. a Field_type.t -> Dialect.t -> string =
  fun field_type dialect ->
  (match field_type, dialect with
   | Bool, _ -> "boolean"
   | (Int16 | Int64), Dialect.Sqlite _ -> "integer"
   | Int, _ -> "integer"
   | Int16, _ -> "smallint"
   | Int32, _ -> "integer"
   | Int64, _ -> "bigint"
   | Float, Dialect.Sqlite _ -> "real"
   | Float, _ -> "double precision"
   | String, _ -> "text" (* TODO: Dialects and size limits. *)
   | Octets, Dialect.Mysql _ -> "longblob"
   | Octets, Dialect.Pgsql _ -> "bytea"
   | Octets, _ -> "varbinary"
   | Pdate, _ -> "date"
   | Ptime, _ -> "timestamp with time zone"
   | Ptime_span, _ -> "interval"
   | Enum name, _ -> name)

module Column_type = struct
  type _ t =
    | Not_nullable : 'a Field_type.t -> 'a t
    | Nullable : 'a Field_type.t -> 'a option t

  let is_required : type a. a t -> bool =
    function Not_nullable _ -> true | Nullable _ -> false

  let row_type : type a. a t -> a Row_type.t = function
   | Not_nullable ft -> Row_type.field ft
   | Nullable ft -> Row_type.(option (field ft))
end

[@@@warning "-30"]
type ('table, 'value) column = {
  column_name: string;
  column_type: 'value Column_type.t;
  is_serial: bool;
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
  mutable schemaless_prefix: string;
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
  let name_q_any (Any col) = Query.lit col.column_name

  let column_type col = col.column_type

  let equal col col' = col.column_name = col'.column_name
  let equal_any (Any col) (Any col') = equal col col'

  let is_required col = Column_type.is_required col.column_type
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
          (match col.column_type with
           | Column_type.Not_nullable ft -> Row_type.field ft
           | Column_type.Nullable ft -> Row_type.(option (field ft)))
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

  let column _subrow column = {
    column_name = column.column_name;
    column_type = column.column_type;
    is_serial = column.is_serial;
  }
end

module Table = struct
  type ('schema, 'table) t = ('schema, 'table) table
  type 'schema any = 'schema any_table =
    Any : ('schema, 'table) t -> 'schema any

  let name table = table.table_name

  let qname table dialect =
    (match table.schema, dialect with
     | None, _ -> table.table_name
     | Some schema, (Dialect.Mysql _ | Dialect.Sqlite _) ->
        schema.schemaless_prefix ^ table.table_name
     | Some schema, _ ->
        schema.schema_name ^ "." ^ table.table_name)

  let columns (table : _ table) = table.columns

  let init_query (table : _ table) dialect =
    let open Caqti.Templater in
    let is_pk columns =
      (match table.primary_key with
       | Some key when List.equal Column.equal_any columns key -> true
       | _ -> false)
    in
    let column_name_q (Column.Any col) = Q.lit col.column_name in
    let column_spec_q (Column.Any col as col') = Q.concat (
      let gen_q =
        if not col.is_serial then [] else
        (match dialect with
         | D.Mysql _ -> [Q.lit " AUTO_INCREMENT"]
         | D.Sqlite _ when is_pk [col'] -> [Q.lit " AUTOINCREMENT"]
         | D.Sqlite _-> []
         | _ -> [Q.lit " GENERATED ALWAYS AS IDENTITY"])
      in
      Q.lit col.column_name ::
      Q.lit " " ::
      (match col.column_type with
       | Not_nullable ft when is_pk [col'] ->
          Q.lit (string_of_field_type ft dialect) :: Q.lit " PRIMARY KEY" :: gen_q
       | Not_nullable ft ->
          Q.lit (string_of_field_type ft dialect) :: Q.lit " NOT NULL" :: gen_q
       | Nullable ft ->
          Q.lit (string_of_field_type ft dialect) :: gen_q)
    ) in
    let columns_qs = List.map column_spec_q table.columns in
    let unique_key_q columns =
      (match columns with
       | [Column.Any col] when is_pk columns && Column.is_required col ->
          None (* done above *)
       | _ ->
          Option.some @@ Q.concat [
            Q.lit "UNIQUE (";
            Q.concat ~sep:", " (List.map column_name_q columns);
            Q.lit ")";
          ])
    in
    let unique_keys_qs = List.filter_map unique_key_q table.unique_keys in
    Q.concat [
      Q.litf "CREATE TABLE %s (" (qname table dialect);
      Q.concat ~sep:", " (columns_qs @ unique_keys_qs);
      Q.lit ")";
    ]

  let init table =
    let open Caqti.Templater in
    direct_gen T.(unit -->. unit) (init_query table)

  let drop_query (table : _ table) dialect =
    let open Caqti.Templater in
    Q.litf "DROP TABLE %s" (qname table dialect)

  let drop table =
    let open Caqti.Templater in
    direct_gen T.(unit -->. unit) (drop_query table)

  let fetch table keyrow valuerow =
    let open Caqti.Templater in
    let q_name (Column.Any col) = Q.lit (Column.name col) in
    let q_cond i (Column.Any col) = Q.concat [
      Q.lit (Column.name col);
      Q.lit " = ";
      Q.param i;
    ] in
    let qf dialect = Q.concat [
      Q.lit "SELECT ";
      Q.concat ~sep:", " (List.map q_name (Subrow.columns valuerow));
      Q.lit " FROM ";
      Q.lit (qname table dialect);
      Q.lit " WHERE ";
      Q.concat ~sep:" AND " (List.mapi q_cond (Subrow.columns keyrow));
    ] in
    direct_gen
      (Subrow.row_type keyrow, Subrow.row_type valuerow, Subrow.row_mult keyrow)
      qf

  let columns_query columns =
    Query.concat ~sep:", " (List.map Column.name_q_any columns)

  let insert_query table subrow dialect =
    let open Caqti.Templater in
    Q.concat [
      Q.litf "INSERT INTO %s (" (qname table dialect);
      columns_query (Subrow.columns subrow);
      Q.lit ") VALUES (";
      Q.concat ~sep:", " (List.init (Subrow.length subrow) Q.param);
      Q.lit ")";
    ]

  let insert table subrow =
    let open Caqti.Templater in
    direct_gen
      T.(Subrow.row_type subrow -->. unit)
      (insert_query table subrow)

  let insert_returning table ins_subrow ret_subrow =
    let open Caqti.Templater in
    let qf dialect = Q.concat [
      Q.litf "INSERT INTO %s (" (qname table dialect);
      columns_query (Subrow.columns ins_subrow);
      Q.lit ") VALUES (";
      Q.concat ~sep:", " (List.init (Subrow.length ins_subrow) Q.param);
      Q.lit ") RETURNING (";
      columns_query (Subrow.columns ret_subrow);
      Q.lit ")"
    ] in
    direct_gen
      T.(Subrow.row_type ins_subrow -->! Subrow.row_type ret_subrow)
      qf

  let assignments_query param_offset subrow =
    let open Caqti.Templater in
    let q_set i (Column.Any col) = Q.concat [
      Q.lit (Column.name col);
      Q.lit " = ";
      Q.param (param_offset + i);
    ] in
    Q.concat ~sep:", " (List.mapi q_set (Subrow.columns subrow))

  let update table keyrow valuerow =
    let open Caqti.Templater in
    let q_cond i (Column.Any col) = Q.concat [
      Q.lit (Column.name col);
      Q.lit " = ";
      Q.param i;
    ] in
    let qf dialect = Q.concat [
      Q.lit "UPDATE ";
      Q.lit (qname table dialect);
      Q.lit " SET ";
      assignments_query (Subrow.length keyrow) valuerow;
      Q.lit " WHERE ";
      Q.concat ~sep:" AND " (List.mapi q_cond (Subrow.columns keyrow));
    ] in
    direct_gen
      T.(t2 (Subrow.row_type keyrow) (Subrow.row_type valuerow) -->. unit)
      qf

  let merge table keyrow valuerow =
    let open Caqti.Templater in
    let insert_q dialect =
      let full_length = Subrow.length keyrow + Subrow.length valuerow in
      Q.concat [
        Q.litf "INSERT INTO %s (" (qname table dialect);
        columns_query (Subrow.columns keyrow @ Subrow.columns valuerow);
        Q.lit ") VALUES (";
        Q.concat ~sep:", " (List.init full_length Q.param);
        Q.lit ")";
      ]
    in
    let qf = function
     | D.Mysql _ as dialect ->
        Q.concat [
          insert_q dialect;
          Q.lit " ON DUPLICATE KEY UPDATE ";
          assignments_query (Subrow.length keyrow) valuerow;
        ]
     | dialect -> (* for PostgreSQL and SQLite3 at least *)
        Q.concat [
          insert_q dialect;
          Q.lit " ON CONFLICT (";
          Q.concat ~sep:", "
            (List.map Column.name_q_any (Subrow.columns keyrow));
          Q.lit ") DO UPDATE SET ";
          assignments_query (Subrow.length keyrow) valuerow;
        ]
    in
    direct_gen
      T.(t2 (Subrow.row_type keyrow) (Subrow.row_type valuerow) -->. unit)
      qf

  let delete table keyrow =
    let open Caqti.Templater in
    let q_cond i (Column.Any col) = Q.concat [
      Q.lit (Column.name col);
      Q.lit " = ";
      Q.param i;
    ] in
    let qf dialect = Q.concat [
      Q.lit "DELETE FROM ";
      Q.lit (qname table dialect);
      Q.lit " WHERE ";
      Q.concat ~sep:" AND " (List.mapi q_cond (Subrow.columns keyrow));
    ] in
    direct_gen
      T.(Subrow.row_type keyrow -->. unit)
      qf
end

module Reference = struct
  type ('schema, 'table, 'foreign_schema, 'foreign_table, 'key) t =
    ('schema, 'table, 'foreign_schema, 'foreign_table, 'key) reference

  type 'schema any = 'schema any_reference =
    Any : ('schema, _, _, _, _) t -> 'schema any

  let constraint_name reference =
    let table_name = Table.name reference.table in
    let Common subrow = reference.subrow in
    let column_name (Column.Any col) = Column.name col in
    let column_names = List.map column_name (Subrow.columns subrow) in
    table_name ^ "_" ^ String.concat "_" column_names ^ "_fkey"

  let init_query reference dialect =
    let open Caqti.Templater in
    let q_colname (Column.Any col) = Q.lit (Column.name col) in
    let Common subrow = reference.subrow in
    let Unique foreign_subrow = reference.foreign_subrow in
    Q.concat [
      Q.lit "ALTER TABLE ";
      Q.lit (Table.qname reference.table dialect);
      Q.lit " ADD CONSTRAINT ";
      Q.lit (constraint_name reference);
      Q.lit " FOREIGN KEY (";
      Q.concat ~sep:", " (List.map q_colname (Subrow.columns subrow));
      Q.lit ") REFERENCES ";
      Q.lit (Table.qname reference.foreign_table dialect);
      Q.lit " (";
      Q.concat ~sep:", " (List.map q_colname (Subrow.columns foreign_subrow));
      Q.lit ")";
    ]

  let drop_query reference dialect =
    let open Caqti.Templater in
    Q.concat [
      Q.lit "ALTER TABLE ";
      Q.lit (Table.qname reference.table dialect);
      Q.lit " DROP CONSTRAINT ";
      Q.lit (constraint_name reference);
    ]
end

module Schema = struct
  type 'schema t = 'schema schema

  let name schema = schema.schema_name
  let tables schema = schema.tables
  let references schema = schema.references

  let init schema =
    let open Caqti.Templater in
    let q_create_schema = Q.litf "CREATE SCHEMA %s" schema.schema_name in
    let qs dialect =
      let init_table (Table.Any table) = Table.init_query table dialect in
      let init_reference (Reference.Any r) = Reference.init_query r dialect in
      (match dialect with
       | D.Sqlite _ ->
          List.map init_table schema.tables
       | _ ->
          List.map init_table schema.tables @
          List.map init_reference schema.references)
    in
    direct_gen_multi @@ function
     | D.Mysql _ | D.Sqlite _ as dialect -> qs dialect
     | dialect -> q_create_schema :: qs dialect

  let drop schema =
    let open Caqti.Templater in
    let q_drop_schema = Q.litf "DROP SCHEMA %s" schema.schema_name in
    let qs tail dialect =
      let drop_table (Table.Any table) = Table.drop_query table dialect in
      let drop_reference (Reference.Any r) = Reference.drop_query r dialect in
      (match dialect with
       | D.Sqlite _ -> []
       | _ -> List.rev_map drop_reference schema.references) @
      List.rev_map drop_table schema.tables @
      tail
    in
    direct_gen_multi @@ function
     | D.Mysql _ | D.Sqlite _ as dialect -> qs [] dialect
     | dialect -> qs [q_drop_schema] dialect
end

module New_table (Table_tag : sig type t end) = struct
  let columns_acc = ref []
  let unique_keys_acc = ref []

  let column column_name field_type =
    let column_type = Column_type.Not_nullable field_type in
    let column = {column_name; column_type; is_serial = false} in
    columns_acc := Column.Any column :: !columns_acc;
    column

  let column_opt column_name field_type =
    let column_type = Column_type.Nullable field_type in
    let column = {column_name; column_type; is_serial = false} in
    columns_acc := Column.Any column :: !columns_acc;
    column

  let serial column_name field_type =
    let column_type = Column_type.Not_nullable field_type in
    let column = {column_name; column_type; is_serial = true} in
    columns_acc := Column.Any column :: !columns_acc;
    column

  let unique intro projections =
    unique_keys_acc := Subrow.extract_columns projections :: !unique_keys_acc;
    Subrow.create_unsafe Row_mult.one intro projections

  let unique_and_present = unique

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
    schemaless_prefix = "";
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
    schema_acc.schemaless_prefix <- schema_name ^ "_";
    schema_acc.tables <- List.rev schema_acc.tables;
    schema_acc.references <- List.rev schema_acc.references;
    schema_acc

end
