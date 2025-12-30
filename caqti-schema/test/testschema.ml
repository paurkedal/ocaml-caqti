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

open Caqti_schema

module User = struct

  type tag
  open New_table (struct type t = tag end)

  let uid = column "uid" Int
  let uid_key = unique Result.ok [uid, Fun.id]
  let name = column "name" String
  let shell = column "shell" String
  let primary_gid = column "primary_gid" Int

  let table = finish ~pk:uid_key "user"
end

module Group = struct
  type tag
  open New_table (struct type t = tag end)

  let gid = column "id" Int
  let gid_key = unique Result.ok [gid, Fun.id]
  let name = column "name" String

  let table = finish ~pk:gid_key "group"
end

type tag
open New_schema (struct type t = tag end)

let user = add_table User.table
let group = add_table Group.table

let primary_group_fk =
  foreign_key
    user (Subrow.of_column User.primary_gid)
    group Group.gid_key

let schema = finish "testschema"
