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

open Caqti_schema

type schema_tag
open New_schema (struct type t = schema_tag end)

module Artist = struct
  type table_tag
  open New_table (struct type t = table_tag end)

  type id = int64

  let id = serial "id" Int64
  let id_key = unique_and_present Result.ok [id, Fun.id]
  let name = column "name" String
  let name_key = unique Result.ok [name, Fun.id]

  let table = add_table @@ finish ~pk:id_key "artist"

  let insert = Table.insert_returning table name_key id_key

  type t = {
    id: id;
    name: string;
  }

  let row =
    Subrow.create (fun id name -> Ok {id; name}) [
      id, (fun r -> r.id);
      name, (fun r -> r.name);
    ]

  let fetch =
    Table.fetch table id_key row
end

module Album = struct
  type table_tag
  open New_table (struct type t = table_tag end)

  type id = int64

  let id = serial "id" Int64
  let id_key = unique Result.ok [id, Fun.id]
  let title = column "title" String
  let pubyear =  column "pubyear" Int

  let table = add_table @@ finish ~pk:id_key "album"

  let insert =
    let param =
      unique (fun title pubyear -> Ok (title, pubyear)) [
        title, fst;
        pubyear, snd;
      ]
    in
    Table.insert_returning table param id_key
end

module Album_artist_role = struct
  type table_tag
  open New_table (struct type t = table_tag end)

  let album_id = column "album_id" Int64
  let artist_id = column "artist_id" Int64
  let role = column_opt "role" String

  let album_artist_key =
    unique (fun album_id artist_id -> Ok (album_id, artist_id)) [
      album_id, fst;
      artist_id, snd;
    ]

  let table = add_table @@ finish ~pk:album_artist_key "album_artist_role"

  let artist_fkey =
    foreign_key table (Subrow.of_column artist_id) Artist.table Artist.id_key

  let album_fkey =
    foreign_key table (Subrow.of_column album_id) Album.table Album.id_key

  let insert =
    let param =
      unique (fun album_id artist_id descr -> Ok (album_id, artist_id, descr)) [
        album_id, (fun (x, _, _) -> x);
        artist_id, (fun (_, x, _) -> x);
        role, (fun (_, _, x) -> x);
      ]
    in
    Table.insert table param

  let merge = Table.merge table album_artist_key (Subrow.of_column role)

  let find = Table.fetch table album_artist_key (Subrow.of_column role)
end

let schema = finish "discography"
