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
open Discography

module Make (Ground : Testlib.Sig.Ground) = struct
  open Ground
  open Ground.Fiber.Infix

  let ( let* ) = (>>=)

  let cleanup (module C : CONNECTION) =
    let q =
      let open Caqti.Templater in
      direct_gen_multi @@ function
       | D.Mysql _ | D.Sqlite _ ->
          let drop_table_q (Table.Any table) =
            Q.litf "DROP TABLE IF EXISTS %s_%s"
              (Schema.name schema) (Table.name table)
          in
          List.map drop_table_q (List.rev (Schema.tables Discography.schema))
       | _ ->
          let schema_name = Schema.name Discography.schema in
          [Q.litf "DROP SCHEMA IF EXISTS %s CASCADE" schema_name]
    in
    C.exec q ()

  let test_init_drop (module C : CONNECTION) =
    cleanup (module C) >>= or_fail >>= fun () ->

    let insert_artist arg = C.find Artist.insert arg >>= or_fail in
    let insert_album name year = C.find Album.insert (name, year) >>= or_fail in
    let relate album artist role =
      C.exec Album_artist_role.merge ((album, artist), role) >>= or_fail
    in
    let relation album_id artist_id =
      C.find_opt Album_artist_role.find (album_id, artist_id) >>= or_fail
    in

    C.exec (Schema.init schema) () >>= or_fail >>= fun () ->
    let* artist1_id = insert_artist "Ayami Suzuki" in
    let* artist2_id = insert_artist "Leo Okagawa" in
    let* album1_id = insert_album "Undercurrent/Wanderlust" 2021 in
    relate album1_id artist1_id (Some "voice & pedals") >>= fun () ->
    relate album1_id artist1_id None >>= fun () ->
    relate album1_id artist2_id None >>= fun () ->
    relate album1_id artist2_id (Some "electronics") >>= fun () ->

    let* artist1 = C.find Artist.fetch artist1_id >>= or_fail in
    let* artist2 = C.find Artist.fetch artist2_id >>= or_fail in

    Alcotest.(check int64) "artist1 id"
      (artist1_id :> int64) (artist1.id :> int64);
    Alcotest.(check string) "artist1 name" "Ayami Suzuki" artist1.name;

    Alcotest.(check int64) "artist2 id"
      (artist2_id :> int64) (artist2.id :> int64);
    Alcotest.(check string) "artist2 name" "Leo Okagawa" artist2.name;

    let* artist1_role = relation album1_id artist1_id in
    let* artist2_role = relation album1_id artist2_id in

    Alcotest.(check (option (option string)))
      "artist1 role" (Some None) artist1_role;
    Alcotest.(check (option (option string)))
      "artist2 role" (Some (Some "electronics")) artist2_role;

    C.exec (Schema.drop schema) () >>= or_fail

  let test_cases = [
    "init-drop", `Quick, test_init_drop;
  ]
end
