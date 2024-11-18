(* Copyright (C) 2022--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

module Make (Ground : Testlib.Sig.Ground) = struct
  open Ground
  open Ground.Fiber.Infix

  let env driver_info = function
   | "engine_innodb" ->
      (match Caqti_driver_info.dialect_tag driver_info with
       | `Mysql -> Caqti_query.L" ENGINE = InnoDB"
       | _ -> Caqti_query.S[])
   | _ -> raise Not_found

  let create_reqs =
    List.map Caqti_template.Create.(unit ->. unit) [
      "DROP TABLE IF EXISTS caqti_test_publication";
      "DROP TABLE IF EXISTS caqti_test_genre";
      "CREATE TABLE caqti_test_genre \
        (id INTEGER PRIMARY KEY, \
         genre VARCHAR(40) UNIQUE NOT NULL CHECK (genre != ''))\
        $(engine_innodb)";
      "CREATE TABLE caqti_test_publication \
        (title VARCHAR(160) UNIQUE NOT NULL, \
         author TEXT NOT NULL, \
         genre_id INTEGER NOT NULL, \
         FOREIGN KEY (genre_id) REFERENCES caqti_test_genre (id))\
        $(engine_innodb)";
      "INSERT INTO caqti_test_genre VALUES (1, 'fiction'), (2, 'nonfiction')";
      "INSERT INTO caqti_test_publication VALUES ('Fiction', 'N.N.', 2)"
    ]

  let not_null_violation_req =
    Caqti_template.Create.(unit ->. unit)
    "INSERT INTO caqti_test_genre VALUES (NULL, NULL)"

  let unique_violation_req =
    Caqti_template.Create.(unit ->. unit)
    "INSERT INTO caqti_test_genre VALUES (3, 'fiction')"

  let foreign_key_violation_req =
    Caqti_template.Create.(unit ->. unit)
    "INSERT INTO caqti_test_publication \
     VALUES ('Unclassified', 'N.N.', 0)"

  let check_violation_req =
    Caqti_template.Create.(unit ->. unit)
    "INSERT INTO caqti_test_genre VALUES (3, '')"

  (* Missing:
   *   - Restrict violation is only mapped for PostgreSQL, but even there it is
   *     likely unused (deleting a restricted FK casuse FK violation.
   *   - Exclusion violation is also only mapped for PostgreSQL. *)
  let check_restrict_violation_req =
    Caqti_template.Create.(unit ->. unit)
    "DELETE FROM caqti_test_genre WHERE id = 2"

  (* TODO: exclusion *)

  let harness (module Db : CONNECTION) =
    List_result_fiber.iter_s (fun req -> Db.exec req ()) create_reqs
      >>= or_fail

  let make_test_case (expected_cause, req) =
    let test (module Db : CONNECTION) =
      Db.exec req () >|= function
       | Ok () -> Alcotest.fail "Error not reported."
       | Error (`Request_failed _ | `Response_failed _ as err) ->
          let actual_cause = Caqti_error.cause err in
          Alcotest.(check string) "cause"
            (Caqti_error.show_cause expected_cause)
            (Caqti_error.show_cause actual_cause)
       | Error err ->
          Alcotest.failf "Unexpected error: %a" Caqti_error.pp err
    in
    (Caqti_error.show_cause expected_cause, `Quick, test)

  let test_cases =
    ("harness", `Quick, harness) ::
    List.map make_test_case [
      `Not_null_violation, not_null_violation_req;
      `Unique_violation, unique_violation_req;
      `Foreign_key_violation, foreign_key_violation_req;
      `Check_violation, check_violation_req;
    ]
end
