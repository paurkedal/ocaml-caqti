(* Copyright (C) 2024--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

[@@@alert "-caqti_private"]

open Caqti_template

module A = struct
  include Alcotest.V1

  let version = testable Version.pp Version.equal
end

let versions = List.map (List.map Version.of_string_unsafe) [
  ["~beta"];
  ["0~beta"; "00~beta"];
  [""; "0"; "0.0"; "0.0.0"; "0.000.0"; "000.00.0"];
  ["0.1~"];
  ["0.1.0~"];
  ["0.1"; "0.1.0"; "000.1.000"];
  ["0.1.0r1"; "00.001.0r1"; "000.1.000r1"];
  ["0.1r1"];
  ["0.2~"];
  ["0.2.0.000"];
  ["0.2-2"];
  ["0.2.1"];
  ["25.100"; "025.0100"];
]

let test_equal () =
  let equal_to v0 v = A.(check version) "same version" v0 v in
  let all_equal = function
   | [] -> assert false
   | v0 :: vs -> List.iter (equal_to v0) vs
  in
  List.iter all_equal versions

let test_compare () =
  let versions = List.map List.hd versions in
  A.(check (list version)) "order" versions (List.sort Version.compare versions)

let test_cases = [
  A.test_case "equal" `Quick test_equal;
  A.test_case "compare" `Quick test_compare;
]
