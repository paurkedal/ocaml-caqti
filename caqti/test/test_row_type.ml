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

open Caqti_template

let random_field_type () =
  let open Field_type in
  (match Random.int 12 with
   | 0 -> Any Bool
   | 1 -> Any Int
   | 2 -> Any Int16
   | 3 -> Any Int32
   | 4 -> Any Int64
   | 5 -> Any Float
   | 6 -> Any String
   | 7 -> Any Octets
   | 8 -> Any Pdate
   | 9 -> Any Ptime
   | 10 -> Any Ptime_span
   | 11 -> Any (Enum "erated")
   | _ -> assert false)

let field_testable =
  let pp ppf (Field_type.Any ft) = Field_type.pp ppf ft in
  let eq (Field_type.Any ft) (Field_type.Any ft') =
    Field_type.unify ft ft' <> None
  in
  Alcotest.testable pp eq

let rec random_row_type_using fields =
  let n = Array.length fields in
  if n = 0 then Row_type.Any Row_type.unit else
  if n = 1 then
    let Field_type.Any ft = fields.(0) in
    Row_type.Any (Row_type.field ft)
  else
  (match Random.int (min n 3) with
   | 0 ->
      let Row_type.Any t = random_row_type_using fields in
      if Random.bool () then
        Row_type.Any (Row_type.option t)
      else
        Row_type.Any (Row_type.redacted t)
   | 1 ->
      let i = Random.int (n + 1) in
      let Any t0 = random_row_type_using (Array.sub fields 0 i) in
      let Any t1 = random_row_type_using (Array.sub fields i (n - i)) in
      Row_type.Any (Row_type.t2 t0 t1)
   | 2 ->
      let i, j = Random.int (n + 1), Random.int (n + 1) in
      let i, j = min i j, max i j in
      let Any t0 = random_row_type_using (Array.sub fields 0 i) in
      let Any t1 = random_row_type_using (Array.sub fields i (j - i)) in
      let Any t2 = random_row_type_using (Array.sub fields j (n - j)) in
      Row_type.Any (Row_type.t3 t0 t1 t2)
   | _ -> assert false)

let test_row_type_fields () =
  let fields = Array.init 10 (fun _ -> random_field_type ()) in
  let Any t = random_row_type_using fields in
  let fields' = Row_type.fields t |> Array.of_seq in
  Alcotest.(check (array field_testable)) "same field type sequence"
    fields fields'

let test_cases = [
  "fields", `Quick, test_row_type_fields;
]
