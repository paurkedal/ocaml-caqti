(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

type typedesc =
  [ `Bool
  | `Int
  | `Float
  | `String
  | `Bytes
  | `Date
  | `Utc
  | `Other of string
  | `Unknown ]

type querydesc = {
  querydesc_params : typedesc array;
  querydesc_fields : (string * typedesc) array;
}

let string_of_typedesc = function
  | `Bool -> "bool"
  | `Int -> "int"
  | `Float -> "float"
  | `String -> "string"
  | `Bytes -> "bytes"
  | `Date -> "date"
  | `Utc -> "utc"
  | `Other s -> s
  | `Unknown -> "unknown"

let string_of_querydesc qd =
  let buf = Buffer.create 256 in
  if Array.length qd.querydesc_params = 0 then
    Buffer.add_string buf "unit"
  else
    Array.iteri
      (fun i td ->
        if i <> 0 then Buffer.add_string buf " * ";
        Buffer.add_string buf (string_of_typedesc td))
      qd.querydesc_params;
  Buffer.add_string buf " -> {";
  Array.iteri
    (fun i (name, td) ->
      if i <> 0 then Buffer.add_string buf "; ";
      Buffer.add_string buf name;
      Buffer.add_string buf ": ";
      Buffer.add_string buf (string_of_typedesc td))
    qd.querydesc_fields;
  Buffer.add_char buf '}';
  Buffer.contents buf
