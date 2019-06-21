(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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

type query =
  | L of string
  | P of int
  | S of query list

let rec pp_query ppf = function
 | L s -> Format.pp_print_string ppf s
 | P n -> Format.pp_print_char ppf '$'; Format.pp_print_int ppf (n + 1)
 | S qs -> List.iter (pp_query ppf) qs

let bprint_sql_escaped buf s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\'' -> Buffer.add_string buf "''"
    | c -> Buffer.add_char buf c
  done

let bprint_sql_quoted buf s =
  Buffer.add_char buf '\'';
  bprint_sql_escaped buf s;
  Buffer.add_char buf '\''

let sql_escaped s =
  let buf = Buffer.create (5 * String.length s / 4) in
  bprint_sql_escaped buf s;
  Buffer.contents buf

let sql_quoted s =
  let buf = Buffer.create (5 * String.length s / 4 + 2) in
  bprint_sql_quoted buf s;
  Buffer.contents buf
