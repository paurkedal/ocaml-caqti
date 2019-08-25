(* Copyright (C) 2019  Petter A. Urkedal <paurkedal@gmail.com>
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

type t =
  | L of string
  | P of int
  | S of t list
[@@deriving eq]

let normal =
  let rec collect acc = function
   | [] -> List.rev acc
   | ((L"" | S[]) :: qs) -> collect acc qs
   | (P i :: qs) -> collect (P i :: acc) qs
   | (S (q' :: qs') :: qs) -> collect acc (q' :: S qs' :: qs)
   | (L s :: qs) -> collectL acc [s] qs
  and collectL acc accL = function
   | ((L"" | S[]) :: qs) -> collectL acc accL qs
   | (L s :: qs) -> collectL acc (s :: accL) qs
   | (S (q' :: qs') :: qs) -> collectL acc accL (q' :: S qs' :: qs)
   | [] | (P _ :: _) as qs ->
      collect (L (String.concat "" (List.rev accL)) :: acc) qs
  in
  fun q ->
    (match collect [] [q] with
     | [] -> S[]
     | [q] -> q
     | qs -> S qs)

let hash = Hashtbl.hash

let rec pp ppf = function
 | L s -> Format.pp_print_string ppf s
 | P n -> Format.pp_print_char ppf '$'; Format.pp_print_int ppf (n + 1)
 | S qs -> List.iter (pp ppf) qs

let show q =
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf q; Format.pp_print_flush ppf ();
  Buffer.contents buf

let concat =
  let rec loop pfx acc = function
   | [] -> acc
   | q :: qs -> loop pfx (pfx :: q :: acc) qs
  in
  fun sep -> function
   | [] -> S[]
   | q :: qs -> S (q :: loop (L sep) [] (List.rev qs))
