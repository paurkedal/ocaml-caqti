(* Copyright (C) 2019--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_common_priv
open Caqti_compat [@@warning "-33"]

type t =
  | L of string
  | Q of string
  | P of int
  | S of t list

let normal =
  let rec collect acc = function
   | [] -> List.rev acc
   | ((L"" | S[]) :: qs) -> collect acc qs
   | ((P _ | Q _ as q) :: qs) -> collect (q :: acc) qs
   | (S (q' :: qs') :: qs) -> collect acc (q' :: S qs' :: qs)
   | (L s :: qs) -> collectL acc [s] qs
  and collectL acc accL = function
   | ((L"" | S[]) :: qs) -> collectL acc accL qs
   | (L s :: qs) -> collectL acc (s :: accL) qs
   | (S (q' :: qs') :: qs) -> collectL acc accL (q' :: S qs' :: qs)
   | [] | ((P _ | Q _) :: _) as qs ->
      collect (L (String.concat "" (List.rev accL)) :: acc) qs
  in
  fun q ->
    (match collect [] [q] with
     | [] -> S[]
     | [q] -> q
     | qs -> S qs)

let rec equal t1 t2 =
  match t1, t2 with
  | L s1, L s2 -> String.equal s1 s2
  | Q s1, Q s2 -> String.equal s1 s2
  | P i1, P i2 -> Int.equal i1 i2
  | S ts1, S ts2 -> List.equal equal ts1 ts2
  | L _, _ -> false
  | Q _, _ -> false
  | P _, _ -> false
  | S _, _ -> false

let hash = Hashtbl.hash

let rec pp ppf = function
 | L s -> Format.pp_print_string ppf s
 | Q s ->
    (* Using non-SQL quoting, to avoid issues with newlines and other control
     * characters when printing to log files. *)
    Format.pp_print_string ppf "E'";
    for i = 0 to String.length s - 1 do
      (match s.[i] with
       | '\\' -> Format.pp_print_string ppf {|\\|}
       | '\'' -> Format.pp_print_string ppf {|\'|}
       | '\t' -> Format.pp_print_string ppf {|\t|}
       | '\n' -> Format.pp_print_string ppf {|\n|}
       | '\r' -> Format.pp_print_string ppf {|\r|}
       | '\x00'..'\x1f' as c -> Format.fprintf ppf {|\x%02x|} (Char.code c)
       | _ -> Format.pp_print_char ppf s.[i])
    done;
    Format.pp_print_char ppf '\''
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
