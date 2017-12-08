(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_prereq

let linear_param_length templ =
  let rec loop = function
   | Caqti_request.L _ -> ident
   | Caqti_request.P _ -> succ
   | Caqti_request.S frags -> List.fold loop frags in
  loop templ 0

let nonlinear_param_length templ =
  let rec loop = function
   | Caqti_request.L _ -> ident
   | Caqti_request.P n -> max (n + 1)
   | Caqti_request.S frags -> List.fold loop frags in
  loop templ 0

let linear_param_order templ =
  let a = Array.make (nonlinear_param_length templ) [] in
  let rec loop = function
   | Caqti_request.L _ -> fun j -> j
   | Caqti_request.P i -> fun j -> a.(i) <- j :: a.(i); j + 1
   | Caqti_request.S frags -> List.fold loop frags in
  let _ = loop templ 0 in
  Array.to_list a

let linear_query_string templ =
  let buf = Buffer.create 64 in
  let rec loop = function
   | Caqti_request.L s -> Buffer.add_string buf s
   | Caqti_request.P _ -> Buffer.add_char buf '?'
   | Caqti_request.S frags -> List.iter loop frags in
  loop templ;
  Buffer.contents buf
