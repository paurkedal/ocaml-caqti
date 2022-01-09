(* Copyright (C) 2017--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

let no_env _ = raise Not_found

let linear_param_length ?(env = no_env) templ =
  let rec loop = function
   | Caqti_query.L _ -> ident
   | Caqti_query.Q _ -> succ
   | Caqti_query.P _ -> succ
   | Caqti_query.E v -> loop (env v)
   | Caqti_query.S frags -> List.fold loop frags
  in
  loop templ 0

let nonlinear_param_length ?(env = no_env) templ =
  let rec loop = function
   | Caqti_query.L _ -> ident
   | Caqti_query.Q _ -> ident
   | Caqti_query.P n -> max (n + 1)
   | Caqti_query.E v -> loop (env v)
   | Caqti_query.S frags -> List.fold loop frags
  in
  loop templ 0

let linear_param_order ?(env = no_env) templ =
  let a = Array.make (nonlinear_param_length templ) [] in
  let rec loop = function
   | Caqti_query.L _ -> ident
   | Caqti_query.Q s -> fun (j, quotes) -> (j + 1, (j, s) :: quotes)
   | Caqti_query.P i -> fun (j, quotes) -> a.(i) <- j :: a.(i); (j + 1, quotes)
   | Caqti_query.E v -> loop (env v)
   | Caqti_query.S frags -> List.fold loop frags
  in
  let _, quotes = loop templ (0, []) in
  (Array.to_list a, List.rev quotes)

let linear_query_string ?(env = no_env) templ =
  let buf = Buffer.create 64 in
  let rec loop = function
   | Caqti_query.L s -> Buffer.add_string buf s
   | Caqti_query.Q _ | Caqti_query.P _ -> Buffer.add_char buf '?'
   | Caqti_query.E v -> loop (env v)
   | Caqti_query.S frags -> List.iter loop frags
  in
  loop templ;
  Buffer.contents buf
