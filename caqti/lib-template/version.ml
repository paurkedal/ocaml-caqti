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

type t = string

let is_known v = v <> ""

let rec skip_zeros_from s i =
  if i = String.length s || s.[i] <> '0' then i else
  skip_zeros_from s (i + 1)

let rec skip_digits_from s i =
  if i = String.length s then i else
  (match s.[i] with
   | '0'..'9' -> skip_digits_from s (i + 1)
   | _ -> i)

let rec compare_with_empty v i j =
  if i = j then 0 else
  (match v.[i] with
   | '~' -> -1
   | '0' | '.' | '-' -> compare_with_empty v (i + 1) j
   | _ -> 1)

let compare_char chL chR =
  if chL = chR then 0 else
  if chL = '~' then -1 else
  if chR = '~' then +1 else
  Char.compare chL chR

let skip_group s i =
  (match String.index_from_opt s i '-' with
   | None -> String.length s
   | Some j -> j)

let compare vL vR =
  let nL, nR = String.length vL, String.length vR in
  let rec start iL iR =
    if iL = nL && iR = nR then 0 else
    if iL = nL then - compare_with_empty vR iR nR else
    if iR = nR then + compare_with_empty vL iL nL else
    (match vL.[iL], vR.[iR] with
     | '.', '.' -> start (iL + 1) (iR + 1)
     | '-', '-' -> start (iL + 1) (iR + 1)
     | '.', '-' ->
        let jL = skip_group vL (iL + 1) in
        let c = compare_with_empty vL (iL + 1) jL in
        if c <> 0 then +c else start jL iR
     | '-', '.' ->
        let jR = skip_group vR (iR + 1) in
        let c = compare_with_empty vR (iR + 1) jR in
        if c <> 0 then -c else start iL jR
     | '0'..'9', '0'..'9' ->
        let iL = skip_zeros_from vL iL in
        let iR = skip_zeros_from vR iR in
        let kL = skip_digits_from vL iL in
        let kR = skip_digits_from vR iR in
        if kL - iL < kR - iR then -1 else
        if kL - iL > kR - iR then +1 else
        let c = digits iL iR (kL - iL) in
        if c < 0 then -1 else
        if c > 0 then +1 else
        start kL kR
     | chL, chR ->
        let c = compare_char chL chR in
        if c < 0 then -1 else
        if c > 0 then +1 else
        start (iL + 1) (iR + 1))
  and digits iL iR n =
    if n = 0 then start iL iR else
    let c = Char.compare vL.[iL] vR.[iR] in
    if c < 0 then -1 else
    if c > 0 then +1 else
    digits (iL + 1) (iR + 1) (n - 1)
  in
  start 0 0

let equal vL vR = compare vL vR = 0

let pp ppf version =
  Format.pp_print_string ppf (if is_known version then version else "[unknown]")

let of_string_unsafe version = version

module Infix = struct
  let ( =* ) v1 v2 = compare v1 v2 = 0
  let ( <>* ) v1 v2 = compare v1 v2 <> 0
  let ( <* ) v1 v2 = compare v1 v2 < 0
  let ( <=* ) v1 v2 = compare v1 v2 <= 0
  let ( >* ) v1 v2 = compare v1 v2 > 0
  let ( >=* ) v1 v2 = compare v1 v2 >= 0
end
