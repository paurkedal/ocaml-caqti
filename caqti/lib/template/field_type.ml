(* Copyright (C) 2024--2026  Petter A. Urkedal <paurkedal@gmail.com>
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

open Shims

type 'a t =
  | Bool : bool t
  | Int : int t
  | Int16 : int t
  | Int32 : int32 t
  | Int64 : int64 t
  | Float : float t
  | String : string t
  | Octets : string t
  | Pdate : Ptime.t t
  | Ptime : Ptime.t t
  | Ptime_span : Ptime.span t
  | Enum : string -> string t

type any = Any : 'a t -> any

let unify : type a b. a t -> b t -> (a, b) Type.eq option =
  fun ft1 ft2 ->
  (match ft1, ft2 with
   | Bool, Bool -> Some Equal
   | Bool, _ | _, Bool -> None
   | Int, Int -> Some Equal
   | Int, _ | _, Int -> None
   | Int16, Int16 -> Some Equal
   | Int16, _ | _, Int16 -> None
   | Int32, Int32 -> Some Equal
   | Int32, _ | _, Int32 -> None
   | Int64, Int64 -> Some Equal
   | Int64, _ | _, Int64 -> None
   | Float, Float -> Some Equal
   | Float, _ | _, Float -> None
   | String, String -> Some Equal
   | String, _ | _, String -> None
   | Octets, Octets -> Some Equal
   | Octets, _ | _, Octets -> None
   | Pdate, Pdate -> Some Equal
   | Pdate, _ | _, Pdate -> None
   | Ptime, Ptime -> Some Equal
   | Ptime, _ | _, Ptime -> None
   | Ptime_span, Ptime_span -> Some Equal
   | Ptime_span, _ | _, Ptime_span -> None
   | Enum name1, Enum name2 when name1 = name2 -> Some Equal
   | Enum _, Enum _ -> None)

let equal_value : type a. a t -> a -> a -> bool = function
 | Bool -> Bool.equal
 | Int -> Int.equal
 | Int16 -> Int.equal
 | Int32 -> Int32.equal
 | Int64 -> Int64.equal
 | Float -> Float.equal
 | String -> String.equal
 | Octets -> String.equal
 | Pdate -> Ptime.equal
 | Ptime -> Ptime.equal
 | Ptime_span -> Ptime.Span.equal
 | Enum _ -> String.equal

let to_string : type a. a t -> string = function
 | Bool -> "bool"
 | Int -> "int"
 | Int16 -> "int16"
 | Int32 -> "int32"
 | Int64 -> "int64"
 | Float -> "float"
 | String -> "string"
 | Octets -> "octets"
 | Pdate -> "pdate"
 | Ptime -> "ptime"
 | Ptime_span -> "ptime_span"
 | Enum name -> name

let pp ppf ft = Format.pp_print_string ppf (to_string ft)

let pp_ptime = Ptime.pp_rfc3339 ~tz_offset_s:0 ~space:false ()

let pp_value : type a. _ -> a t * a -> unit = fun ppf -> function
 | Bool, x -> Format.pp_print_bool ppf x
 | Int, x -> Format.pp_print_int ppf x
 | Int16, x -> Format.pp_print_int ppf x
 | Int32, x -> Format.fprintf ppf "%ldl" x
 | Int64, x -> Format.fprintf ppf "%LdL" x
 | Float, x -> Format.fprintf ppf "%F" x
 | String, x -> Format.fprintf ppf "%S" x
 | Octets, x -> Format.fprintf ppf "%S" x
 | Pdate, x ->
    let y, m, d = Ptime.to_date x in
    Format.fprintf ppf "%d-%02d-%02d" y m d
 | Ptime, x -> pp_ptime ppf x
 | Ptime_span, x -> Ptime.Span.pp ppf x
 | Enum _, x -> Format.pp_print_string ppf x
