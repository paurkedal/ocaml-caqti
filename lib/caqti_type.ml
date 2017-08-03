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

module type ISO = sig
  type t
  type u

  val f : t -> u
  val g : u -> t
end

type _ t =
 | Unit : unit t
 | Bool : bool t
 | Int : int t
 | Int32 : int32 t
 | Int64 : int64 t
 | Float : float t
 | String : string t
 | Pdate : int t
 | Ptime : Ptime.t t
 | Option : 'a t -> 'a option t
 | [] : unit Caqti_tuple.t t
 | (::) : 'a t * 'b Caqti_tuple.t t -> ('a * 'b) Caqti_tuple.t t
 | Iso : (module ISO with type t = 'a and type u = 'b) * 'b t -> 'a t

let rec length : type a. a t -> int = function
 | Unit -> 0
 | Bool -> 1
 | Int -> 1
 | Int32 -> 1
 | Int64 -> 1
 | Float -> 1
 | String -> 1
 | Pdate -> 1
 | Ptime -> 1
 | Option t -> length t
 | [] -> 0
 | (t :: ts) -> length t + length ts
 | Iso (_, t) -> length t

let rec pp_hum_at
  : type a. int -> Format.formatter -> a t -> unit
  = fun prec ppf -> function
 | Unit -> Format.pp_print_string ppf "unit"
 | Bool -> Format.pp_print_string ppf "bool"
 | Int -> Format.pp_print_string ppf "int"
 | Int32 -> Format.pp_print_string ppf "int32"
 | Int64 -> Format.pp_print_string ppf "int64"
 | Float -> Format.pp_print_string ppf "float"
 | String -> Format.pp_print_string ppf "string"
 | Pdate -> Format.pp_print_string ppf "pdate"
 | Ptime -> Format.pp_print_string ppf "ptime"
 | Option t -> pp_hum_at 1 ppf t; Format.pp_print_string ppf " option"
 | [] -> Format.pp_print_string ppf "[]"
 | (t :: ts) ->
    if prec > 0 then Format.pp_print_char ppf '[';
    pp_hum_at 1 ppf t;
    Format.pp_print_string ppf ", ";
    pp_hum_at 0 ppf ts;
    if prec > 0 then Format.pp_print_char ppf ']'
 | Iso (_, t) ->
    Format.pp_print_string ppf "</";
    pp_hum_at 0 ppf t;
    Format.pp_print_string ppf "/>"

let pp_hum ppf = pp_hum_at 1 ppf

let to_string_hum t =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  pp_hum ppf t;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
