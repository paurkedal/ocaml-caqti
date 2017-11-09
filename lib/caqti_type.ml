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
  | T2 : 'a0 t * 'a1 t -> ('a0 * 'a1) t
  | T3 : 'a0 t * 'a1 t * 'a2 t -> ('a0 * 'a1 * 'a2) t
  | T4 : 'a0 t * 'a1 t * 'a2 t * 'a3 t -> ('a0 * 'a1 * 'a2 * 'a3) t
  | Custom : {rep: 'b t; encode: 'a -> 'b; decode: 'b -> 'a} -> 'a t

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
 | T2 (t0, t1) -> length t0 + length t1
 | T3 (t0, t1, t2) -> length t0 + length t1 + length t2
 | T4 (t0, t1, t2, t3) -> length t0 + length t1 + length t2 + length t3
 | Custom {rep; _} -> length rep

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
 | T2 (t0, t1) ->
    if prec > 0 then Format.pp_print_char ppf '(';
    pp_hum_at 1 ppf t0;
    Format.pp_print_string ppf " × ";
    pp_hum_at 1 ppf t1;
    if prec > 0 then Format.pp_print_char ppf ')'
 | T3 (t0, t1, t2) ->
    if prec > 0 then Format.pp_print_char ppf '(';
    pp_hum_at 1 ppf t0;
    Format.pp_print_string ppf " × ";
    pp_hum_at 1 ppf t1;
    Format.pp_print_string ppf " × ";
    pp_hum_at 1 ppf t2;
    if prec > 0 then Format.pp_print_char ppf ')'
 | T4 (t0, t1, t2, t3) ->
    if prec > 0 then Format.pp_print_char ppf '(';
    pp_hum_at 1 ppf t0;
    Format.pp_print_string ppf " × ";
    pp_hum_at 1 ppf t1;
    Format.pp_print_string ppf " × ";
    pp_hum_at 1 ppf t2;
    Format.pp_print_string ppf " × ";
    pp_hum_at 1 ppf t3;
    if prec > 0 then Format.pp_print_char ppf ')'
 | Custom {rep; _} ->
    Format.pp_print_string ppf "</";
    pp_hum_at 0 ppf rep;
    Format.pp_print_string ppf "/>"

let pp_hum ppf = pp_hum_at 1 ppf

let to_string_hum t =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  pp_hum ppf t;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
