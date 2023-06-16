(* Copyright (C) 2017--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

exception Reject of string

module Field = struct

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
end

type _ t =
  | Unit : unit t
  | Field : 'a Field.t -> 'a t
  | Option : 'a t -> 'a option t
  | Product : 'i * ('a, 'i) product -> 'a t
  | Annot : [`Redacted] * 'a t -> 'a t
and (_, _) product =
  | Proj_end : ('a, 'a) product
  | Proj : 'b t * ('a -> 'b) * ('a, 'i) product -> ('a, 'b -> 'i) product

type any = Any : 'a t -> any

let rec length : type a. a t -> int = function
 | Unit -> 0
 | Field _ -> 1
 | Option t -> length t
 | Product (_, prod) ->
    let rec loop : type a i. (a, i) product -> _ -> _ = function
     | Proj_end -> Fun.id
     | Proj (t, _, prod) -> fun n -> loop prod (n + length t)
    in
    loop prod 0
 | Annot (_, t) -> length t

let rec pp_at : type a. int -> Format.formatter -> a t -> unit =
    fun prec ppf -> function
 | Unit -> Format.pp_print_string ppf "unit"
 | Field ft -> Format.pp_print_string ppf (Field.to_string ft)
 | Option t -> pp_at 1 ppf t; Format.pp_print_string ppf " option"
 | Product (_, prod) ->
    if prec > 0 then Format.pp_print_char ppf '(';
    let rec loop : type a i. int -> (a, i) product -> _ = fun i -> function
     | Proj_end -> ()
     | Proj (t, _, prod) ->
        if i > 0 then Format.pp_print_string ppf " × ";
        pp_at 1 ppf t; loop (i + 1) prod
    in
    loop 0 prod;
    if prec > 0 then Format.pp_print_char ppf ')'
 | Annot (`Redacted, t) ->
    Format.pp_print_string ppf "redacted ";
    pp_at 2 ppf t

let pp ppf = pp_at 1 ppf
let pp_any ppf (Any t) = pp_at 1 ppf t

let rec pp_value : type a. _ -> a t * a -> unit = fun ppf -> function
 | Unit, () -> Format.pp_print_string ppf "()"
 | Field ft, fv -> Field.pp_value ppf (ft, fv)
 | Option _, None -> Format.pp_print_string ppf "None"
 | Option t, Some x ->
    Format.pp_print_string ppf "Some ";
    pp_value ppf (t, x)
 | Product (_, prod), x ->
    let rec loop : type i. int -> (a, i) product -> _ = fun i -> function
     | Proj_end -> ()
     | Proj (t, p, prod) ->
        if i > 0 then Format.pp_print_string ppf ", ";
        pp_value ppf (t, p x);
        loop (i + 1) prod
    in
    loop 0 prod
 | Annot (`Redacted, _), _ ->
    Format.pp_print_string ppf "#redacted#"

let show t =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf t;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let field ft = Field ft

module Std = struct
  let unit = Unit
  let option t = Option t

  let product intro prod = Product (intro, prod)
  let proj t p prod = Proj (t, p, prod)
  let proj_end = Proj_end

  let t2 t1 t2 =
    let intro x1 x2 = (x1, x2) in
    product intro
      @@ proj t1 fst
      @@ proj t2 snd
      @@ proj_end

  let t3 t1 t2 t3 =
    let intro x1 x2 x3 = (x1, x2, x3) in
    product intro
      @@ proj t1 (fun (x, _, _) -> x)
      @@ proj t2 (fun (_, x, _) -> x)
      @@ proj t3 (fun (_, _, x) -> x)
      @@ proj_end

  let t4 t1 t2 t3 t4 =
    let intro x1 x2 x3 x4 = (x1, x2, x3, x4) in
    product intro
      @@ proj t1 (fun (x, _, _, _) -> x)
      @@ proj t2 (fun (_, x, _, _) -> x)
      @@ proj t3 (fun (_, _, x, _) -> x)
      @@ proj t4 (fun (_, _, _, x) -> x)
      @@ proj_end

  let t5 t1 t2 t3 t4 t5 =
    let intro x1 x2 x3 x4 x5 = (x1, x2, x3, x4, x5) in
    product intro
      @@ proj t1 (fun (x, _, _, _, _) -> x)
      @@ proj t2 (fun (_, x, _, _, _) -> x)
      @@ proj t3 (fun (_, _, x, _, _) -> x)
      @@ proj t4 (fun (_, _, _, x, _) -> x)
      @@ proj t5 (fun (_, _, _, _, x) -> x)
      @@ proj_end

  let t6 t1 t2 t3 t4 t5 t6 =
    let intro x1 x2 x3 x4 x5 x6 = (x1, x2, x3, x4, x5, x6) in
    product intro
      @@ proj t1 (fun (x, _, _, _, _, _) -> x)
      @@ proj t2 (fun (_, x, _, _, _, _) -> x)
      @@ proj t3 (fun (_, _, x, _, _, _) -> x)
      @@ proj t4 (fun (_, _, _, x, _, _) -> x)
      @@ proj t5 (fun (_, _, _, _, x, _) -> x)
      @@ proj t6 (fun (_, _, _, _, _, x) -> x)
      @@ proj_end

  let t7 t1 t2 t3 t4 t5 t6 t7 =
    let intro x1 x2 x3 x4 x5 x6 x7 = (x1, x2, x3, x4, x5, x6, x7) in
    product intro
      @@ proj t1 (fun (x, _, _, _, _, _, _) -> x)
      @@ proj t2 (fun (_, x, _, _, _, _, _) -> x)
      @@ proj t3 (fun (_, _, x, _, _, _, _) -> x)
      @@ proj t4 (fun (_, _, _, x, _, _, _) -> x)
      @@ proj t5 (fun (_, _, _, _, x, _, _) -> x)
      @@ proj t6 (fun (_, _, _, _, _, x, _) -> x)
      @@ proj t7 (fun (_, _, _, _, _, _, x) -> x)
      @@ proj_end

  let t8 t1 t2 t3 t4 t5 t6 t7 t8 =
    let intro x1 x2 x3 x4 x5 x6 x7 x8 = (x1, x2, x3, x4, x5, x6, x7, x8) in
    product intro
      @@ proj t1 (fun (x, _, _, _, _, _, _, _) -> x)
      @@ proj t2 (fun (_, x, _, _, _, _, _, _) -> x)
      @@ proj t3 (fun (_, _, x, _, _, _, _, _) -> x)
      @@ proj t4 (fun (_, _, _, x, _, _, _, _) -> x)
      @@ proj t5 (fun (_, _, _, _, x, _, _, _) -> x)
      @@ proj t6 (fun (_, _, _, _, _, x, _, _) -> x)
      @@ proj t7 (fun (_, _, _, _, _, _, x, _) -> x)
      @@ proj t8 (fun (_, _, _, _, _, _, _, x) -> x)
      @@ proj_end

  let custom ~encode ~decode rep =
    let encode' x =
      (match encode x with
       | Ok y -> y
       | Error msg -> raise (Reject msg))
    in
    let decode' y =
      (match decode y with
       | Ok x -> x
       | Error msg -> raise (Reject msg))
    in
    product decode' @@ proj rep encode' @@ proj_end

  let redacted t = Annot (`Redacted, t)

  let enum ~encode ~decode name =
    let decode' y =
      (match decode y with
       | Ok x -> x
       | Error msg -> raise (Reject msg))
    in
    product decode' @@ proj (Field (Enum name)) encode @@ proj_end

  let bool = Field Bool
  let int = Field Int
  let int16 = Field Int16
  let int32 = Field Int32
  let int64 = Field Int64
  let float = Field Float
  let string = Field String
  let octets = Field Octets
  let pdate = Field Pdate
  let ptime = Field Ptime
  let ptime_span = Field Ptime_span

  (* deprecated *)
  let tup2 = t2
  let tup3 = t3
  let tup4 = t4
end
include Std
