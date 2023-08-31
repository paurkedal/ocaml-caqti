(* Copyright (C) 2017--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

type (_, _) eq = Equal : ('a, 'a) eq (* OCaml 5.1 *)

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

  let unify : type a b. a t -> b t -> (a, b) eq option =
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
end

type _ record_serial = ..

type 'a product_id = {
  serial: 'a record_serial;
  is_serial: 'b. 'b record_serial -> ('a, 'b) eq option;
}

let make_id (type a) () : a product_id =
  let module M = struct
    type _ record_serial += Serial : a record_serial
  end in
  let is_serial : type b. b record_serial -> (a, b) eq option = function
   | M.Serial -> Some Equal
   | _ -> None
  in
  {serial = M.Serial; is_serial}

let unify_id {is_serial; _} {serial; _} = is_serial serial

type _ t =
  | Field : 'a Field.t -> 'a t
  | Option : 'a t -> 'a option t
  | Product : 'a product_id * 'i * ('a, 'i) product -> 'a t
  | Annot : [`Redacted] * 'a t -> 'a t
and (_, _) product =
  | Proj_end : ('a, 'a) product
  | Proj : 'b t * ('a -> 'b) * ('a, 'i) product -> ('a, 'b -> 'i) product

type any = Any : 'a t -> any

let rec unify : type a b. a t -> b t -> (a, b) eq option =
  fun t1 t2 ->
  (match t1, t2 with
   | Field ft1, Field ft2 -> Field.unify ft1 ft2
   | Field _, _ | _, Field _ -> None
   | Option t1, Option t2 ->
      (match unify t1 t2 with None -> None | Some Equal -> Some Equal)
   | Option _, _ | _, Option _ -> None
   | Product (id1, _, _), Product (id2, _, _) -> unify_id id1 id2
   | Product _, _ | _, Product _ -> None
   | Annot (`Redacted, t1), Annot (`Redacted, t2) -> unify t1 t2)

let equal_option f x y =
  (match x, y with
   | None, None -> true
   | Some x, Some y -> f x y
   | None, Some _ | Some _, None -> false)
let rec equal_value : type a. a t -> a -> a -> bool =
  (function
   | Field ft -> Field.equal_value ft
   | Option t -> equal_option (equal_value t)
   | Product (_, _, prod) -> equal_value_prod prod
   | Annot (_, t) -> equal_value t)
and equal_value_prod : type a i. (a, i) product -> a -> a -> bool =
  (function
   | Proj_end -> fun _ _ -> true
   | Proj (t, p, prod) ->
      let eq_first = equal_value t in
      let eq_rest = equal_value_prod prod in
      fun x y -> eq_first (p x) (p y) && eq_rest x y)

let rec length : type a. a t -> int = function
 | Field _ -> 1
 | Option t -> length t
 | Product (_, _, prod) ->
    let rec loop : type a i. (a, i) product -> _ -> _ = function
     | Proj_end -> Fun.id
     | Proj (t, _, prod) -> fun n -> loop prod (n + length t)
    in
    loop prod 0
 | Annot (_, t) -> length t

let rec pp_at : type a. int -> Format.formatter -> a t -> unit =
    fun prec ppf -> function
 | Field ft -> Format.pp_print_string ppf (Field.to_string ft)
 | Option t -> pp_at 1 ppf t; Format.pp_print_string ppf " option"
 | Product (_, _, Proj_end) -> Format.pp_print_string ppf "unit"
 | Product (_, _, Proj (t0, _, prod)) ->
    if prec > 0 then Format.pp_print_char ppf '(';
    let rec loop : type a i. (a, i) product -> _ = function
     | Proj_end -> ()
     | Proj (t, _, prod) ->
        Format.pp_print_string ppf " × ";
        pp_at 1 ppf t;
        loop prod
    in
    pp_at 1 ppf t0;
    loop prod;
    if prec > 0 then Format.pp_print_char ppf ')'
 | Annot (`Redacted, t) ->
    pp_at 1 ppf t;
    Format.pp_print_string ppf " redacted"

let pp ppf = pp_at 0 ppf
let pp_any ppf (Any t) = pp_at 0 ppf t

let rec pp_value : type a. _ -> a t * a -> unit = fun ppf -> function
 | Field ft, fv -> Field.pp_value ppf (ft, fv)
 | Option _, None -> Format.pp_print_string ppf "None"
 | Option t, Some x ->
    Format.pp_print_string ppf "Some ";
    pp_value ppf (t, x)
 | Product (_, _, prod), x ->
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
  let option t = Option t

  let product intro prod = Product (make_id (), intro, prod)
  let proj t p prod = Proj (t, p, prod)
  let proj_end = Proj_end

  let unit = product () proj_end

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
