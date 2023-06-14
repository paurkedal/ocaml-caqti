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

type _ field = ..

type _ field +=
  | Bool : bool field
  | Int : int field
  | Int16 : int field
  | Int32 : int32 field
  | Int64 : int64 field
  | Float : float field
  | String : string field
  | Octets : string field
  | Pdate : Ptime.t field
  | Ptime : Ptime.t field
  | Ptime_span : Ptime.span field
  | Enum : string -> string field

module Field = struct

  type 'a t = 'a field = ..

  type _ coding = Coding : {
    rep: 'b t;
    encode: 'a -> ('b, string) result;
    decode: 'b -> ('a, string) result;
  } -> 'a coding

  type get_coding = {get_coding: 'a. Caqti_driver_info.t -> 'a t -> 'a coding}

  let coding_ht : (extension_constructor, get_coding) Hashtbl.t =
    Hashtbl.create 11

  let define_coding ft get =
    let ec = Obj.Extension_constructor.of_val ft in
    Hashtbl.add coding_ht ec get

  let coding di ft =
    let ec = Obj.Extension_constructor.of_val ft in
    try Some ((Hashtbl.find coding_ht ec).get_coding di ft)
    with Not_found -> None

  let to_string : type a. a field -> string = function
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
   | ft -> Obj.Extension_constructor.name (Obj.Extension_constructor.of_val ft)

  let pp ppf ft = Format.pp_print_string ppf (to_string ft)

  let pp_ptime = Ptime.pp_rfc3339 ~tz_offset_s:0 ~space:false ()

  let rec pp_value : type a. _ -> a field * a -> unit = fun ppf -> function
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
   | (ft, x) ->
      (match coding Caqti_driver_info.dummy ft with
       | Some (Coding {rep; encode; _}) ->
          (match encode x with
           | Ok y -> pp_value ppf (rep, y)
           | Error _ ->
              Format.fprintf ppf "<%s,invalid>"
                (Obj.Extension_constructor.name
                  (Obj.Extension_constructor.of_val ft)))
       | None ->
          Format.fprintf ppf "<%s>"
            (Obj.Extension_constructor.name
              (Obj.Extension_constructor.of_val ft)))
end

type _ t =
  | Unit : unit t
  | Field : 'a field -> 'a t
  | Option : 'a t -> 'a option t
  | Product : 'i * ('a, 'i) product -> 'a t
  | Annot : [`Redacted] * 'a t -> 'a t
and (_, _) product =
  | [] : ('a, 'a) product
  | (::) : ('b t * ('a -> 'b)) * ('a, 'i) product -> ('a, 'b -> 'i) product

type any = Any : 'a t -> any

let rec length : type a. a t -> int = function
 | Unit -> 0
 | Field _ -> 1
 | Option t -> length t
 | Product (_, prod) ->
    let rec loop : type a i. (a, i) product -> _ -> _ = function
     | [] -> Fun.id
     | (t, _) :: prod -> fun n -> loop prod (n + length t)
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
     | [] -> ()
     | (t, _) :: prod ->
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
     | [] -> ()
     | (t, p) :: prod ->
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

  let t2 t1 t2 =
    let intro x1 x2 = (x1, x2) in
    Product (intro, [
      t1, fst;
      t2, snd;
    ])

  let t3 t1 t2 t3 =
    let intro x1 x2 x3 = (x1, x2, x3) in
    Product (intro, [
      t1, (fun (x, _, _) -> x);
      t2, (fun (_, x, _) -> x);
      t3, (fun (_, _, x) -> x);
    ])

  let t4 t1 t2 t3 t4 =
    let intro x1 x2 x3 x4 = (x1, x2, x3, x4) in
    Product (intro, [
      t1, (fun (x, _, _, _) -> x);
      t2, (fun (_, x, _, _) -> x);
      t3, (fun (_, _, x, _) -> x);
      t4, (fun (_, _, _, x) -> x);
    ])

  let t5 t1 t2 t3 t4 t5 =
    let intro x1 x2 x3 x4 x5 = (x1, x2, x3, x4, x5) in
    Product (intro, [
      t1, (fun (x, _, _, _, _) -> x);
      t2, (fun (_, x, _, _, _) -> x);
      t3, (fun (_, _, x, _, _) -> x);
      t4, (fun (_, _, _, x, _) -> x);
      t5, (fun (_, _, _, _, x) -> x);
    ])

  let t6 t1 t2 t3 t4 t5 t6 =
    let intro x1 x2 x3 x4 x5 x6 = (x1, x2, x3, x4, x5, x6) in
    Product (intro, [
      t1, (fun (x, _, _, _, _, _) -> x);
      t2, (fun (_, x, _, _, _, _) -> x);
      t3, (fun (_, _, x, _, _, _) -> x);
      t4, (fun (_, _, _, x, _, _) -> x);
      t5, (fun (_, _, _, _, x, _) -> x);
      t6, (fun (_, _, _, _, _, x) -> x);
    ])

  let t7 t1 t2 t3 t4 t5 t6 t7 =
    let intro x1 x2 x3 x4 x5 x6 x7 = (x1, x2, x3, x4, x5, x6, x7) in
    Product (intro, [
      t1, (fun (x, _, _, _, _, _, _) -> x);
      t2, (fun (_, x, _, _, _, _, _) -> x);
      t3, (fun (_, _, x, _, _, _, _) -> x);
      t4, (fun (_, _, _, x, _, _, _) -> x);
      t5, (fun (_, _, _, _, x, _, _) -> x);
      t6, (fun (_, _, _, _, _, x, _) -> x);
      t7, (fun (_, _, _, _, _, _, x) -> x);
    ])

  let t8 t1 t2 t3 t4 t5 t6 t7 t8 =
    let intro x1 x2 x3 x4 x5 x6 x7 x8 = (x1, x2, x3, x4, x5, x6, x7, x8) in
    Product (intro, [
      t1, (fun (x, _, _, _, _, _, _, _) -> x);
      t2, (fun (_, x, _, _, _, _, _, _) -> x);
      t3, (fun (_, _, x, _, _, _, _, _) -> x);
      t4, (fun (_, _, _, x, _, _, _, _) -> x);
      t5, (fun (_, _, _, _, x, _, _, _) -> x);
      t6, (fun (_, _, _, _, _, x, _, _) -> x);
      t7, (fun (_, _, _, _, _, _, x, _) -> x);
      t8, (fun (_, _, _, _, _, _, _, x) -> x);
    ])

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
    Product (decode', [rep, encode'])

  let redacted t = Annot (`Redacted, t)

  let enum ~encode ~decode name =
    let decode' y =
      (match decode y with
       | Ok x -> x
       | Error msg -> raise (Reject msg))
    in
    Product (decode', [Field (Enum name), encode])

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
