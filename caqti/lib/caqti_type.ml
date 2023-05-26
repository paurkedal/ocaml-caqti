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
  | Tup2 : 'a0 t * 'a1 t -> ('a0 * 'a1) t
  | Tup3 : 'a0 t * 'a1 t * 'a2 t -> ('a0 * 'a1 * 'a2) t
  | Tup4 : 'a0 t * 'a1 t * 'a2 t * 'a3 t -> ('a0 * 'a1 * 'a2 * 'a3) t
  | Custom : {
      rep: 'b t;
      encode: 'a -> ('b, string) result;
      decode: 'b -> ('a, string) result;
    } -> 'a t
  | Annot : [`Redacted] * 'a t -> 'a t

type any = Any : 'a t -> any

let rec length : type a. a t -> int = function
 | Unit -> 0
 | Field _ -> 1
 | Option t -> length t
 | Tup2 (t0, t1) -> length t0 + length t1
 | Tup3 (t0, t1, t2) -> length t0 + length t1 + length t2
 | Tup4 (t0, t1, t2, t3) -> length t0 + length t1 + length t2 + length t3
 | Custom {rep; _} -> length rep
 | Annot (_, t) -> length t

let rec pp_at : type a. int -> Format.formatter -> a t -> unit =
    fun prec ppf -> function
 | Unit -> Format.pp_print_string ppf "unit"
 | Field ft -> Format.pp_print_string ppf (Field.to_string ft)
 | Option t -> pp_at 1 ppf t; Format.pp_print_string ppf " option"
 | Tup2 (t0, t1) ->
    if prec > 0 then Format.pp_print_char ppf '(';
    pp_at 1 ppf t0;
    Format.pp_print_string ppf " × ";
    pp_at 1 ppf t1;
    if prec > 0 then Format.pp_print_char ppf ')'
 | Tup3 (t0, t1, t2) ->
    if prec > 0 then Format.pp_print_char ppf '(';
    pp_at 1 ppf t0;
    Format.pp_print_string ppf " × ";
    pp_at 1 ppf t1;
    Format.pp_print_string ppf " × ";
    pp_at 1 ppf t2;
    if prec > 0 then Format.pp_print_char ppf ')'
 | Tup4 (t0, t1, t2, t3) ->
    if prec > 0 then Format.pp_print_char ppf '(';
    pp_at 1 ppf t0;
    Format.pp_print_string ppf " × ";
    pp_at 1 ppf t1;
    Format.pp_print_string ppf " × ";
    pp_at 1 ppf t2;
    Format.pp_print_string ppf " × ";
    pp_at 1 ppf t3;
    if prec > 0 then Format.pp_print_char ppf ')'
 | Custom {rep; _} ->
    Format.pp_print_string ppf "</";
    pp_at 0 ppf rep;
    Format.pp_print_string ppf "/>"
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
 | Tup2 (t1, t2), (x1, x2) ->
    Format.pp_print_char ppf '(';
    pp_value ppf (t1, x1);
    Format.pp_print_string ppf ", ";
    pp_value ppf (t2, x2);
    Format.pp_print_char ppf ')'
 | Tup3 (t1, t2, t3), (x1, x2, x3) ->
    Format.pp_print_char ppf '(';
    pp_value ppf (t1, x1);
    Format.pp_print_string ppf ", ";
    pp_value ppf (t2, x2);
    Format.pp_print_string ppf ", ";
    pp_value ppf (t3, x3);
    Format.pp_print_char ppf ')'
 | Tup4 (t1, t2, t3, t4), (x1, x2, x3, x4) ->
    Format.pp_print_char ppf '(';
    pp_value ppf (t1, x1);
    Format.pp_print_string ppf ", ";
    pp_value ppf (t2, x2);
    Format.pp_print_string ppf ", ";
    pp_value ppf (t3, x3);
    Format.pp_print_string ppf ", ";
    pp_value ppf (t4, x4);
    Format.pp_print_char ppf ')'
 | Custom {rep; encode; _}, x ->
    (match encode x with
     | Ok y -> pp_value ppf (rep, y)
     | Error _ -> Format.pp_print_string ppf "INVALID")
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

  let t2 t0 t1 = Tup2 (t0, t1)
  let t3 t0 t1 t2 = Tup3 (t0, t1, t2)
  let t4 t0 t1 t2 t3 = Tup4 (t0, t1, t2, t3)

  let t5 t0 t1 t2 t3 t4 =
    let rep = Tup4 (t0, t1, t2, Tup2 (t3, t4)) in
    let encode (x0, x1, x2, x3, x4) = Ok (x0, x1, x2, (x3, x4)) in
    let decode (x0, x1, x2, (x3, x4)) = Ok (x0, x1, x2, x3, x4) in
    Custom {rep; encode; decode}

  let t6 t0 t1 t2 t3 t4 t5 =
    let rep = Tup4 (t0, t1, t2, Tup3 (t3, t4, t5)) in
    let encode (x0, x1, x2, x3, x4, x5) = Ok (x0, x1, x2, (x3, x4, x5)) in
    let decode (x0, x1, x2, (x3, x4, x5)) = Ok (x0, x1, x2, x3, x4, x5) in
    Custom {rep; encode; decode}

  let t7 t0 t1 t2 t3 t4 t5 t6 =
    let rep = Tup4 (t0, t1, t2, Tup4 (t3, t4, t5, t6)) in
    let encode (x0, x1, x2, x3, x4, x5, x6) =
      Ok (x0, x1, x2, (x3, x4, x5, x6))
    in
    let decode (x0, x1, x2, (x3, x4, x5, x6)) =
      Ok (x0, x1, x2, x3, x4, x5, x6)
    in
    Custom {rep; encode; decode}

  let t8 t0 t1 t2 t3 t4 t5 t6 t7 =
    let rep = Tup2 (Tup4 (t0, t1, t2, t3), Tup4 (t4, t5, t6, t7)) in
    let encode (x0, x1, x2, x3, x4, x5, x6, x7) =
      Ok ((x0, x1, x2, x3), (x4, x5, x6, x7))
    in
    let decode ((x0, x1, x2, x3), (x4, x5, x6, x7)) =
      Ok (x0, x1, x2, x3, x4, x5, x6, x7)
    in
    Custom {rep; encode; decode}

  let custom ~encode ~decode rep = Custom {rep; encode; decode}
  let redacted t = Annot (`Redacted, t)
  let enum ~encode ~decode name =
    Custom {rep = Field (Enum name); encode = (fun x -> Ok (encode x)); decode}

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
