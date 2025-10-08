(* Copyright (C) 2017--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

type annot = [`Redacted] (* TODO: Consider open type. *)

module Private = struct
  type _ t =
    | Field : 'a Field_type.t -> 'a t
    | Option : 'a t -> 'a option t
    | Product : ('i, 'a) Constructor.t * ('i, 'a) product -> 'a t
    | Annot : annot * 'a t -> 'a t
  and (_, _) product =
    | Proj_end : ('a Constructor.return, 'a) product
    | Proj : 'b t * ('a -> 'b) * ('i, 'a) product -> ('b -> 'i, 'a) product
end
open Private

type 'a t = 'a Private.t
type ('i, 'a) product = ('i, 'a) Private.product

type any = Any : 'a t -> any

let rec unify : type a b. a t -> b t -> (a, b) Type.eq option =
  fun t1 t2 ->
  (match t1, t2 with
   | Field ft1, Field ft2 -> Field_type.unify ft1 ft2
   | Field _, _ | _, Field _ -> None
   | Option t1, Option t2 ->
      (match unify t1 t2 with
       | None -> None
       | Some Equal -> Some Equal)
   | Option _, _ | _, Option _ -> None
   | Product (name1, pt1), Product (name2, pt2) ->
      (match Constructor.unify name1 name2 with
       | Some dep -> unify_product pt1 pt2 dep
       | None -> None)
   | Product _, _ | _, Product _ -> None
   | Annot (`Redacted, t1), Annot (`Redacted, t2) -> unify t1 t2)
and unify_product
  : type i j a b. (i, a) product -> (j, b) product ->
    (i, j) Constructor.unifier -> (a, b) Type.eq option =
  fun pt1 pt2 deq ->
  (match pt1, pt2, deq with
   | Proj_end, Proj_end, Equal -> Some Type.Equal
   | Proj (t1, _, pt1), Proj (t2, _, pt2), Assume dep ->
      Option.bind (unify t1 t2) (fun p -> unify_product pt1 pt2 (dep p))
   | _ -> .)

(* length *)

let rec length : type a. a t -> int =
  (function
   | Field _ -> 1
   | Option t -> length t
   | Product (_, pt) -> length_product pt
   | Annot (_, t) -> length t)
and length_product : type a i. (i, a) product -> int =
  (function
   | Proj_end -> 0
   | Proj (t, _, pt) -> length t + length_product pt)

(* pp *)

let rec pp : type a. a t -> int -> Format.formatter -> unit -> unit =
  (function
   | Field ft ->
      let string_of_ft = Field_type.to_string ft in
      fun _ ppf () -> Format.pp_print_string ppf string_of_ft
   | Option t ->
      let pp_t = pp t 1 in
      fun _ ppf () -> Format.fprintf ppf "@[%a@ option@]" pp_t ()
   | Product (_, Proj_end) ->
      fun _ ppf () -> Format.pp_print_string ppf "unit"
   | Product (_, Proj (t0, _, pt)) ->
      let pp_t0 = pp t0 1 in
      let pp_pt = pp_product_tail pt in
      fun prec ->
        fun ppf () ->
          if prec > 0 then Format.pp_print_char ppf '(';
          pp_t0 ppf ();
          pp_pt ppf ();
          if prec > 0 then Format.pp_print_char ppf ')'
   | Annot (`Redacted, t) ->
      let pp_t = pp t 1 in
      fun _prec ppf () ->
        pp_t ppf ();
        Format.pp_print_string ppf " redacted")
and pp_product_tail
  : type a i. (i, a) product -> Format.formatter -> unit -> unit =
  (function
   | Proj_end -> fun _ () -> ()
   | Proj (t, _, pt) ->
      let pp_t = pp t 1 in
      let pp_pt = pp_product_tail pt in
      fun ppf () ->
        Format.pp_print_string ppf " × ";
        pp_t ppf ();
        pp_pt ppf ())

let pp ppf t = pp t 1 ppf ()
let pp_any ppf (Any t) = pp ppf t
let show t = Format.asprintf "%a" pp t

let field ft = Field ft

module type STD = sig
  val bool : bool t
  val int : int t
  val int16 : int t
  val int32 : int32 t
  val int64 : int64 t
  val float : float t
  val string : string t
  val octets : string t
  val pdate : Ptime.t t
  val ptime : Ptime.t t
  val ptime_span : Ptime.span t
  val option : 'a t -> 'a option t
  val redacted : 'a t -> 'a t
  val unit : unit t
  val t2 : 'a1 t -> 'a2 t -> ('a1 * 'a2) t
  val elim_t2 : ('a1 * 'a2) t -> ('a1 t * 'a2 t) option
  val t3 : 'a1 t -> 'a2 t -> 'a3 t -> ('a1 * 'a2 * 'a3) t
  val elim_t3 : ('a1 * 'a2 * 'a3) t -> ('a1 t * 'a2 t * 'a3 t) option
  val t4 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> ('a1 * 'a2 * 'a3 * 'a4) t
  val elim_t4 :
    ('a1 * 'a2 * 'a3 * 'a4) t -> ('a1 t * 'a2 t * 'a3 t * 'a4 t) option
  val t5 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5) t
  val elim_t5 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t) option
  val t6 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) t
  val elim_t6 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t) option
  val t7 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) t
  val elim_t7 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t) option
  val t8 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) t
  val elim_t8 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t * 'a8 t) option
  val t9 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t ->
    'a9 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) t
  val elim_t9 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t * 'a8 t * 'a9 t)
      option
  val t10 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t ->
    'a9 t -> 'a10 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) t
  val elim_t10 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t * 'a8 t * 'a9 t
      * 'a10 t) option
  val t11 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t ->
    'a9 t -> 'a10 t -> 'a11 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11) t
  val elim_t11 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t * 'a8 t * 'a9 t
      * 'a10 t * 'a11 t) option
  val t12 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t ->
    'a9 t -> 'a10 t -> 'a11 t -> 'a12 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12) t
  val elim_t12 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11
      * 'a12) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t * 'a8 t * 'a9 t
      * 'a10 t * 'a11 t * 'a12 t) option
end

let option t = Option t

let rec product_unifier
  : type a i. (i, a) product -> (i, i) Constructor.unifier =
  let open Constructor in
  (function
   | Proj_end -> Equal
   | Proj (_, _, prod) -> Assume (fun Type.Equal -> product_unifier prod))

exception Reject of string

let product : type i a. i -> (i, a) product -> a t =
  fun intro prod ->
    let open struct
      open Constructor
      type (_, _) tag += Tag : (i, a) tag
      let unify_tag : type j b. (j, b) tag -> (i, j) unifier option =
        (function
         | Tag -> Some (product_unifier prod)
         | _ -> None)
      let ctor = {tag = Tag; unify_tag; construct = intro}
    end in
    Product (ctor, prod)

let product' ctor prod = Product (ctor, prod)

let proj t p prod = Proj (t, p, prod)
let proj_end = Proj_end

let enum ~encode ~decode name =
  product decode
    @@ proj (Field (Enum name)) encode
    @@ proj_end

let unit = product (Ok ()) proj_end

type (_, _) Constructor.tag +=
  | T2 : (
      'a0 -> 'a1 -> ('a0 * 'a1) Constructor.return,
      'a0 * 'a1
    ) Constructor.tag
  | T3 : (
      'a0 -> 'a1 -> 'a2 -> ('a0 * 'a1 * 'a2) Constructor.return,
      'a0 * 'a1 * 'a2
    ) Constructor.tag
  | T4 : (
      'a0 -> 'a1 -> 'a2 -> 'a3 ->
      ('a0 * 'a1 * 'a2 * 'a3) Constructor.return,
      'a0 * 'a1 * 'a2 * 'a3
    ) Constructor.tag
  | T5 : (
      'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 ->
      ('a0 * 'a1 * 'a2 * 'a3 * 'a4) Constructor.return,
      'a0 * 'a1 * 'a2 * 'a3 * 'a4
    ) Constructor.tag
  | T6 : (
      'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 ->
      ('a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5) Constructor.return,
      'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5
    ) Constructor.tag
  | T7 : (
      'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 ->
      ('a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) Constructor.return,
      'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6
    ) Constructor.tag
  | T8 : (
      'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 ->
      ('a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) Constructor.return,
      'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7
    ) Constructor.tag
  | T9 : (
      'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 ->
      ('a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8)
        Constructor.return,
      'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8
    ) Constructor.tag
  | T10 : (
      'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 ->
      ('a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9)
        Constructor.return,
      'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9
    ) Constructor.tag
  | T11 : (
      'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 ->
      'a10 ->
      ('a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10)
        Constructor.return,
      'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10
    ) Constructor.tag
  | T12 : (
      'a0 -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'a7 -> 'a8 -> 'a9 ->
      'a10 -> 'a11 ->
      ('a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11)
        Constructor.return,
      'a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11
    ) Constructor.tag

let t2 =
  let unify_tag
    : type j b a0 a1.
      (j, b) Constructor.tag ->
      (a0 -> a1 -> (a0 * a1) Constructor.return, j) Constructor.unifier option =
    (function
     | T2 ->
        Some (Assume (fun Type.Equal ->
              Assume (fun Type.Equal -> Equal)))
     | _ -> None)
  in
  let construct x0 x1 = Ok (x0, x1) in
  fun t0 t1 ->
    product' {tag = T2; unify_tag; construct}
    @@ proj t0 fst
    @@ proj t1 snd
    @@ proj_end

let elim_t2 : type a0 a1. (a0 * a1) t -> (a0 t * a1 t) option =
  (function
   | Product ({tag = T2; _}, Proj (t0, _, Proj (t1, _, Proj_end))) ->
      Some (t0, t1)
   | _ -> None)

let t3 =
  let unify_tag
    : type j b a0 a1 a2.
      (j, b) Constructor.tag ->
      (a0 -> a1 -> a2 -> (a0 * a1 * a2) Constructor.return, j)
        Constructor.unifier option =
    (function
     | T3 ->
        Some (Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal -> Equal))))
     | _ -> None)
  in
  let construct x0 x1 x2 = Ok (x0, x1, x2) in
  fun t0 t1 t2 ->
    product' {tag = T3; unify_tag; construct}
    @@ proj t0 (fun (x, _, _) -> x)
    @@ proj t1 (fun (_, x, _) -> x)
    @@ proj t2 (fun (_, _, x) -> x)
    @@ proj_end

let elim_t3 : type a0 a1 a2. (a0 * a1 * a2) t -> (a0 t * a1 t * a2 t) option =
  (function
   | Product ({tag = T3; _},
        Proj (t0, _,
        Proj (t1, _,
        Proj (t2, _, Proj_end)))) ->
      Some (t0, t1, t2)
   | _ -> None)

let t4 =
  let unify_tag
    : type j b a0 a1 a2 a3.
      (j, b) Constructor.tag ->
      (a0 -> a1 -> a2 -> a3 -> (a0 * a1 * a2 * a3) Constructor.return, j)
        Constructor.unifier option =
    (function
     | T4 ->
        Some (Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal -> Equal)))))
     | _ -> None)
  in
  let construct x0 x1 x2 x3 = Ok (x0, x1, x2, x3) in
  fun t0 t1 t2 t3 ->
    product' {tag = T4; unify_tag; construct}
    @@ proj t0 (fun (x, _, _, _) -> x)
    @@ proj t1 (fun (_, x, _, _) -> x)
    @@ proj t2 (fun (_, _, x, _) -> x)
    @@ proj t3 (fun (_, _, _, x) -> x)
    @@ proj_end

let elim_t4
  : type a0 a1 a2 a3.
    (a0 * a1 * a2 * a3) t -> (a0 t * a1 t * a2 t * a3 t) option =
  (function
   | Product ({tag = T4; _},
        Proj (t0, _,
        Proj (t1, _,
        Proj (t2, _,
        Proj (t3, _, Proj_end))))) ->
      Some (t0, t1, t2, t3)
   | _ -> None)

let t5 =
  let unify_tag
    : type j b a0 a1 a2 a3 a4.
      (j, b) Constructor.tag ->
      (a0 -> a1 -> a2 -> a3 -> a4 ->
       (a0 * a1 * a2 * a3 * a4) Constructor.return, j)
        Constructor.unifier option =
    (function
     | T5 ->
        Some (Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal -> Equal))))))
     | _ -> None)
  in
  let construct x0 x1 x2 x3 x4 = Ok (x0, x1, x2, x3, x4) in
  fun t0 t1 t2 t3 t4 ->
    product' {tag = T5; unify_tag; construct}
    @@ proj t0 (fun (x, _, _, _, _) -> x)
    @@ proj t1 (fun (_, x, _, _, _) -> x)
    @@ proj t2 (fun (_, _, x, _, _) -> x)
    @@ proj t3 (fun (_, _, _, x, _) -> x)
    @@ proj t4 (fun (_, _, _, _, x) -> x)
    @@ proj_end

let elim_t5
  : type a0 a1 a2 a3 a4.
    (a0 * a1 * a2 * a3 * a4) t -> (a0 t * a1 t * a2 t * a3 t * a4 t) option =
  (function
   | Product ({tag = T5; _},
        Proj (t0, _,
        Proj (t1, _,
        Proj (t2, _,
        Proj (t3, _,
        Proj (t4, _, Proj_end)))))) ->
      Some (t0, t1, t2, t3, t4)
   | _ -> None)

let t6 =
  let unify_tag
    : type j b a0 a1 a2 a3 a4 a5.
      (j, b) Constructor.tag ->
      (a0 -> a1 -> a2 -> a3 -> a4 -> a5 ->
       (a0 * a1 * a2 * a3 * a4 * a5) Constructor.return, j)
        Constructor.unifier option =
    (function
     | T6 ->
        Some (Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal -> Equal)))))))
     | _ -> None)
  in
  fun t0 t1 t2 t3 t4 t5 ->
    let construct x0 x1 x2 x3 x4 x5 = Ok (x0, x1, x2, x3, x4, x5) in
    product' {tag = T6; unify_tag; construct}
    @@ proj t0 (fun (x, _, _, _, _, _) -> x)
    @@ proj t1 (fun (_, x, _, _, _, _) -> x)
    @@ proj t2 (fun (_, _, x, _, _, _) -> x)
    @@ proj t3 (fun (_, _, _, x, _, _) -> x)
    @@ proj t4 (fun (_, _, _, _, x, _) -> x)
    @@ proj t5 (fun (_, _, _, _, _, x) -> x)
    @@ proj_end

let elim_t6
  : type a0 a1 a2 a3 a4 a5.
    (a0 * a1 * a2 * a3 * a4 * a5) t ->
    (a0 t * a1 t * a2 t * a3 t * a4 t * a5 t) option =
  (function
   | Product ({tag = T6; _},
        Proj (t0, _,
        Proj (t1, _,
        Proj (t2, _,
        Proj (t3, _,
        Proj (t4, _,
        Proj (t5, _, Proj_end))))))) ->
      Some (t0, t1, t2, t3, t4, t5)
   | _ -> None)

let t7 t0 t1 t2 t3 t4 t5 t6 =
  let unify_tag
    : type j b a0 a1 a2 a3 a4 a5 a6.
      (j, b) Constructor.tag ->
      (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 ->
       (a0 * a1 * a2 * a3 * a4 * a5 * a6) Constructor.return, j)
        Constructor.unifier option =
    (function
     | T7 ->
        Some (Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal -> Equal))))))))
     | _ -> None)
  in
  let construct x0 x1 x2 x3 x4 x5 x6 = Ok (x0, x1, x2, x3, x4, x5, x6) in
  product' {tag = T7; unify_tag; construct}
  @@ proj t0 (fun (x, _, _, _, _, _, _) -> x)
  @@ proj t1 (fun (_, x, _, _, _, _, _) -> x)
  @@ proj t2 (fun (_, _, x, _, _, _, _) -> x)
  @@ proj t3 (fun (_, _, _, x, _, _, _) -> x)
  @@ proj t4 (fun (_, _, _, _, x, _, _) -> x)
  @@ proj t5 (fun (_, _, _, _, _, x, _) -> x)
  @@ proj t6 (fun (_, _, _, _, _, _, x) -> x)
  @@ proj_end

let elim_t7
  : type a0 a1 a2 a3 a4 a5 a6.
    (a0 * a1 * a2 * a3 * a4 * a5 * a6) t ->
    (a0 t * a1 t * a2 t * a3 t * a4 t * a5 t * a6 t) option =
  (function
   | Product ({tag = T7; _},
        Proj (t0, _,
        Proj (t1, _,
        Proj (t2, _,
        Proj (t3, _,
        Proj (t4, _,
        Proj (t5, _,
        Proj (t6, _, Proj_end)))))))) ->
      Some (t0, t1, t2, t3, t4, t5, t6)
   | _ -> None)

let t8 t0 t1 t2 t3 t4 t5 t6 t7 =
  let unify_tag
    : type j b a0 a1 a2 a3 a4 a5 a6 a7.
      (j, b) Constructor.tag ->
      (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 ->
       (a0 * a1 * a2 * a3 * a4 * a5 * a6 * a7) Constructor.return, j)
        Constructor.unifier option =
    (function
     | T8 ->
        Some (Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal -> Equal)))))))))
     | _ -> None)
  in
  let construct x0 x1 x2 x3 x4 x5 x6 x7 = Ok (x0, x1, x2, x3, x4, x5, x6, x7) in
  product' {tag = T8; unify_tag; construct}
  @@ proj t0 (fun (x, _, _, _, _, _, _, _) -> x)
  @@ proj t1 (fun (_, x, _, _, _, _, _, _) -> x)
  @@ proj t2 (fun (_, _, x, _, _, _, _, _) -> x)
  @@ proj t3 (fun (_, _, _, x, _, _, _, _) -> x)
  @@ proj t4 (fun (_, _, _, _, x, _, _, _) -> x)
  @@ proj t5 (fun (_, _, _, _, _, x, _, _) -> x)
  @@ proj t6 (fun (_, _, _, _, _, _, x, _) -> x)
  @@ proj t7 (fun (_, _, _, _, _, _, _, x) -> x)
  @@ proj_end

let elim_t8
  : type a0 a1 a2 a3 a4 a5 a6 a7.
    (a0 * a1 * a2 * a3 * a4 * a5 * a6 * a7) t ->
    (a0 t * a1 t * a2 t * a3 t * a4 t * a5 t * a6 t * a7 t) option =
  (function
   | Product ({tag = T8; _},
        Proj (t0, _,
        Proj (t1, _,
        Proj (t2, _,
        Proj (t3, _,
        Proj (t4, _,
        Proj (t5, _,
        Proj (t6, _,
        Proj (t7, _, Proj_end))))))))) ->
      Some (t0, t1, t2, t3, t4, t5, t6, t7)
   | _ -> None)

let t9 t1 t2 t3 t4 t5 t6 t7 t8 t9 =
  let unify_tag
    : type j b a0 a1 a2 a3 a4 a5 a6 a7 a8.
      (j, b) Constructor.tag ->
      (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 ->
       (a0 * a1 * a2 * a3 * a4 * a5 * a6 * a7 * a8) Constructor.return, j)
        Constructor.unifier option =
    (function
     | T9 ->
        Some (Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal -> Equal))))))))))
     | _ -> None)
  in
  let construct x0 x1 x2 x3 x4 x5 x6 x7 x8 =
    Ok (x0, x1, x2, x3, x4, x5, x6, x7, x8)
  in
  product' {tag = T9; unify_tag; construct}
  @@ proj t1 (fun (x, _, _, _, _, _, _, _, _) -> x)
  @@ proj t2 (fun (_, x, _, _, _, _, _, _, _) -> x)
  @@ proj t3 (fun (_, _, x, _, _, _, _, _, _) -> x)
  @@ proj t4 (fun (_, _, _, x, _, _, _, _, _) -> x)
  @@ proj t5 (fun (_, _, _, _, x, _, _, _, _) -> x)
  @@ proj t6 (fun (_, _, _, _, _, x, _, _, _) -> x)
  @@ proj t7 (fun (_, _, _, _, _, _, x, _, _) -> x)
  @@ proj t8 (fun (_, _, _, _, _, _, _, x, _) -> x)
  @@ proj t9 (fun (_, _, _, _, _, _, _, _, x) -> x)
  @@ proj_end

let elim_t9
  : type a0 a1 a2 a3 a4 a5 a6 a7 a8.
    (a0 * a1 * a2 * a3 * a4 * a5 * a6 * a7 * a8) t ->
    (a0 t * a1 t * a2 t * a3 t * a4 t * a5 t * a6 t * a7 t * a8 t) option =
  (function
   | Product ({tag = T9; _},
        Proj (t0, _,
        Proj (t1, _,
        Proj (t2, _,
        Proj (t3, _,
        Proj (t4, _,
        Proj (t5, _,
        Proj (t6, _,
        Proj (t7, _,
        Proj (t8, _, Proj_end)))))))))) ->
      Some (t0, t1, t2, t3, t4, t5, t6, t7, t8)
   | _ -> None)

let t10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 =
  let unify_tag
    : type j b a0 a1 a2 a3 a4 a5 a6 a7 a8 a9.
      (j, b) Constructor.tag ->
      (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 ->
       (a0 * a1 * a2 * a3 * a4 * a5 * a6 * a7 * a8 * a9)
         Constructor.return, j) Constructor.unifier option =
    (function
     | T10 ->
        Some (Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal -> Equal)))))))))))
     | _ -> None)
  in
  let construct x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 =
    Ok (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9)
  in
  product' {tag = T10; unify_tag; construct}
  @@ proj t0 (fun (x, _, _, _, _, _, _, _, _, _) -> x)
  @@ proj t1 (fun (_, x, _, _, _, _, _, _, _, _) -> x)
  @@ proj t2 (fun (_, _, x, _, _, _, _, _, _, _) -> x)
  @@ proj t3 (fun (_, _, _, x, _, _, _, _, _, _) -> x)
  @@ proj t4 (fun (_, _, _, _, x, _, _, _, _, _) -> x)
  @@ proj t5 (fun (_, _, _, _, _, x, _, _, _, _) -> x)
  @@ proj t6 (fun (_, _, _, _, _, _, x, _, _, _) -> x)
  @@ proj t7 (fun (_, _, _, _, _, _, _, x, _, _) -> x)
  @@ proj t8 (fun (_, _, _, _, _, _, _, _, x, _) -> x)
  @@ proj t9 (fun (_, _, _, _, _, _, _, _, _, x) -> x)
  @@ proj_end

let elim_t10
  : type a0 a1 a2 a3 a4 a5 a6 a7 a8 a9.
    (a0 * a1 * a2 * a3 * a4 * a5 * a6 * a7 * a8 * a9) t ->
    (a0 t * a1 t * a2 t * a3 t * a4 t * a5 t * a6 t * a7 t * a8 t * a9 t)
      option =
  (function
   | Product ({tag = T10; _},
        Proj (t0, _,
        Proj (t1, _,
        Proj (t2, _,
        Proj (t3, _,
        Proj (t4, _,
        Proj (t5, _,
        Proj (t6, _,
        Proj (t7, _,
        Proj (t8, _,
        Proj (t9, _, Proj_end))))))))))) ->
      Some (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9)
   | _ -> None)

let t11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 =
  let unify_tag
    : type j b a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10.
      (j, b) Constructor.tag ->
      (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 ->
       (a0 * a1 * a2 * a3 * a4 * a5 * a6 * a7 * a8 * a9 * a10)
         Constructor.return, j) Constructor.unifier option =
    (function
     | T11 ->
        Some (Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal -> Equal))))))))))))
     | _ -> None)
  in
  let construct x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 =
    Ok (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10)
  in
  product' {tag = T11; unify_tag; construct}
  @@ proj t0  (fun (x, _, _, _, _, _, _, _, _, _, _) -> x)
  @@ proj t1  (fun (_, x, _, _, _, _, _, _, _, _, _) -> x)
  @@ proj t2  (fun (_, _, x, _, _, _, _, _, _, _, _) -> x)
  @@ proj t3  (fun (_, _, _, x, _, _, _, _, _, _, _) -> x)
  @@ proj t4  (fun (_, _, _, _, x, _, _, _, _, _, _) -> x)
  @@ proj t5  (fun (_, _, _, _, _, x, _, _, _, _, _) -> x)
  @@ proj t6  (fun (_, _, _, _, _, _, x, _, _, _, _) -> x)
  @@ proj t7  (fun (_, _, _, _, _, _, _, x, _, _, _) -> x)
  @@ proj t8  (fun (_, _, _, _, _, _, _, _, x, _, _) -> x)
  @@ proj t9  (fun (_, _, _, _, _, _, _, _, _, x, _) -> x)
  @@ proj t10 (fun (_, _, _, _, _, _, _, _, _, _, x) -> x)
  @@ proj_end

let elim_t11
  : type a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10.
    (a0 * a1 * a2 * a3 * a4 * a5 * a6 * a7 * a8 * a9 * a10) t ->
    (a0 t * a1 t * a2 t * a3 t * a4 t * a5 t * a6 t * a7 t * a8 t * a9 t
      * a10 t) option =
  (function
   | Product ({tag = T11; _},
        Proj (t0, _,
        Proj (t1, _,
        Proj (t2, _,
        Proj (t3, _,
        Proj (t4, _,
        Proj (t5, _,
        Proj (t6, _,
        Proj (t7, _,
        Proj (t8, _,
        Proj (t9, _,
        Proj (t10, _, Proj_end)))))))))))) ->
      Some (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
   | _ -> None)

let t12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 =
  let unify_tag
    : type j b a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11.
      (j, b) Constructor.tag ->
      (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 ->
        (a0 * a1 * a2 * a3 * a4 * a5 * a6 * a7 * a8 * a9 * a10 * a11)
          Constructor.return, j) Constructor.unifier option =
    (function
     | T12 ->
        Some (Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal ->
              Assume (fun Type.Equal -> Equal)))))))))))))
     | _ -> None)
  in
  let construct x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 =
    Ok (x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
  in
  product' {tag = T12; unify_tag; construct}
  @@ proj t0  (fun (x, _, _, _, _, _, _, _, _, _, _, _) -> x)
  @@ proj t1  (fun (_, x, _, _, _, _, _, _, _, _, _, _) -> x)
  @@ proj t2  (fun (_, _, x, _, _, _, _, _, _, _, _, _) -> x)
  @@ proj t3  (fun (_, _, _, x, _, _, _, _, _, _, _, _) -> x)
  @@ proj t4  (fun (_, _, _, _, x, _, _, _, _, _, _, _) -> x)
  @@ proj t5  (fun (_, _, _, _, _, x, _, _, _, _, _, _) -> x)
  @@ proj t6  (fun (_, _, _, _, _, _, x, _, _, _, _, _) -> x)
  @@ proj t7  (fun (_, _, _, _, _, _, _, x, _, _, _, _) -> x)
  @@ proj t8  (fun (_, _, _, _, _, _, _, _, x, _, _, _) -> x)
  @@ proj t9  (fun (_, _, _, _, _, _, _, _, _, x, _, _) -> x)
  @@ proj t10 (fun (_, _, _, _, _, _, _, _, _, _, x, _) -> x)
  @@ proj t11 (fun (_, _, _, _, _, _, _, _, _, _, _, x) -> x)
  @@ proj_end

let elim_t12
  : type a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11.
    (a0 * a1 * a2 * a3 * a4 * a5 * a6 * a7 * a8 * a9 * a10 * a11) t ->
    (a0 t * a1 t * a2 t * a3 t * a4 t * a5 t * a6 t * a7 t * a8 t * a9 t
      * a10 t * a11 t) option =
  (function
   | Product ({tag = T12; _},
        Proj (t0, _,
        Proj (t1, _,
        Proj (t2, _,
        Proj (t3, _,
        Proj (t4, _,
        Proj (t5, _,
        Proj (t6, _,
        Proj (t7, _,
        Proj (t8, _,
        Proj (t9, _,
        Proj (t10, _,
        Proj (t11, _, Proj_end))))))))))))) ->
      Some (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
   | _ -> None)

let custom ~encode ~decode rep =
  let encode' x =
    (match encode x with
     | Ok y -> y
     | Error msg -> raise (Reject msg))
  in
  product decode @@ proj rep encode' @@ proj_end

let redacted t = Annot (`Redacted, t)

let bool = field Bool
let int = field Int
let int16 = field Int16
let int32 = field Int32
let int64 = field Int64
let float = field Float
let string = field String
let octets = field Octets
let pdate = field Pdate
let ptime = field Ptime
let ptime_span = field Ptime_span
