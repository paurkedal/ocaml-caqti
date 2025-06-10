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

[@@@alert "-caqti_private"]

open Caqti_template
type ('a, 'b) eq = ('a, 'b) Caqti_template.Shims.Type.eq = Equal : ('a, 'a) eq
module Field = Caqti_template.Field_type
include Caqti_template.Row_type
let equal_value = Caqti_template.Row.equal
let pp_value ppf (t, v) = Caqti_template.Row.pp t ppf v

type (_, _) product =
  | Proj_end : ('a, 'a) product
  | Proj : 'b t * ('a -> 'b) * ('a, 'i) product -> ('a, 'b -> 'i) product

let field ft = Private.Field ft

module Std = struct
  include (Caqti_template.Row_type : Caqti_template.Row_type.STD)

  (* moved away from STD signature *)
  let enum = enum

  type (_, _) rewritten_product =
    | Rewritten_product :
        ('i -> 'j) * ('j, 'a) Row_type.product ->
        ('i, 'a) rewritten_product

  let rec rewrite_product
    : type i a. (a, i) product -> (i, a) rewritten_product =
    (function
     | Proj_end ->
        Rewritten_product (Result.ok, Row_type.Private.Proj_end)
     | Proj (t, p, tps) ->
        let Rewritten_product (conv, ts') = rewrite_product tps in
        let conv' f = fun x -> conv (f x) in
        Rewritten_product (conv', Row_type.Private.Proj (t, p, ts')))

  let product intro tps =
    let Rewritten_product (conv, ts') = rewrite_product tps in
    product (conv intro) ts'

  let proj t p tps = Proj (t, p, tps)
  let proj_end = Proj_end

  let custom = custom

  (* deprecated *)
  let tup2 = t2
  let tup3 = t3
  let tup4 = t4
end
include Std
