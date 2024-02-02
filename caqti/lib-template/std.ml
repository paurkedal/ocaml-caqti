(* Copyright (C) 2024  Petter A. Urkedal <paurkedal@gmail.com>
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

let (-->.) t u ?oneshot f = Request.create ?oneshot t u Row_mult.zero f
let (-->!) t u ?oneshot f = Request.create ?oneshot t u Row_mult.one f
let (-->?) t u ?oneshot f = Request.create ?oneshot t u Row_mult.zero_or_one f
let (-->*) t u ?oneshot f = Request.create ?oneshot t u Row_mult.zero_or_more f

let (@:-) f s =
  let q = Query.of_string_exn s in
  f (fun _ -> q)

let (@@:-) f g =
  f (fun di -> Query.of_string_exn (g (Driver_info.dialect_tag di)))

let (->.) t u ?oneshot s = Request.create ?oneshot t u Row_mult.zero @:- s
let (->!) t u ?oneshot s = Request.create ?oneshot t u Row_mult.one @:- s
let (->?) t u ?oneshot s = Request.create ?oneshot t u Row_mult.zero_or_one @:- s
let (->*) t u ?oneshot s = Request.create ?oneshot t u Row_mult.zero_or_more @:- s

include (Row_type : Row_type.STD)
