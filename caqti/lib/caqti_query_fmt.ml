(* Copyright (C) 2023  Petter A. Urkedal <paurkedal@gmail.com>
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

type 'a t = Format.formatter -> 'a -> unit

[@@@alert "-deprecated"]
let qprintf = Caqti_query.qprintf
let kqprintf = Caqti_query.kqprintf
let param = Caqti_query.param
let env = Caqti_query.env
let quote = Caqti_query.quote
let query = Caqti_query.query
[@@@alert "+deprecated"]

let bool ppf x = query ppf (V (Caqti_type.Field.Bool, x))
let int ppf x = query ppf (V (Caqti_type.Field.Int, x))
let float ppf x = query ppf (V (Caqti_type.Field.Float, x))
let string ppf x = query ppf (V (Caqti_type.Field.String, x))
let octets ppf x = query ppf (V (Caqti_type.Field.Octets, x))
let pdate ppf x = query ppf (V (Caqti_type.Field.Pdate, x))
let ptime ppf x = query ppf (V (Caqti_type.Field.Ptime, x))
let ptime_span ppf x = query ppf (V (Caqti_type.Field.Ptime_span, x))
