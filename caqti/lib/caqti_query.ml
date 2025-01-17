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

open Caqti_template

include Caqti_template.Query
include Caqti_template.Query.Private [@@alert "-caqti_private"]

type expand_error = Query.Expand_error.t
let pp_expand_error = Query.Expand_error.pp

let of_string repr =
  let conv err = `Invalid Query.Parse_error.(position err, message err) in
  Query.parse_result repr |> Result.map_error conv

let of_string_exn repr =
  (try Query.parse repr with
   | Query.Parse_error err -> Format.kasprintf failwith "%a" Parse_error.pp err)

let concat sep = concat ~sep
let qprintf = Query_fmt.qprintf
let kqprintf = Query_fmt.kqprintf
let param = Query_fmt.param
let env = Query_fmt.env
let quote = Query_fmt.quote
let query = Query_fmt.query
