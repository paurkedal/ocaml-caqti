(* Copyright (C) 2025  Petter A. Urkedal <paurkedal@gmail.com>
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

type ('a, 'b, 'm) t = 'a Row_type.t * 'b Row_type.t * 'm Row_mult.t

module Infix = struct
  let ( -->. ) t u = (t, u, Row_mult.zero)
  let ( -->! ) t u = (t, u, Row_mult.one)
  let ( -->? ) t u = (t, u, Row_mult.zero_or_one)
  let ( -->* ) t u = (t, u, Row_mult.zero_or_more)
end

let param_type (t, _, _) = t
let row_type (_, u, _) = u
let row_mult (_, _, m) = m
