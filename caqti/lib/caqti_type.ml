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

type ('a, 'b) eq = ('a, 'b) Caqti_template.Shims.Type.eq = Equal : ('a, 'a) eq
module Field = Caqti_template.Field_type
include Caqti_template.Row_type
include Caqti_template.Row_type.Private
module Std = struct
  include (Caqti_template.Row_type : Caqti_template.Row_type.STD)

  (* moved away from STD signature *)
  let enum = enum
  let product = product
  let proj = proj
  let proj_end = proj_end
  let custom = custom

  (* deprecated *)
  let tup2 = t2
  let tup3 = t3
  let tup4 = t4
end
include Std
