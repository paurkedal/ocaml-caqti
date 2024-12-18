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

module Dialect = Dialect
module Field_type = Field_type
module Query = Query
module Query_fmt = Query_fmt
module Request = Request
module Row_mult = Row_mult
module Row_type = Row_type
module Shims = Shims
module Version = Version

module type CREATE = sig
  include module type of Version.Infix
  include module type of Query.Infix
  include module type of Request.Infix
  include Row_type.STD
  module D = Dialect
  module Q = Query
  module Qf = Query_fmt
end

module Create = struct
  include Version.Infix
  include Query.Infix
  include Request.Infix
  include (Row_type : Row_type.STD)
  module D = Dialect
  module Q = Query
  module Qf = Query_fmt
end
