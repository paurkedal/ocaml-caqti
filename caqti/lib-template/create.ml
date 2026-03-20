(* Copyright (C) 2026  Petter A. Urkedal <paurkedal@gmail.com>
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

module T = struct
  include (Row_type : Row_type.STD)
  include Request_type.Infix
end

module D = Dialect
include Version.Infix

module Q = Query
module Qf = Query_fmt
include Query.Infix

let static rt qs = Request.create Static rt (Fun.const (Query.parse qs))
let static_gen req_type qf = Request.create Static req_type qf
let static_gen_multi qf = Request.create_multi Static qf

let dynamic rt qs = Request.create Dynamic rt (Fun.const (Query.parse qs))
let dynamic_gen rt qf = Request.create Dynamic rt qf
let dynamic_gen_multi qf = Request.create_multi Dynamic qf

let direct rt qs = Request.create Direct rt (Fun.const (Query.parse qs))
let direct_gen rt qf = Request.create Direct rt qf
let direct_gen_multi qf = Request.create_multi Direct qf
