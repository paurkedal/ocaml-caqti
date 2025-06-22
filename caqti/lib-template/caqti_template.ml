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

module Constructor = Constructor
module Dialect = Dialect
module Field_type = Field_type
module Query = Query
module Query_fmt = Query_fmt
module Request = Request
module Row_mult = Row_mult
module Row_type = Row_type
module Row = Row
module Shims = Shims
module Version = Version

module Type = struct
  include (Row_type : Row_type.STD)
  include Request_type.Infix
end

module type CREATE = sig

  module T = Type

  module D = Dialect
  include module type of Version.Infix

  module Q = Query
  module Qf = Query_fmt
  include module type of Query.Infix

  val static :
    ('a, 'b, 'm) Request_type.t -> string ->
    ('a, 'b, 'm) Request.t

  val static_gen :
    ('a, 'b, 'm) Request_type.t -> (Dialect.t -> Query.t) ->
    ('a, 'b, 'm) Request.t

  val dynamic :
    ('a, 'b, 'm) Request_type.t -> string ->
    ('a, 'b, 'm) Request.t

  val dynamic_gen :
    ('a, 'b, 'm) Request_type.t -> (Dialect.t -> Query.t) ->
    ('a, 'b, 'm) Request.t

  val direct :
    ('a, 'b, 'm) Request_type.t -> string ->
    ('a, 'b, 'm) Request.t

  val direct_gen :
    ('a, 'b, 'm) Request_type.t -> (Dialect.t -> Query.t) ->
    ('a, 'b, 'm) Request.t
end

module Create = struct

  module T = Type

  module D = Dialect
  include Version.Infix

  module Q = Query
  module Qf = Query_fmt
  include Query.Infix

  let static req_type qs =
    Request.create Static req_type (Fun.const (Query.parse qs))

  let static_gen req_type qf =
    Request.create Static req_type qf

  let dynamic req_type qs =
    Request.create Dynamic req_type (Fun.const (Query.parse qs))

  let dynamic_gen req_type qf =
    Request.create Dynamic req_type qf

  let direct req_type qs =
    Request.create Direct req_type (Fun.const (Query.parse qs))

  let direct_gen req_type qf =
    Request.create Direct req_type qf

end
