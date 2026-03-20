(* Copyright (C) 2024--2026  Petter A. Urkedal <paurkedal@gmail.com>
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

[@@@alert caqti_unstable
  "This library is a preview; expect incompatible changes without prior notice."]

(** {2 Primitives}

    These modules are part of the stable API, but the casual user may find it
    sufficient to use the {!Create} module. *)

(** {3 Prerequisities} *)

module Shims = Shims
module Version = Version
module Dialect = Dialect

(** {3 Data Types} *)

module Constructor = Constructor
module Field_type = Field_type
module Row_type = Row_type
module Row_mult = Row_mult
module Row = Row

(** {3 Request Templates} *)

module Query = Query
module Query_fmt = Query_fmt
module Request = Request

(** {2 Convenience} *)

module Create = Create
