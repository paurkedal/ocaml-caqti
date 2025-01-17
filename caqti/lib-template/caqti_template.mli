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

[@@@alert caqti_unstable
  "This library is a preview; expect incompatible changes without prior notice."]

(** {2 Primitives}

    These modules are part of the stable API, but the casual user may find it
    sufficient to use the top-level definitions and reexports described in the
    following sections. *)

(** {3 Prerequisities} *)

module Shims = Shims
module Version = Version
module Dialect = Dialect

(** {3 Data Types} *)

module Field_type = Field_type
module Row_type = Row_type
module Row_mult = Row_mult

(** {3 Request Templates} *)

module Query = Query
module Query_fmt = Query_fmt
module Request = Request

(** {2 Convenience} *)

module type CREATE = sig
  (** This is a convenience API which collects everything needed to create
      {{!Caqti_template.Request} request templates}.  A request template
      describes a database query and how to encode parameters and decode the
      result.

      Consider the example:
      {[
        let bounds_upto_req =
          let open Caqti_template.Create in
          (t2 int32 float ->! option (t2 float float))
          "SELECT min(y), max(y) FROM samples WHERE series_id = ? AND x < ?"
      ]}
      The first open provides shortcuts to everything we need.  This is followed
      by a description of the parameter type and result row type combined with
      an arrow which describes the multiplicity of the result rows.  The
      exclamation mark in the arrow indicates that precisely one result row is
      expected.  This line describing the shape of the data involved evaluates
      to a function which takes a query template as the only argument, and
      returns the request template.

      In the query template, [?] refer to parameters, but you can also use the
      [PostgreSQL]-style [$1], [$2], etc. if you prefer, as long as you stick to
      the same convention for a given query template.  Caqti drivers translate
      parameter references and rearranges parameters if necessary so that it
      will work as expected independent of the database system.

      Caqti provides a way to handle dialectical differences between database
      systems apart from the parameter syntax.  The example above uses uses a
      shortcut, since it does not need this functionality.  In the full form it
      looks like:
      {[
        let bounds_upto_req =
          let open Caqti_template.Create in
          t2 int32 float -->! option (t2 float float) @@ fun _ ->
          Q.parse
            "SELECT min(y), max(y) FROM samples WHERE series_id = ? AND x < ?"
      ]}
      Note the use of the longer arrow [-->!], which takes a function instead of
      a string as the last argument.  The function receives a {!Dialect.t} as
      the first argument and returns a {!Query.t}.  Here we parse the original
      query string, but the {!Query} and {!Query_fmt} modules provides
      alternative ways of constructing query template which is more suitable for
      dynamically generated queries.

      The following example makes use of the dialect argument to handle
      dialectical differences regarding string concatenation:
      {[
        let concat_req =
          let open Caqti_template.Create in
          t2 string string -->! string @@:- function
           | D.Mysql _ -> "SELECT concat(?, ?)"
           | _ -> "SELECT ? || ?"
      ]}
      Note that we here use the [@@:-] combinator instead of [@@] to avoid
      calling {!Q.parse} on the result.  In summary

        - Short arrows are shortcuts for the most common usage.
        - Long arrows provides the full functionality.
        - The arrow decoration ([.], [!], [?], [*]) selects the expected
          multiplicity (zero, one, zero or one, zero or more) of result rows.
        - Additional combinators can be used with the long arrows as shortcuts
          for the final application to simplify the specification of the query
          template.

      Applications which use custom row type descriptors may want to combine
      them with the current module.  The following example shows how to
      formulate an interface and implementation with two additional row types:
      {[
        module Ct : sig
          open Caqti_template

          include Caqti_template.CREATE

          val password : string Row_type.t
          val uri : Uri.t Row_type.t

        end = struct
          open Caqti_template

          include Caqti_template.Create

          let password = redacted string
          let uri =
            let encode x = Ok (Uri.to_string x) in
            let decode s = Ok (Uri.of_string s) in
            Row_type.custom ~encode ~decode string

        end
      ]}
  *)

  include module type of Version.Infix
  include module type of Query.Infix
  include module type of Request.Infix
  include Row_type.STD
  module D = Dialect
  module Q = Query
  module Qf = Query_fmt
end

module Create : CREATE
