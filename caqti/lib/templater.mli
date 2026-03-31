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

(** This is a convenience API which collects everything needed to create
    {{!Template.Request} request templates}.
    A request template describes an SQL query, how to encode parameters, and how
    to decode result rows.
    This is the recommended entry point for typical usage.

    {1 Basic Usage}

    Consider the example:
    {[
      let bounds_upto_req =
        let open Caqti.Templater in
        static T.(t2 int32 float -->! option (t2 float float))
        "SELECT min(y), max(y) FROM samples WHERE series_id = ? AND x < ?"
    ]}
    First we opening the current module.
    We then pick the function {!static} to create a template for prepared
    queries where the query template has static lifetime.
    The first argument describes the parameter type and the result row type
    combined with an arrow which describes the multiplicity of the result rows.
    The exclamation mark in the arrow indicates that precisely one result row is
    expected.
    The second argument is the query template, here in the form of a string.

    In the query template, [?] refer to parameters, but you can also use the
    [PostgreSQL]-style [$1], [$2], etc. if you prefer, as long as you stick to
    the same convention for a given query template.
    Caqti drivers translate parameter references to fit the database system,
    rearranging parameters if necessary.

    Caqti provides a way to handle dialectical differences between database
    systems apart from the parameter syntax.
    The example above uses a shortcut, since it does not need this
    functionality.
    In the full form it looks like:
    {[
      let bounds_upto_req =
        let open Caqti.Templater in
        static_gen
          T.(t2 int32 float -->! option (t2 float float)) @@ Fun.const @@
          Q.parse
            "SELECT min(y), max(y) FROM samples WHERE series_id = ? AND x < ?"
    ]}
    The callback receives a {!Template.Dialect.t} and returns a
    {!Template.Query.t}.
    We can now see that the still same query string is explicitly parsed.
    {!Template.Query} and {!Template.Query_fmt} provides alternative ways of
    constructing query template which is more suitable for dynamically generated
    queries.

    The following example makes use of the dialect argument to handle
    dialectical differences regarding string concatenation:
    {[
      let concat_req =
        let open Caqti.Templater in
        static_gen T.(t2 string string -->! string) @@ function
         | D.Mysql _ -> Q.parse "SELECT concat(?, ?)"
         | _ -> Q.parse "SELECT ? || ?"
    ]}
    In summary

      - Pick the main function according to the lifetime of prepared queries
        and whether to use the simplified or generic callback.
      - In the request type argument, the arrow decoration
        selects the expected multiplicity of result rows:
        [-->.] for zero, [-->!] for one, [-->?] for zero or one, [-->*] for
        zero or more.

    {1 Supplementing}

    If needed, you can supplement the current module with custom types:
    {[
      module Ct : sig
        open Caqti.Template

        include module type of (struct include Caqti.Templater end)

        module T : sig
          include module type of T

          val password : string Row_type.t
          val uri : Uri.t Row_type.t
        end
      end = struct
        open Caqti.Template

        include Caqti.Templater

        module T = struct
          include T

          let password = redacted string (* a string redacted from logs *)

          let uri =
            let encode x = Ok (Uri.to_string x) in
            let decode s = Ok (Uri.of_string s) in
            Row_type.custom ~encode ~decode string
        end
      end
    ]}
*)

open Template

(** {1 Reference} *)

(** {2 Type Descriptors} *)

module T : sig
  include Template.Row_type.STD
  include module type of Template.Request_type.Infix
end
(** This module imports everything needed to describe a request type, including
    the parameter type, row type, and row multiplicity. *)

(** {2 Dialect Descriptors} *)

module D = Template.Dialect
include module type of Template.Version.Infix

(** {2 Query Templates} *)

module Q = Template.Query
module Qf = Query_fmt
include module type of Template.Query.Infix

(** {2 Request Templates}

    The following are shortcuts for {!Template.Request.create} and {!Template.Query.parse} In
    particular {!static}, {!dynamic}, and {!direct} covers the most common case
    of sending a pre-composed query string to the database while the
    {!static_gen}, {!dynamic_gen}, and {!direct_gen} are the correspending fully
    generic variants.

    Where the query is supplied as function of a dialect, the function should be
    pure and may be recalled repeatedly, cf. {!Template.Request.create}. *)

val static :
  ('a, 'b, 'm) Template.Request_type.t -> string ->
  ('a, 'b, 'm) Template.Request.t
(** Creates a template of static lifetime for prepared requests where the query
    template is provided as a string to be parsed by {!Template.Query.parse}. *)

val static_gen :
  ('a, 'b, 'm) Template.Request_type.t -> (Template.Dialect.t -> Template.Query.t) ->
  ('a, 'b, 'm) Template.Request.t
(** Creates a template of static lifetime for prepared requests where the query
    template is dialect-dependent and explicitly constructed by the caller. *)

val static_gen_multi :
  (Template.Dialect.t -> Template.Query.t list) -> (unit, unit, Template.Row_mult.zero) Template.Request.t
(** A variant of {!static_gen} which executes multiple queries which must
    receive no parameters and return no result rows.

    Depending on the chosen driver, statements may be prepared individually,
    with the limitations this implies, cf. {!Template.Request.create_multi}.
    Esp. {!direct_gen_multi} is recommended for schema initialization and
    updates. *)

val dynamic :
  ('a, 'b, 'm) Template.Request_type.t -> string ->
  ('a, 'b, 'm) Template.Request.t
(** Creates a template of static lifetime for prepared requests where the query
    template is provided as a string to be parsed by {!Template.Query.parse}. *)

val dynamic_gen :
  ('a, 'b, 'm) Template.Request_type.t -> (Template.Dialect.t -> Template.Query.t) ->
  ('a, 'b, 'm) Template.Request.t
(** Creates a template of static lifetime for prepared requests where the query
    template is dialect-dependent and explicitly constructed by the caller. *)

val dynamic_gen_multi :
  (Template.Dialect.t -> Template.Query.t list) -> (unit, unit, Template.Row_mult.zero) Template.Request.t
(** A variant of {!dynamic_gen} which executes multiple queries which must
    receive no parameters and return no result rows.

    Depending on the chosen driver, statements may be prepared individually,
    with the limitations this implies, cf. {!Template.Request.create_multi}.
    Esp. {!direct_gen_multi} is recommended for schema initialization and
    updates. *)

val direct :
  ('a, 'b, 'm) Template.Request_type.t -> string ->
  ('a, 'b, 'm) Template.Request.t
(** Creates a template for non-prepared requests where the query template is
    provided as a string to be parsed by {!Template.Query.parse}.
    If non-prepared requests are not unsupported by the driver, a temporarily
    prepared request is used instead. *)

val direct_gen :
  ('a, 'b, 'm) Template.Request_type.t -> (Template.Dialect.t -> Template.Query.t) ->
  ('a, 'b, 'm) Template.Request.t
(** Creates a template for non-prepared requests where the query template is
    dialect-dependent and explicitly constructed by the caller.
    If non-prepared requests are not unsupported by the driver, a temporarily
    prepared request is used instead. *)

val direct_gen_multi :
  (Template.Dialect.t -> Template.Query.t list) -> (unit, unit, Template.Row_mult.zero) Template.Request.t
(** A variant of {!direct_gen} which executes multiple queries which must
    receive no parameters and return no result rows.
    To ensure that direct queries are used when available, the queries must have
    no quoted string or values, since these may produce implied parameters. *)
