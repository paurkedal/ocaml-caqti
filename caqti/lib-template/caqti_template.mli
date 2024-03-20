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

(** {2 Submodules for Advanced Usage}

    These modules are part of the stable API, but the casual user may find it
    sufficient to use the top-level definitions and reexports described in the
    following sections. *)

module Driver_info = Driver_info
module Field_type = Field_type
module Query = Query
module Query_fmt = Query_fmt
module Request = Request
module Row_mult = Row_mult
module Row_type = Row_type
module Shims = Shims

(** {2 Request Template Construction}

    The following imports collects everything you need to create
    {{!Caqti_template.Request} request templates}, which describe the (prepared)
    query to send to the database, the parameters type, the result row type, and
    the expected multiplicity of the result.  So, you can simply use a local
    open,
    {[
      let bounds_upto_req =
        let open Caqti_template.Std in
        T.(t2 int32 float ->! option (t2 float float))
        "SELECT min(y), max(y) FROM samples WHERE series_id = ? AND x < ?"
    ]}
    Here, [?] is used to refer to parameters, but you can also use the
    [PostgreSQL]-style [$1], [$2], etc. if you prefer, as long as you stick to
    the same convention within a single query.  Caqti translates parameter
    references and rearrages parameters if necessary so that it will work as
    expected indepnedent of the database system.

    Apart from parameter references, there are differences between SQL dialects
    which often makes it necessary to use different queries for different
    database systems.  The driver-dependent constructors (long arrows) allows to
    you define a single query template which works across database systems, e.g.
    {[
      let concat_req =
        let open Caqti_template.Std in
        T.(t2 string string -->! string) @@:- function
         | `Mysql -> "SELECT concat(?, ?)"
         | _ -> "SELECT ? || ?"
    ]}
    The long arrows expects a function which receives a
    {!Caqti_template.Driver_info.t} and returns a {!Caqti_template.Query.t}.
    Since we only need the {!Caqti_template.Driver_info.type-dialect_tag} for
    dispatching and since we want to return the query i the form of a string, we
    use the shortcut [@@:-] which transforms the function accordingly before
    applying it to the request constructor.  That is, the above example expands
    to
    {[
      let concat_req =
        let open Caqti_template.Std in
        T.(t2 string string -->! string) @@ fun driver_info ->
          (match Caqti_template.Driver_info.dialect_tag driver_info with
           | `Mysql -> Caqti_template.Query.of_string_exn "SELECT concat(?, ?)"
           | _ -> Caqti_template.Query.of_string_exn "SELECT ? || ?")
    ]}

    For more complex applications it may be convenient to create a module with
    custom definitions, maybe extending the current API to avoid double open:
    {[
      module Ct : sig
        open Caqti_template

        include module type of Caqti_template.Std

        module T : sig
          include Row_type.STD
          val password : string Row_type.t
          val uri : Uri.t Row_type.t
        end

      end = struct
        open Caqti_template

        include Caqti_template.Std

        module T = struct
          include (Row_type : Row_type.STD)
          let password = redacted string
          let uri =
            let encode x = Ok (Uri.to_string x) in
            let decode s = Ok (Uri.of_string s) in
            Row_type.custom ~encode ~decode string
        end

      end
    ]}
  *)

module Std : sig
  include module type of Request.Infix
  module T : Row_type.STD
  module Q = Query
  module Qf = Query_fmt
end
