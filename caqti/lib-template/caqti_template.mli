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

(** {2 Infix Operators for Constructing Request Templates}

    The following operators provides a more visually appealing way of expressing
    requests.  They are implemented in terms of {!Request.create} and
    {!Query.of_string_exn}, meaning that the query string arguments accepts
    {{!query_template} The Syntax of Query Templates}.

    The [?oneshot] argument defaults to [false], so when not constructing
    one-shot queries, the full application [(pt -->! rt) f] can be written
    [pt -->! rt @@ f], which motivates the {!(@:-)} and {!(@@:-)} shortcuts.

    In the simplest case you can use this module directly:
    {[
      let bounds_upto_req =
        let open Caqti_template.Std in
        tup2 int32 float -->! option (tup2 float float) @:-
        "SELECT min(y), max(y) FROM samples WHERE series_id = ? AND x < ?"
    ]}
    For more complex applications it may be convenient to provide a custom
    module, to avoid the double open and to customize the operators, e.g.
    {[
      module Caqtireq = struct
        include Caqti_template.Std
        include Caqti_type_calendar (* if needed, link caqti-type-calendar *)

        (* Any additional types. *)
        let password = redacted string
        let uri =
          custom ~encode:(fun x -> Ok (Uri.to_string x))
                 ~decode:(fun s -> Ok (Uri.of_string s)) string

        (* Optionally define a custom environment providing two schema names.
         * The references should only be assigned at startup. *)
        let myapp_schema = ref "myapp"
        let mylib_schema = ref "mylib"
        let env = function
         | "" -> Query.L !myapp_schema
         | "mylib" -> Query.L !mylib_schema
         | _ -> raise Not_found

        (* Since we have a custom environment, override the definitions of the
         * following operators to perform the substitution. *)
        let (@:-) t qs =
          let q = Query.expand env (Query.of_string_exn qs) in
          t (fun _ -> q)
        let (@@:-) t qsf =
          t (fun driver_info ->
            let qs = qsf (Driver_info.dialect_tag driver_info) in
            Query.expand env (Query.of_string_exn qs))
      end
    ]}
    If you don't like using global references, or you need to work with
    different environments for different connections, you should instead pass
    the environment function when connecting to the database.  We can now
    simplify and schema-qualify the previous request,
    {[
      let bounds_upto_req =
        let open Caqtireq in
        tup2 int32 float -->! option (tup2 float float) @:-
        "SELECT min(y), max(y) FROM $.samples WHERE series_id = ? AND x < ?"
    ]}
    {!section:indep} also provides alternative arrow operators for this common
    case, which allows using short-from local open,
    {[
      let bounds_upto_req =
        Caqtireq.(tup2 int32 float ->! option (tup2 float float))
        "SELECT min(y), max(y) FROM $.samples WHERE series_id = ? AND x < ?"
    ]}
 *)

(** {3:indep Constructors for Driver-Independent Requests} *)

val ( ->. ) :
  'a Row_type.t -> unit Row_type.t ->
  ?oneshot: bool -> string -> ('a, unit, [`Zero]) Request.t
(** [(pt ->. Row_type.unit) ?oneshot s] is the request which sends the query
    string [s], encodes parameters according to [pt], and expects no result
    rows. See {!Request.create} for the meaning of [oneshot]. *)

val ( ->! ) :
  'a Row_type.t -> 'b Row_type.t ->
  ?oneshot: bool -> string -> ('a, 'b, [`One]) Request.t
(** [(pt ->! rt) ?oneshot s] is the request which sends the query string [s],
    encodes parameters according to [pt], and decodes a single result row
    according to [rt]. See {!Request.create} for the meaning of [oneshot]. *)

val ( ->? ) :
  'a Row_type.t -> 'b Row_type.t ->
  ?oneshot: bool -> string -> ('a, 'b, [`Zero | `One]) Request.t
(** [(pt ->? rt) ?oneshot s] is the request which sends the query string [s],
    encodes parameters according to [pt], and decodes zero or one result row
    according to [rt]. See {!Request.create} for the meaning of [oneshot]. *)

val ( ->* ) :
  'a Row_type.t -> 'b Row_type.t ->
  ?oneshot: bool -> string -> ('a, 'b, [`Zero | `One | `Many]) Request.t
(** [(pt ->* rt) ?oneshot s] is the request which sends the query string [s],
    encodes parameters according to [pt], and decodes any number of result rows
    according to [rt]. See {!Request.create} for the meaning of [oneshot]. *)

(** {3 Constructors for Driver-Dependent Requests}

    The below arrow operators takes a function instead of a string as their
    third argument.  The function receives information about the current driver
    and returns a {!Query.t}.  This is the most general way of providing the
    query string.

    As an alternative to using plain application (or [@@]) for the third
    positional argument, additional application operators are provided for
    convenience. *)

val ( -->. ) :
  'a Row_type.t -> unit Row_type.t ->
  ?oneshot: bool -> (Driver_info.t -> Query.t) ->
  ('a, unit, [`Zero]) Request.t
(** [(pt -->. Row_type.unit) ?oneshot f] is the request which sends the
    query string returned by [f], encodes parameters according to [pt], and
    expects no result rows. See {!Request.create} for the meaning of
    [oneshot]. *)

val ( -->! ) :
  'a Row_type.t -> 'b Row_type.t ->
  ?oneshot: bool -> (Driver_info.t -> Query.t) ->
  ('a, 'b, [`One]) Request.t
(** [(pt -->! rt) ?oneshot f] is the request which sends the query string
    returned by [f], encodes parameters according to [pt], and decodes a
    single result row according to [rt]. See {!Request.create} for the meaning
    of [oneshot]. *)

val ( -->? ) :
  'a Row_type.t -> 'b Row_type.t ->
  ?oneshot: bool -> (Driver_info.t -> Query.t) ->
  ('a, 'b, [`Zero | `One]) Request.t
(** [(pt -->? rt) ?oneshot f] is the request which sends the query string
    returned by [f], encodes parameters according to [pt], and decodes zero or
    one result row according to [rt]. See {!Request.create} for the meaning of
    [oneshot]. *)

val ( -->* ) :
  'a Row_type.t -> 'b Row_type.t ->
  ?oneshot: bool -> (Driver_info.t -> Query.t) ->
  ('a, 'b, [`Zero | `One | `Many]) Request.t
(** [(pt -->* rt) ?oneshot f] is the request which sends the query string
    returned by [f], encodes parameters according to [pt], and decodes any
    number of result rows according to [rt]. See {!Request.create} for the
    meaning of [oneshot]. *)

val ( @:- ) :
  ((Driver_info.t -> Query.t) -> ('a, 'b, 'm) Request.t) ->
  string -> ('a, 'b, 'm) Request.t
(** Applies a dialect-independent query string which is parsed with
    {!Query.of_string_exn}.  Composition with arrow operators from this
    section, gives the corresponding operators from {!section:indep}. *)

val ( @@:- ) :
  ((Driver_info.t -> Query.t) -> ('a, 'b, 'm) Request.t) ->
  (Driver_info.dialect_tag -> string) -> ('a, 'b, 'm) Request.t
(** Applies a dialect-dependent query string which is parsed with
    {!Query.of_string_exn}. *)

(** {2 Reexported Row Types} *)

include Row_type.STD
