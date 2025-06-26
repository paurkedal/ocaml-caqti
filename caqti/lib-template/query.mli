(* Copyright (C) 2019--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

(** This module provides templating of database query strings.  It helps
    mitigate differences between database systems, and provides additional
    functionality such as variable substitution and safe embedding of values.
    The representation is also suited for dynamic construction.

    There are three ways to construct a template:

      - Using the parser ({!of_string}, {!of_string_exn}, etc.) if the query
        template is known at compile time.

      - Using the {{!query_construction} constructors} of the current module.

      - Using the {!Query_fmt} module, which provides an alternative to the
        previous option. *)

(**/**)
module Private : sig
  type t =
    | L of string
    | V : 'a Field_type.t * 'a -> t
    | Q of string
    | P of int
    | E of string
    | S of t list
end [@@alert caqti_private]
(**/**)

type t = Private.t [@@alert "-caqti_private"]
(** [t] is an intermediate representation of a query string to be send to a
    database, possibly combined with some hidden parameters used to safely embed
    values.  Apart from embedding values, this representation provides indexed
    parameter references, independent of the target database system.  For
    databases which use linear parameter references (like [?] for MariaDB), the
    driver will reshuffle, elide, and duplicate parameters as needed. *)

(** {2:query_construction Construction} *)

val empty : t
(** [empty] is the empty query fragment; i.e. it expands to nothing. *)

val lit : string -> t
(** [lit frag] expands to [frag], literally; i.e. the argument is passed
    unchanged to the database system as a substring of the query.
    Do not use this to inject untrusted data into the query string, since it can
    lead to an SQL injection vulnerability.
    Even when it can be done safely, it is probably easier and more portable to
    use the appropriate function from {!embeddingvalues} or the {!quote}
    function. *)

val quote : string -> t
(** [quote str] expands to the literally quoted string [str] if an reliable
    escape function is available from the driver library, otherwise [quote] is
    equivalent to {!string}. *)

val param : int -> t
(** [param i] expands to a reference to parameter number [i], counting from
    zero.  That is, [param 0] expands to ["$1"] for PostgreSQL and to ["?1"] for
    SQLite3.  For MariaDB, [param i] expands to ["?"] for any [i]; the driver
    will instead shuffle, elide, and duplicate the actual arguments to match
    their order of reference in the query string. *)

val var : string -> t
(** [var v] expands to [subst v] where [subst] is the substitution function
    passed to {!expand} or one of the connector functions. *)

val concat : ?sep: string -> t list -> t
(** [concat ?sep frags] concatenates [frags], optionally separated by [sep].
    Returns the empty fragment on the empty list of fragments. *)

val parens : t -> t
(** [parens frag] wraps [frag] in paranthesis *)

val cat : t -> t -> t
(** [cat q1 q2] expands to the juxtaposition of the expansions of [q1] followed
    by [q2].  This is an associative alternative to {!concat} when no separator
    is needed. *)

module Infix : sig
  (** This module provides a terser way to compose queries.  As an example,
      consider the dynamic construction of a simple SELECT-request which
      extracts a list of named columns given a corresponding row type, and where
      conditions are given as query templates with any values embedded:
      {[
        open Caqti_template.Create

        type cond =
          | Column_eq : string * 'a Caqti_template.Field_type.t * 'a -> cond

        let query_of_cond = function
         | Column_eq (col, t, v) ->
            Q.lit col @++ " = " ^++ Q.const t v

        let make_simple_select conds columns row_type =
          let query =
            "SELECT " ^++ Q.concat ~sep:", " (List.map Q.lit columns) @++
            " FROM $.foo" ^++
            " WHERE " ^++ Q.concat ~sep:" AND " (List.map query_of_cond conds)
          in
          direct_gen T.(unit -->* row_type) (fun _ -> query)
      ]}
      *)

  val (@++) : t -> t -> t
  (** An alias for {!cat}. *)

  val (^++) : string -> t -> t
  (** [pfx ^++ q] is [q] prefixed with the literal fragment [pfx], i.e.
      [cat (lit pfx) q]. *)

  val (++^) : t -> string -> t
  (** [q ++^ sfx] is [q] suffixed with the literal fragment [sfx], i.e.
      [cat q (lit sfx)]. *)
end

(** {3:embeddingvalues Embedding Values}

    The following functions can be used to embed values into a query, including
    the generic {!const}, corresponding specialized variants.  Additionally
    {!const_fields} can be used to extract fragments for multiple fields given a
    row type and a value. *)

val bool : bool -> t
val int : int -> t
val int16 : int -> t
val int32 : int32 -> t
val int64 : int64 -> t
val float : float -> t
val string : string -> t
val octets : string -> t
val pdate : Ptime.t -> t
val ptime : Ptime.t -> t
val ptime_span : Ptime.span -> t

val const : 'a Field_type.t -> 'a -> t
(** [const t x] is a fragment representing the value [x] of field type [t],
    using driver-dependent serialization and escaping mechanisms.
    Drivers will typically expand this to a parameter reference which will
    receive the value [x] when executed, though the value may also be embedded
    in the query if it is deemed safe. *)

val const_fields : 'a Row_type.t -> 'a -> t list
(** [const_fields t x] returns a list of fragments corresponding to the
    single-field projections of the value [x] as described by the type
    descriptor [t].  Each element of the returned list will be either a
    {!const}-fragment containing the projected value, or [lit "NULL"] if
    the projection is [None].

    The result can be turned into a comma-separated list with {!concat}, except
    values of unitary types, i.e. types having no fields, may require special
    care. *)


(** {2 Normalization and Equality} *)

val normal : t -> t
(** [normal q] rewrites [q] to a normal form, flattening nested concatenations
    and removing empty fragments from the internal representation.
    This function can be used to post-process queries before using {!equal} and
    {!hash}. *)

val equal : t -> t -> bool
(** [equal q1 q2] is true iff [q1] and [q2] has the same internal
    representation.
    It may be necessary to pre-process the query templates with {!normal} if
    they are not constructed by a common deterministic algorithm. *)

val hash : t -> int
(** [hash q] computes a hash over the internal representation of [q] which is
    compatible with {!equal}.
    The hash function may change across minor versions and may depend on
    architecture.
    It may be necessary to pre-process the query template with {!normal}, unless
    the hash is to be used among a collection of query templates constructed by
    a common deterministic algorithm. *)


(** {2 Parsing, Expansion, and Printing} *)

val pp : Format.formatter -> t -> unit
(** [pp ppf q] prints a {e human}-readable representation of [q] on [ppf].
    The printed string is {e not suitable for sending to an SQL database}; doing
    so may lead to an SQL injection vulnerability. *)

val show : t -> string
(** [show q] is the same {e human}-readable representation of [q] as printed by
    {!pp}.
    The returned string is {e not suitable for sending to an SQL database};
    doing so may lead to an SQL injection vulnerability. *)

module Expand_error : sig
  type t

  val pp : Format.formatter -> t -> unit
  (** Formats a human-readable error message. *)
end
(** A description of the error caused during {!expand} if the environment lookup
    function returns an invalid result or fails to provide a value for a
    variable when the expansion is final. *)

exception Expand_error of Expand_error.t
(** The exception raised by {!expand} when there are issues expanding an
    environment variable using the provided callback. *)

type subst = string -> t
(** A partial mapping from variable names to query fragments, which raises
    [Not_found] for undefined variables.  This is used by {!expand} to resolve
    variable references, with the special handling of a final period in the
    variable names described in {{!query_template} The Syntax of Query
    Templates}. *)

val expand : ?final: bool -> subst -> t -> t
(** [expand subst query] replaces the occurrence of each variable [var] with
    [subst var] where it is defined, otherwise if [final] is [false], the
    variable is left unchanged, otherwise raises {!Expand_error}.  The result of
    the substitution function may not contain variable references.

    @param final
      Whether this is the final expansion, as when invoked by the drivers.
      Defaults to [false].

    @raise Expand_error
      if the substitution function is invalid or if it is incomplete for a final
      expansion. *)

val angstrom_parser : t Angstrom.t
(** Matches a single expression terminated by the end of input or a semicolon
    lookahead. The accepted languages is described in {{!query_template} The
    Syntax of Query Templates}. *)

val angstrom_parser_with_semicolon : t Angstrom.t
(** A variant of [angstrom_parser] which accepts unquoted semicolons as part of
    the single statement, as is valid in some cases like in SQLite3 trigger
    definitions.  This is the parser used by {!Caqti_template.Request}, where
    it's assumed that the input is a single SQL statement. *)

val angstrom_list_parser : t list Angstrom.t
(** Matches a sequence of statements while ignoring surrounding white space and
    end-of-line comments starting with ["--"].  This parser can be used to load
    schema files with support for environment expansions, like substituting the
    name of the database schema. *)

module Parse_error : sig
  type t

  val position : t -> int
  (** The byte position of the string at which the parser failed. *)

  val message : t -> string
  (** A message describing the problem. *)

  val pp : Format.formatter -> t -> unit
  (** Formats a human-readable error message. *)
end
(** Describes errors from the high-level parsing functions. *)

exception Parse_error of Parse_error.t
(** The exception which may be raised by {!parse}. *)

val parse : string -> t
(** Parses a single expression using {!angstrom_parser_with_semicolon}.  The
    error indicates the byte position of the input string where the parse
    failure occurred in addition to an error message. See {{!query_template} The
    Syntax of Query Templates} for how the input string is interpreted.

    @raise Parse_error if the argument is syntactically invalid. *)

val parse_result : string -> (t, Parse_error.t) result
(** Variant of {!parse} which returns a result instead of raising.  *)
