(* Copyright (C) 2019--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Intermediate query string representation.

    This module provides a common representation of database query strings.
    This module can be used directly to construct queries dynamically, or
    indirectly via the parser, as may be more convenient when the query string
    is known at compile-time.  In the latter case, the input string is typically
    very similar to the output string.  In either case the intermediate
    representation serve to unify the syntax across database systems and to
    provide additional functionality.

    When using this module directly, it provides:

      - flexible, pure, and efficient construction ({!S}, {!L}),
      - uniform index-based parameter references ({!P}),
      - expansion of fragments provided by an environment function ({!E}), and
      - safe embedding of values in queries ({!V}, {!Q}). *)


(** {2 Construction} *)

type t =
  | L of string
    (** [L frag] translates to the literally inserted substring [frag].  The
        [frag] argument must be trusted or verified to be secure to avoid SQL
        injection attacks.  Use {!V}, {!Q}, or {!P} to safely insert strings or
        other values. *)
  | V : 'a Caqti_type.Field.t * 'a -> t
    (** [V (t, v)] translates to a parameter of type [t] bound to the value [v].
        That is, the query string will contain a parameter reference which does
        not conflict with any {!P} nodes and bind [v] to the corresponding
        parameter each time the query is executed.  This allows taking advantage
        of driver-dependent serialization and escaping mechanisms to safely send
        values to the database server. *)
  | Q of string
    (** [Q s] corresponds to a quoted string literal.  This is passed as part of
        the query string if a suitable quoting function is available in the
        client library, otherwise it is equivalent to
        {!V}[(]{!Caqti_type.Field.String}[, s)]. *)
  | P of int
    (** [P i] refers to parameter number [i], counting from 0, so that e.g.
        [P 0] translates to ["$1"] for PostgreSQL and ["?1"] for SQLite3. *)
  | E of string
    (** [E name] will be replaced by the fragment returned by an environment
        lookup function, as passed directly to {!expand} or indirectly through
        the [?env] argument found in higher-level functions.  An error will be
        issued for any remaining [E]-nodes in the final translation to a query
        string. *)
  | S of t list
    (** [S frags] is the concatenation of [frags].  Apart from combining
        different kinds of nodes, this constructor can be nested according to
        the flow of the generating code. *)
(** [t] is an intermediate representation of a query string to be send to a
    database, possibly combined with some hidden parameters used to safely embed
    values.  Apart from embedding values, this representation provides indexed
    parameter references, independent of the target database system.  For
    databases which use linear parameter references (like [?] for MariaDB), the
    driver will reshuffle, elide, and duplicate parameters as needed.

    Please note that additional constructors may be added to this type across
    minor releases. *)

val concat : string -> t list -> t
(** [concat sep frags] is [frags] interfixed with [sep] if [frags] is non-empty,
    and the empty string if [frags] is empty. *)

(** {3 Embedding Values}

    The following are shortcuts for combining {!V} with some of the field types.
    The values will be passed as hidden parameters. *)

val bool : bool -> t
val int : int -> t
val float : float -> t
val string : string -> t
val octets : string -> t
val pdate : Ptime.t -> t
val ptime : Ptime.t -> t
val ptime_span : Ptime.span -> t


(** {2 Normalization and Equality} *)

val normal : t -> t
(** [normal q] rewrites [q] to a normal form containing at most one top-level
    {!S} constructor, containing no empty literals, and no consecutive literals.
    This function can be used to post-process queries before using {!equal} and
    {!hash}. *)

val equal : t -> t -> bool
(** Equality predicate for {!t}. *)

val hash : t -> int
(** A hash function compatible with {!equal}.  The hash function may change
    across minor versions and may depend on architecture. *)


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

type expand_error
(** A description of the error caused during {!expand} if the environment lookup
    function returns an invalid result or raises [Not_found] for a variable when
    the expansion is final. *)

val pp_expand_error : Format.formatter -> expand_error -> unit
(** Prints an informative error. *)

exception Expand_error of expand_error
(** The exception raised by {!expand} when there are issues expanding an
    environment variable using the provided callback. *)

val expand : ?final: bool -> (string -> t) -> t -> t
(** [expand f q] replaces each occurrence of [E v] some some [v] with [f v] or
    leaves it unchanged where [f v] raises [Not_found]. The [Not_found]
    exception will not escape this call.

    @param final
      If [true], then an error is raised instead of leaving environment
      references unexpended if [f] raises [Not_found].  This is used by drivers
      for performing the final expansion.  Defaults to [false].

    @raise Expand_error
      if [~final:true] is passed and [f] raise [Not_found] or if [f] returns a
      query containing environment references. *)

val angstrom_parser : t Angstrom.t
(** Matches a single expression terminated by the end of input or a semicolon
    lookahead. The accepted languages is described in {{!query_template} The
    Syntax of Query Templates}. *)

val angstrom_parser_with_semicolon : t Angstrom.t
(** A variant of [angstrom_parser] which accepts unquoted semicolons as part of
    the single statement, as is valid in some cases like in SQLite3 trigger
    definitions.  This is the parser used by {!Caqti_request}, where it's
    assumed that the input is a single SQL statement. *)

val of_string : string -> (t, [`Invalid of int * string]) result
(** Parses a single expression using {!angstrom_parser_with_semicolon}.  The
    error indicates the byte position of the input string where the parse
    failure occurred in addition to an error message. See {{!query_template} The
    Syntax of Query Templates} for how the input string is interpreted. *)

val of_string_exn : string -> t
(** Like {!of_string}, but raises an exception on error.

    @raise Failure if parsing failed. *)

(**/**)
val qprintf : ('a, Format.formatter, unit, t) format4 -> 'a
[@@alert deprecated "Moved to Caqti_query_fmt."]
val kqprintf : (t -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
[@@alert deprecated "Moved to Caqti_query_fmt."]
val param : Format.formatter -> int -> unit
[@@alert deprecated "Moved to Caqti_query_fmt."]
val env : Format.formatter -> string -> unit
[@@alert deprecated "Moved to Caqti_query_fmt."]
val quote : Format.formatter -> string -> unit
[@@alert deprecated "Moved to Caqti_query_fmt."]
val query : Format.formatter -> t -> unit
[@@alert deprecated "Moved to Caqti_query_fmt."]
