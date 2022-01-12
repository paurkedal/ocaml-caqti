(* Copyright (C) 2019--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Query specification. *)

type t =
  | L of string (** Literal code. May contain incomplete fragments. *)
  | Q of string (** [Q s] corresponds to a [TEXT] literal; passed as part of the
                    query string if a suitable quoting function is available in
                    the client library, otherwise passed as an additional
                    parameter. *)
  | P of int    (** [P i] refers to parameter number [i], counting from 0. *)
  | E of string (** [E name] is expanded by the environemnt lookup function. *)
  | S of t list (** [S frags] is the concatenation of [frags]. *)
(** A representation of a query string to send to a database, abstracting over
    parameter references and providing nested concatenation to simplify
    generation.  For databases which only support linear parameters (typically
    denoted "[?]"), the driver will reshuffle, elide, and duplicate parameters
    as needed.  *)

val normal : t -> t
(** [normal q] rewrites [q] to a normal form containing at most one top-level
    {!S} constructor, containing no empty literals, and no consecutive literals.
    This function can be used to post-process queries before using {!equal} and
    {!hash}. *)

val equal : t -> t -> bool
(** Equality predicate for {!query}. *)

val hash : t -> int
(** A hash function compatible with {!equal}.  This is currently
    {!Hashtbl.hash}. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf q] prints a {e human}-readable representation of [q] on [ppf].
    The printed string is {e not suitable for sending to an SQL database}; doing
    so may lead to an SQL injection vulnerability. *)

val show : t -> string
(** [show q] is the same {e human}-readable representation of [q] as printed by
    {!pp}.
    The returned string is {e not suitable for sending to an SQL database};
    doing so may lead to an SQL injection vulnerability. *)

val concat : string -> t list -> t
(** [concat sep frags] is [frags] interfixed with [sep] if [frags] is non-empty
    and the empty string of [frags] is empty. *)

val expand : (string -> t) -> t -> t
(** [expand f q] replaces each occurrence of [E v] some some [v] with [f v] or
    leaves it unchanged where [f v] raises [Not_found]. *)

val angstrom_parser : t Angstrom.t
(** Matches a single expression terminated by the end of input or a semicolon
    lookahead. The accepted languages is described in {{!query_template} The
    Syntax of Query Templates}. *)

val of_string : string -> (t, [`Invalid of int * string]) result
(** Parses a single expression without semicolon.  The error indicates the byte
    position of the input string where the parse failure occurred in addition to
    an error message. See {{!query_template} The Syntax
    of Query Templates} for how the input string is interpreted. *)

val of_string_exn : string -> t
(** Parses the same strings as {!of_string}.

    @raise Failure if parsing failed. *)
