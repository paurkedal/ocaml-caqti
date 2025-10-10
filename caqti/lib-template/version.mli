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

(** Opaque version strings with comparison.

    This version number module is intended for dispatching on details about an
    SQL intepreter or other relevant aspects of a relational database system.
    The only constructor is private, instead the {!Infix} module provides direct
    comparison to strings for ideomatic usage in the query-returning callback of
    request templates.

    Some care is needed to match ranges of versions correctly:

      - A version provided by {!Dialect} may by unknown.  An unknown version
        compares like the empty string, meaning that it compares before any
        other (sensible) version.  If another logic is desired, {!is_known} can
        be used to distinugish it.

      - A version provided by {!Dialect} may also include build numbers,
        distribution details, etc.  Therefore it seldom makes sense to compare
        it agains the exact version of the software release.  Such versions will
        instead compare between the version of the corresponding software
        release and the next possible version using a normal decimal-dotted
        versioning scheme. *)

type t

val is_known : t -> bool
(** Tests whether a version from the database server is available. *)

val compare : t -> t -> int
(** Compares two versions lexicographically group-by-group and
    component-by-component, using common conventions for comparing version
    numbers:

      - Groups of components are separated by ['-'].
      - Components are separated by ['.'].
      - Continuous sequences of digits are compared numerically, i.e. after
        skipping leading zeros, longer such sequences compare after shorter
        ones.
      - ['~'] compares before anything else, including the empty suffix.

    The ordering subject to change if we should encounter problematic
    differences with versioning schemes used by supported database systems. *)

val equal : t -> t -> bool
(** [equal x y] is equivalent to [compare x y = 0]. *)

module Infix : sig
  val ( =* ) : t -> string -> bool
  val ( <>* ) : t -> string -> bool
  val ( <* ) : t -> string -> bool
  val ( <=* ) : t -> string -> bool
  val ( >* ) : t -> string -> bool
  val ( >=* ) : t -> string -> bool
end
(** Asymmetric infix oparator for testing version ranges.  The first argument is
    a version number, typically obtained from the [server_version] fields of
    {!Dialect.t}, and the second argument is a string representation of the
    version to compare against.

    An unknown version compares before other versions.  Use {!Version.is_known}
    to implement a different logic.  See {!Version.compare} for details about
    the comparison algorithm. *)

val pp : Format.formatter -> t -> unit

(**/**)
val of_string_unsafe : string -> t
[@@alert caqti_private "For use by Caqti drivers."]
