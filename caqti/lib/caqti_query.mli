(* Copyright (C) 2019--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

include module type of (struct include Caqti_template.Query end)

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
