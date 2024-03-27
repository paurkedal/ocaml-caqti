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

(** Opaque version with comparison.

    This version number module is intended for dispatching on details about an
    SQL intepreter or other relevant aspects of a relational database system.
    The only constructor is private, instead the {!Infix} module provides direct
    comparison to strings for ideomatic usage in the query-returning callback of
    request templates.

    Note that a version can be undefined, in which case it will compare like the
    empty string. *)

type t

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int
val equal : t -> t -> bool

module Infix : sig
  val ( =* ) : t -> string -> bool
  val ( <>* ) : t -> string -> bool
  val ( <* ) : t -> string -> bool
  val ( <=* ) : t -> string -> bool
  val ( >* ) : t -> string -> bool
  val ( >=* ) : t -> string -> bool
end

(**/**)
val of_string_unsafe : string -> t
[@@alert caqti_private "For use by Caqti drivers."]
