(* Copyright (C) 2019  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** Query specification. *)

type t =
  | L of string (** Literal code. May contain incomplete fragments. *)
  | P of int    (** [P i] refers to parameter number [i], counting from 0. *)
  | S of t list (** [S frags] is the concatenation of [frags]. *)
(** A representation of a query string to send to a database, abstracting over
    parameter references and providing nested concatenation to simplify
    generation.  For databases which only support linear parameters (typically
    denoted "[?]"), the driver will reshuffle, elide, and duplicate parameters
    as needed.  *)
[@@deriving eq]

val hash : t -> int
(** A hash function for {!query}, currently using {!Hashtbl.hash}. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf q] prints a human-readable representation of [q] on [ppf]. *)

val concat : string -> t list -> t
(** [concat sep frags] is [frags] interfixed with [sep] if [frags] is non-empty
    and the empty string of [frags] is empty. *)
