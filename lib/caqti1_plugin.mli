(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** (v1) Helper for loading plugins.
    @deprecated Don't use this in new code. *)

[@@@ocaml.deprecated]

exception Plugin_missing of string * string
(** [Plugin_missing pkgs] if {!ensure_plugin} fails to find any of the findlib
    packages [pkgs]. *)

exception Plugin_invalid of string * string
(** [Plugin_invalid (pkg, reason)] is called by {!ensure_plugin} if something
    detailed in [reason] went wrong when loading the findlib package {!pkg}. *)

(**/**)

val ensure_plugin : (unit -> 'a option) -> string -> 'a
[@@ocaml.deprecated]
