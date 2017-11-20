(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(** (internal) Helper for loading plugins. *)

exception Plugin_missing of string * string
(** [Plugin_missing pkgs] if {!ensure_plugin} fails to find any of the findlib
    packages [pkgs]. *)

exception Plugin_invalid of string * string
(** [Plugin_invalid (pkg, reason)] is called by {!ensure_plugin} if something
    detailed in [reason] went wrong when loading the findlib package {!pkg}. *)

val ensure_plugin : (unit -> 'a option) -> string -> 'a
(** [ensure_plugin f pkg] returns [x] if [f ()] is [Some x].  Otherwise, it
    loads the findlib package [pkg] and calls [f ()] again to verify that the
    expected functionality has been provided, returning the result which is now
    expected to be non-[None].

    {b Note!} This function is meant for internal use.

    @raise Plugin_missing if a second call to [f ()] returns [None].
    @raise Plugin_invalid if something goes wrong while loading a package. *)
