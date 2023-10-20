(* Copyright (C) 2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Configuration passed to connect functions. *)

type _ key

type t

(** {2 Construction and Lookup} *)

val default : t
(** The configuration with all keys set to their default values. *)

val get : 'a key -> t -> 'a
(** [get key cfg] is the value associated with [key] in [cfg], which may be a
    default value if the key has not been explicitly {!set} or if it has been
    {!reset}. *)

val set : 'a key -> 'a -> t -> t
(** [set key value cfg] associates [key] with [value] in [cfg]. *)

val reset : 'a key -> t -> t
(** [reset key cfg] associates [key] with its default value in [cfg]. *)

(** {2 Configuration Keys} *)

val tweaks_version : (int * int) key
(** Declares compatibility with {{!tweaks} database tweaks} introduced up to the
    given version of Caqti.  Defaults to a conservative value. *)

(**/**) (* for internal use *)
val create_key : string -> 'a -> 'a key
