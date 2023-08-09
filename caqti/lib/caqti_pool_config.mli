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

(** Pool configuration. *)

type t

(** {2 Construction, Defaults, and Merge} *)

val create :
  ?max_size: int ->
  ?max_idle_size: int ->
  ?max_idle_age: Mtime.Span.t option ->
  ?max_use_count: int option ->
  unit -> t
(** Creates a configuration for the implementation of {!Caqti_pool_sig.S}.

    @param max_size
      The maximum number of resources controlled by the pool at any time,
      including currently used ones.
    @param max_idle_size
      The maximum number of unused resources kept in the pool.
    @param max_idle_age
      The maximum time a resource is kept idle in the pool before it is removed
      or [None] for no limit, which is the default.
    @param max_use_count
      The maximum number of times a resource is re-used before being discarded.
 *)

val create_from_env : string -> t
(** [create_from_env prefix] creates a configuration from environment variables
    starting with the given prefix.  See also {!default_from_env}. *)

val default : t
(** The configuration object with no setting, which gives the built-in defaults.
    Alternatively, use {!default_from_env} for a configuration based on
    environment variables. *)

val default_from_env : unit -> t
(** [default_from_env ()] is a configuration constructed from environment
    variables of the form [CAQTI_POOL_<suffix>], with the upper-cased names of
    arguments of {!create} substituted for [<suffix>].  It is equivalent to
    {!create_from_env}["CAQTI_POOL"]. *)

val merge_left : t -> t -> t
(** [merge_left cL cR] is the configuration [cL] with missing settings populated
    by the corresponding present settings from [cR]. *)

(** {2 Individual Parameters} *)

type _ key

val get : 'a key -> t -> 'a option
val set : 'a key -> 'a -> t -> t
val unset : 'a key -> t -> t

val max_size : int key
val max_idle_size : int key
val max_idle_age : Mtime.Span.t option key
val max_use_count : int option key
