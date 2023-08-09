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

type _ key
type t

(** {2 Construction and Generic Operations} *)

val create :
  ?max_size: int ->
  ?max_idle_size: int ->
  ?max_idle_age: Mtime.Span.t option ->
  ?max_use_count: int option ->
  unit -> t
(** Creates a configuration for the implementation of {!Caqti_pool_sig.S}.  The
    main arguments populate the configuration with the corresponding settings,
    which are explaind in {!cpck}. *)

val default : t
(** The configuration object with no setting, which gives the built-in defaults.
    Alternatively, use {!default_from_env} for a configuration based on
    environment variables. *)

val default_from_env : unit -> t
(** [default_from_env ()] is a configuration constructed from environment
    variables of the form [CAQTI_POOL_<suffix>], with the upper-cased names of
    the configuration keys substituted for [<suffix>]. *)

val merge_left : t -> t -> t
(** [merge_left cL cR] is the configuration [cL] with missing settings populated
    by the corresponding present settings from [cR]. *)

val get : 'a key -> t -> 'a option
(** [get key config] is the value of [key] in [config] or [None] if unset. *)

val set : 'a key -> 'a -> t -> t
(** [set key value config] is [config] with [key] set to [value] regardless of
    any previous mapping. *)

val unset : 'a key -> t -> t
(** [unset key config] is [config] without its mapping for [key] if any. *)


(** {2:cpck Configuration Keys} *)

val max_size : int key
(** The maximum number of open connections associated with the pool.  When this
    limit is hit, an attempt to use the pool will block until a connection
    becomes available.  The value must be at least one.  If the selected driver
    does not support concurrent connections, the value [1] is assumed. *)

val max_idle_size : int key
(** The maximum number of idle connections to put into the pool for reuse.  Must
    be between [0] and {!max_size}.  If you set this, you must also ensure
    {!max_size} is set so that the combination is valid.  Defaults to
    {!max_size}.  For drivers which does not support concurrent connections, the
    value will clipped to a maximum of [1]. *)

val max_idle_age : Mtime.Span.t option key
(** The maximum age of idle connections before they are scheduled to be
    disconnected and removed from the pool, or [None] for no limit.  Where
    possible, a timer will be used to trigger the cleanup.  For the
    [caqti.blocking] library, the cleanup will only be done opportunistically
    when the pool is used. *)

val max_use_count : int option key
(** The maximum number of times a pooled connection is reused, or [None] for no
    limit.  The default is currently 100, but may be changed in the future based
    on real-world experience.  The reason this setting was introduced is that we
    have seen state being retained on the server side. *)
