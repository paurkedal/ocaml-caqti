(* Copyright (C) 2017--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signatures providing functions for establishing database connections. *)

module type S = sig

  type +'a fiber
  (** The type of a deferred value of type ['a]. *)

  type +'a with_switch
  (** Adds a switch argument to the type if relevant for the platform. *)

  type +'a with_stdenv
  (** Adds environment argument(s) to the type if relevant for the platform. *)

  type (+'a, +'e) stream
  (** A stream implementation. *)

  type ('a, +'e) pool
  (** A pool implementation for the current concurrency library. *)

  type connection
  (** Shortcut for the connection API passed as a value. *)

  val connect :
    ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
    ?tweaks_version: int * int ->
    (Uri.t -> (connection, [> Caqti_error.load_or_connect]) result fiber)
    with_stdenv with_switch
  (** [connect uri] locates and loads a driver which can handle [uri], passes
      [uri] to the driver, which establish a connection and returns a
      first-class module implementing {!Caqti_connection_sig.S}.

      If you use preemptive threading, note that the connection must only be
      used from the thread where it was created.

      See {{!tweaks} Database Tweaks} for details about the [tweaks_version]
      parameter.

      @param tweaks_version
        Declares compatibility with database tweaks introduced up to the given
        version of Caqti.  Defaults to a conservative value.  See the above
        reference for more info (not linked here due to odoc issue).

      @param env
        If provided, this function will do a final expansion of environment
        variables which occurs in the query templates of the requests executed
        on the connection. *)

  val with_connection :
    ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
    ?tweaks_version: int * int ->
    (Uri.t ->
     (connection ->
      ('a, [> Caqti_error.load_or_connect] as 'e) result fiber) ->
     ('a, 'e) result fiber)
    with_stdenv
  (** [with_connection uri f] calls {!connect} on [uri]. If {!connect} evaluates
      to [Ok connection], [with_connection] passes the connection to [f]. Once
      [f] either evaluates to a [result], or raises an exception,
      [with_connection] closes the database connection.

      @param tweaks_version Passed to {!connect}.
      @param env Passed to {!connect}. *)

  val connect_pool :
    ?pool_config: Caqti_pool_config.t ->
    ?post_connect: (connection -> (unit, 'connect_error) result fiber) ->
    ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
    ?tweaks_version: int * int ->
    (Uri.t ->
     ((connection, [> Caqti_error.connect] as 'connect_error) pool,
      [> Caqti_error.load]) result)
    with_stdenv with_switch
  (** [connect_pool uri] is a pool of database connections constructed by
      [connect uri].

      Do not use pooling for connections to volatile resources like
      [sqlite3::memory:] and beware of temporary tables or other objects which
      may not be shared across connections to the same URI.

      If you use preemptive threading, note that the connection pool must only
      be used from the thread where it was created. Use thread local storage to
      create a separate pool per thread if necessary.

      @param pool_config
        Provides tuning parameters for the pool.  The default is the result of a
        fresh call of {!Caqti_pool_config.default_from_env}.

      @param tweaks_version
        Passed to {!connect} when creating new connections.

      @param env
        Passed to {!connect} when creating new connections. *)
end
