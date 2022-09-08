(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signature for establishing database connections. *)

module type S_without_connect = sig

  type +'a future
  (** The type of a deferred value of type ['a]. *)

  module Pool : Caqti_pool_sig.S with type 'a future := 'a future
  (** A pool implementation for the current concurrency library. *)

  module Stream : Caqti_stream.S with type 'a future := 'a future

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a future := 'a future
     and type ('a, 'err) stream := ('a, 'err) Stream.t
  (** The connection API specialized for the current concurrency library. *)

  type connection = (module CONNECTION)
  (** Shortcut for the connection API passed as a value. *)

end

module type Connect = sig
  type +'a future
  type connection
  type ('a, +'e) pool
  type +'a connect_fun

  val connect :
    (connection, [> Caqti_error.load_or_connect]) result future connect_fun
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
    ((connection ->
      ('a, [> Caqti_error.load_or_connect] as 'e) result future) ->
     ('a, 'e) result future) connect_fun
  (** [with_connection uri f] calls {!connect} on [uri]. If {!connect} evaluates
      to [Ok connection], [with_connection] passes the connection to [f]. Once
      [f] either evaluates to a [result], or raises an exception,
      [with_connection] closes the database connection.

      @param tweaks_version Passed to {!connect}.
      @param env Passed to {!connect}. *)

  val connect_pool :
    ?max_size: int -> ?max_idle_size: int -> ?max_use_count: int option ->
    ?post_connect: (connection -> (unit, 'connect_error) result future) ->
    ((connection, [> Caqti_error.connect] as 'connect_error) pool,
     [> Caqti_error.load]) result connect_fun
  (** [connect_pool uri] is a pool of database connections constructed by
      [connect uri].

      Do not use pooling for connections to volatile resources like
      [sqlite3::memory:] and beware of temporary tables or other objects which
      may not be shared across connections to the same URI.

      If you use preemptive threading, note that the connection pool must only
      be used from the thread where it was created. Use thread local storage to
      create a separate pool per thread if necessary.

      @param tweaks_version
        Passed to {!connect} when creating new connections.

      @param env
        Passed to {!connect} when creating new connections.

      @param max_size
        The maximum number of open connections. Must be at least [1].
        For drivers which does not support concurrent connections, this will be
        ignored and the value [1] used instead.

      @param max_idle_size
        The maximum number of idle connections to put into the pool for reuse.
        Defaults to [max_size]. Must be between 0 and [max_size]. If you set
        this, you must also set [max_size].
        For drivers which does not support pooling, this will be ignored and the
        value [0] used instead. For drivers which does not support concurrent
        connections, but supports pooling, the value will clipped to a maximum
        of [1].

      @param max_use_count
        The maximum number of times to use a connection before dropping it from
        the pool, or [None] for no limit.  The default is currently 100, but may
        be changed in the future based on real-world experience.  The reason
        this setting was introduced is that we have seen state being retained on
        the server side. *)
end

module type S = sig
  include S_without_connect
  include Connect
    with type 'a future := 'a future
     and type connection := connection
     and type ('a, 'e) pool := ('a, 'e) Pool.t
     and type 'a connect_fun :=
      ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
      ?tweaks_version: int * int ->
      Uri.t -> 'a
end
