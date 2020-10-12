(* Copyright (C) 2017--2020  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signature for establishing database connections. *)

module type S = sig

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

  val connect : Uri.t ->
    (connection, [> Caqti_error.load_or_connect]) result future
  (** [connect uri] locates and loads a driver which can handle [uri], passes
      [uri] to the driver, which establish a connection and returns a
      first-class module implementing {!Caqti_connection_sig.S}.

      If you use preemptive threading, note that the connection must only be
      used from the thread where it was created. *)

  val connect_pool : ?max_size: int -> Uri.t ->
    ((connection, [> Caqti_error.connect]) Pool.t, [> Caqti_error.load]) result
  (** [connect_pool uri] is a pool of database connections constructed by
      [connect uri].

      If you use preemptive threading, note that the connection pool must only
      be used from the thread where it was created. Use thread local storage to
      create a separate pool per thread if necessary. *)
end
