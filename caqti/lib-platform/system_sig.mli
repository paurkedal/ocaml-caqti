(* Copyright (C) 2022--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signature for concurrency model and OS-calls.

    This is the common part of the signature declarning system dependencies.
    Driver will usually require an extended signature, as provided by

      - {!Caqti_platform_unix.System_sig.S} for drivers which depend on the unix
        library
      - {!Caqti_platform_net.System_sig.S} for drivers which can manage with a
        network stack implemented in pure OCaml
  *)

module type CORE = sig

  type +'a future
  (** A concurrency monad with an optional failure monad, or just the identity
      type constructor for blocking operation. *)

  val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
  (** Bind operation of the concurrency monad. *)

  val (>|=) : 'a future -> ('a -> 'b) -> 'b future
  (** Map operation of the concurrency monad. *)

  val return : 'a -> 'a future
  (** Return operation of the  concurrency monad. *)

  val catch : (unit -> 'a future) -> (exn -> 'a future) -> 'a future

  val finally : (unit -> 'a future) -> (unit -> unit future) -> 'a future
  (** [finally f g] runs [f ()] and then runs [g ()] whether the former
      finished, failed with an exception, or failed with a monadic failure. *)

  val cleanup : (unit -> 'a future) -> (unit -> unit future) -> 'a future
  (** [cleanup f g] runs [f ()] and then runs [g ()] and re-raise the failure if
      and only if [f ()] failed with an exception or a monadic failure. *)

  module Semaphore : sig
    type t
    val create : unit -> t
    val release : t -> unit
    val acquire : t -> unit future
  end

  module Log : sig
    type 'a log = ('a, unit future) Logs.msgf -> unit future
    val err : ?src: Logs.src -> 'a log
    val warn : ?src: Logs.src -> 'a log
    val info : ?src: Logs.src -> 'a log
    val debug : ?src: Logs.src -> 'a log
  end

  type connect_env
  (** Type of an extra argument to connect functions used to pass through the
      network stack in Mirage and stdenv and switch in EIO.  This is eliminated
      at the service API where not needed. *)

  val async : connect_env: connect_env -> (unit -> unit future) -> unit
  (** [async f] runs [f ()] asynchroneously if possible, else immediately. *)
end

(** Scheduling taks in the future. *)
module type ALARM = sig
  type t
  (** A handle for cancelling the alarm if supported. *)

  type connect_env

  val schedule : connect_env: connect_env -> Mtime.t -> (unit -> unit) -> t
  (** This schedules the alarm if supported. The caqti-blocking implementation
      does nothing. The pool implementation using it makes additional
      opportunistic calls to the handler. *)

  val unschedule : t -> unit
  (** Cancels the alarm if supported. This is only used for early clean-up, so
      the implementation may choose to let it time out instead. *)
end

module type POOL = sig
  include Caqti_pool_sig.S

  type connect_env

  val create :
    ?max_size: int ->
    ?max_idle_size: int ->
    ?max_idle_age: Mtime.Span.t ->
    ?max_use_count: int option ->
    ?check: ('a -> (bool -> unit) -> unit) ->
    ?validate: ('a -> bool future) ->
    ?log_src: Logs.Src.t ->
    connect_env: connect_env ->
    (unit -> ('a, 'e) result future) -> ('a -> unit future) ->
    ('a, 'e) t
  (** {b Internal:} [create alloc free] is a pool of resources allocated by
      [alloc] and freed by [free]. This is primarily intended for implementing
      the [connect_pool] functions.

      @param max_size
        The maximum number of allocated resources.

      @param max_idle_size
        The maximum number of resources to pool for later use. Defaults to
        [max_size].

      @param max_use_count
        The maximum number of times to use a connection, or [None] for no limit.

      @param check
        A function used to check a resource after use.

      @param validate
        A function to check before use that a resource is still valid. *)
end

(** The full shared system signature, as needed by {!Make_connect}. *)
module type S = sig
  include CORE

  module Stream : Caqti_stream_sig.S with type 'a future := 'a future

  module Pool : POOL
    with type 'a future := 'a future and type connect_env := connect_env
end
