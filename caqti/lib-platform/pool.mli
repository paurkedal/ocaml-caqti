(* Copyright (C) 2014--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Internal resource pool implementation. *)

(** Scheduling taks in the future. *)
module type ALARM = sig
  type switch
  type stdenv

  type t
  (** A handle for cancelling the alarm if supported. *)

  val schedule :
    sw: switch ->
    stdenv: stdenv ->
    Mtime.t -> (unit -> unit) -> t
  (** If supported, [schedule ~sw ~stdenv time f] schedules [f] to be run
      at [time] and returns a handle which can be used to {!unschedule} it.  The
      caqti-blocking implementation does nothing.  The pool implementation using
      it makes additional opportunistic calls to the handler.  This function
      must insert a yield before running the function even if the delay is
      non-positive. *)

  val unschedule : t -> unit
  (** Cancels the alarm if supported. This is only used for early clean-up, so
      the implementation may choose to let it time out instead. *)
end

module type S = sig
  type switch
  type stdenv

  include Caqti_pool_sig.S

  val create :
    ?config: Caqti_pool_config.t ->
    ?check: ('a -> (bool -> unit) -> unit) ->
    ?validate: ('a -> bool fiber) ->
    ?log_src: Logs.Src.t ->
    sw: switch ->
    stdenv: stdenv ->
    (unit -> ('a, 'e) result fiber) -> ('a -> unit fiber) ->
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

module Make
  (System : System_sig.CORE)
  (_ : ALARM
    with type switch := System.Switch.t
     and type stdenv := System.stdenv) :
  S with type 'a fiber := 'a System.Fiber.t
     and type switch := System.Switch.t
     and type stdenv := System.stdenv

module Make_without_alarm (System : System_sig.CORE) :
  S with type 'a fiber := 'a System.Fiber.t
     and type switch := System.Switch.t
     and type stdenv := System.stdenv
