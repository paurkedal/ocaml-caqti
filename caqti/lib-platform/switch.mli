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

module type FUTURE = sig
  type 'a future

  val return : 'a -> 'a future
  val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
  val finally : (unit -> 'a future) -> (unit -> unit future) -> 'a future
end

module type S = sig
  type 'a future
  type t
  type hook

  exception Off

  val eternal : t
  (** A switch which is never released. *)

  val create : unit -> t
  (** Create a fresh releasable switch which is initially on. *)

  val release : t -> unit future
  (** [release sw] calls all cleanup handlers on [sw] in reverse order of
      registration and marks the switch as being off. *)

  val run : (t -> 'a future) -> 'a future
  (** [run f] calls [f] with a fresh switch which will be released upon exit or
      in case of failure. *)

  val check : t -> unit
  (** [check sw] raises [Off] if [sw] has been turned off. *)

  val on_release_cancellable : t -> (unit -> unit future) -> hook
  (** [on_release_cancellable sw f] registers [f] to be called upon the evetual
      release of [sw] unless {!remove_hook} is called on the returned hook
      before that happen. *)

  val remove_hook : hook -> unit
  (** Given a [hook] returned by {!on_release_cancellable}, [remove_hook hook]
      cancels the cleanup registered by that call. *)
end

module Make (Future : FUTURE) : S with type 'a future := 'a Future.future
