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

(** {b Internal:} Signature for driver implementation.

    This interface is unstable and may change between minor versions.  If you
    are developing an external driver, please open an issue to sort out
    requirements and to announce you need for a stable driver API. *)

module type System_common = sig

  type +'a future
  (** A concurrency monad with an optional failure monad, or just the identity
      type constructor for blocking operation. *)

  val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
  (** Bind operation of the concurrency monad. *)

  val (>|=) : 'a future -> ('a -> 'b) -> 'b future
  (** Map operation of the concurrency monad. *)

  val return : 'a -> 'a future
  (** Return operation of the  concurrency monad. *)

  val finally : (unit -> 'a future) -> (unit -> unit future) -> 'a future
  (** [finally f g] runs [f ()] and then runs [g ()] whether the former
      finished, failed with an exception, or failed with a monadic failure. *)

  val cleanup : (unit -> 'a future) -> (unit -> unit future) -> 'a future
  (** [cleanup f g] runs [f ()] and then runs [g ()] and re-raise the failure if
      and only if [f ()] failed with an exception or a monadic failure. *)

  val join : unit future list -> unit future
  (** [join ms] runs the elements of [ms] in parallel if supported by the
      concurrency implementation. *)

  module Mvar : sig
    type 'a t
    val create : unit -> 'a t
    val store : 'a -> 'a t -> unit
    val fetch : 'a t -> 'a future
  end

  module Log : sig
    type 'a log = ('a, unit future) Logs.msgf -> unit future
    val err : ?src: Logs.src -> 'a log
    val warn : ?src: Logs.src -> 'a log
    val info : ?src: Logs.src -> 'a log
    val debug : ?src: Logs.src -> 'a log
  end

  module Stream : Caqti_stream.S with type 'a future := 'a future
end

module type System_unix = sig
  include System_common

  module Unix : sig
    type file_descr
    val wrap_fd : (file_descr -> 'a future) -> Unix.file_descr -> 'a future
    val poll :
      ?read: bool -> ?write: bool -> ?timeout: float ->
      file_descr -> (bool * bool * bool) future
  end

  module Preemptive : sig
    val detach : ('a -> 'b) -> 'a -> 'b future
    val run_in_main : (unit -> 'a future) -> 'a
  end

end

module type S = sig
  type +'a future
  type (+'a, +'err) stream

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a future := 'a future
     and type ('a, 'err) stream := ('a, 'err) stream

  val driver_info : Caqti_driver_info.t

  val connect :
    ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
    Uri.t -> ((module CONNECTION), [> Caqti_error.connect]) result future
end

module type Of_system_unix =
  functor (System : System_unix) ->
  S with type 'a future := 'a System.future
     and type ('a, 'err) stream := ('a, 'err) System.Stream.t
