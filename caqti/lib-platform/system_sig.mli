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

module type SEQUENCER = sig
  type 'a t
  type 'a future
  val create : 'a -> 'a t
  val enqueue : 'a t -> ('a -> 'b future) -> 'b future
end

module type CORE = sig

  type +'a future
  (** A concurrency monad with an optional failure monad, or just the identity
      type constructor for blocking operation. *)

  type connect_env
  (** Type of an extra argument to connect functions used to pass through the
      network stack in Mirage and stdenv and switch in EIO.  This is eliminated
      at the service API where not needed. *)

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

  val async : connect_env: connect_env -> (unit -> unit future) -> unit
  (** [async f] runs [f ()] asynchroneously if possible, else immediately. *)

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

  module Stream : Caqti_stream_sig.S with type 'a future := 'a future

  module Sequencer : SEQUENCER with type 'a future := 'a future

end

module type S = sig
  include CORE

  module Net : sig

    module Sockaddr : sig
      type t
      val unix : string -> t
      val tcp : Ipaddr.t * int -> t
    end

    type in_channel
    type out_channel

    val getaddrinfo :
      connect_env: connect_env -> [`host] Domain_name.t -> int ->
      (Sockaddr.t list, [> `Msg of string]) result future
    (** This should be a specialized version of getaddrinfo, which only returns
        entries which is expected to work with the corresponding connect on the
        platform implementing this interface.  In particular:

          - The family can be IPv4 or IPv6, where supported, and this must be
            encoded in the {!Sockaddr.t}.
          - The socket type is restricted to STREAM.
          - The protocol is assumed to be selected automatically from address
            family, given the socket type restriction.

        All returned values are TCP destinations.  If a distinction can be made,
        an empty list indicates that the address has no DNS entries, while an
        error return indicates that an appropriate DNS server could not be
        queried. *)

    val connect :
      connect_env: connect_env -> Sockaddr.t ->
      (in_channel * out_channel, [> `Msg of string]) result future

    (* TODO: STARTTLS *)

    (* These are currently only used by PGX.  Despite the flush, it PGX is doing
     * it's own buffering, so unbuffered should be okay.  output_char and
     * input_char are only used for the packet header. *)
    val output_char : out_channel -> char -> unit future
    val output_string : out_channel -> string -> unit future
    val flush : out_channel -> unit future
    val input_char : in_channel -> char future
    val really_input : in_channel -> Bytes.t -> int -> int -> unit future
    val close_in : in_channel -> unit future
  end
end
