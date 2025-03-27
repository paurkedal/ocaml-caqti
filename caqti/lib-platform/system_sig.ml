(* Copyright (C) 2022--2025  Petter A. Urkedal <paurkedal@gmail.com>
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
    Driver which depend on features from the unix library will also need
    {!Caqti_platform_unix.System_sig.S}.
  *)

module type FIBER = sig

  type +'a t
  (** A concurrency monad with an optional failure monad, or just the identity
      type constructor for blocking operation. *)

  module Infix : sig

    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    (** Bind operation of the concurrency monad. *)

    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    (** Map operation of the concurrency monad. *)

  end

  val return : 'a -> 'a t
  (** Return operation of the  concurrency monad. *)

  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

  val finally : (unit -> 'a t) -> (unit -> unit t) -> 'a t
  (** [finally f g] runs [f ()] and then runs [g ()] whether the former
      finished, failed with an exception, or failed with a monadic failure. *)

  val cleanup : (unit -> 'a t) -> (unit -> unit t) -> 'a t
  (** [cleanup f g] runs [f ()] and then runs [g ()] and re-raise the failure if
      and only if [f ()] failed with an exception or a monadic failure. *)
end

module type SEQUENCER = sig
  type 'a fiber
  type 'a t
  val create : 'a -> 'a t
  val enqueue : 'a t -> ('a -> 'b fiber) -> 'b fiber
end

module type CORE = sig
  module Fiber : FIBER

  type stdenv
  (** Type of an extra argument to connect functions used to pass through the
      network stack in Mirage and stdenv in EIO.  This is eliminated at the
      service API where not needed. *)

  (** A module used by EIO to handle cleanup tasks; unit for other platforms. *)
  module Switch : sig
    type t
    type hook
    val run : (t -> 'a Fiber.t) -> 'a Fiber.t
    val check : t -> unit
    val on_release_cancellable : t -> (unit -> unit Fiber.t) -> hook
    val remove_hook : hook -> unit
  end

  val async : sw: Switch.t -> (unit -> unit Fiber.t) -> unit
  (** [async f] runs [f ()] asynchroneously if possible, else immediately. *)

  module Mutex : sig
    type t
    val create : unit -> t
    val lock : t -> unit Fiber.t
    val unlock : t -> unit
  end

  module Condition : sig
    type t
    val create : unit -> t
    val wait : t -> Mutex.t -> unit Fiber.t
    val signal : t -> unit
  end

  module Log : sig
    type 'a log = ('a, unit Fiber.t) Logs.msgf -> unit Fiber.t
    val err : ?src: Logs.src -> 'a log
    val warn : ?src: Logs.src -> 'a log
    val info : ?src: Logs.src -> 'a log
    val debug : ?src: Logs.src -> 'a log
  end

  module Stream : Caqti_stream_sig.S with type 'a fiber := 'a Fiber.t

  module Sequencer : SEQUENCER with type 'a fiber := 'a Fiber.t

end

module type SOCKET_OPS = sig
  type 'a fiber
  type t

  (* These are currently only used by PGX.  Despite the flush, it PGX is doing
   * it's own buffering, so unbuffered should be okay.  output_char and
   * input_char are only used for the packet header. *)
  val output_char : t -> char -> unit fiber
  val output_string : t -> string -> unit fiber
  val flush : t -> unit fiber
  val input_char : t -> char fiber
  val really_input : t -> Bytes.t -> int -> int -> unit fiber
  val close : t -> unit fiber
end

module type TLS_PROVIDER = sig
  type 'a fiber
  type tcp_flow
  type tls_flow

  type tls_config
  val tls_config_key : tls_config option Caqti_connect_config.key

  val start_tls :
    config: tls_config ->
    ?host: [`host] Domain_name.t ->
    tcp_flow -> (tls_flow, Caqti_error.msg) result fiber
end

module type NET = sig
  type 'a fiber
  type switch
  type stdenv

  module Sockaddr : sig
    type t
    val unix : string -> t
    val tcp : Ipaddr.t * int -> t
  end

  val getaddrinfo :
    stdenv: stdenv -> [`host] Domain_name.t -> int ->
    (Sockaddr.t list, [> `Msg of string]) result fiber
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

  val convert_io_exception : exn -> Caqti_error.msg option
  (** If the read and write operations in Socket raise exceptions other than
      {!End_of_file} and {!Failure}, this function is used to intercept them. *)

  (** A socket with input and output channels and dedicated IO functions.  This
      bundling is done to support the various APIs involved for networking and
      StartTLS. *)
  module Socket : SOCKET_OPS with type 'a fiber := 'a fiber

  type tcp_flow
  type tls_flow

  val connect_tcp :
    sw: switch -> stdenv: stdenv -> Sockaddr.t ->
    (Socket.t, Caqti_error.msg) result fiber

  val tcp_flow_of_socket : Socket.t -> tcp_flow option
  val socket_of_tls_flow : sw: switch -> tls_flow -> Socket.t

  module type TLS_PROVIDER = TLS_PROVIDER
    with type 'a fiber := 'a fiber
     and type tcp_flow := tcp_flow
     and type tls_flow := tls_flow

  val register_tls_provider : (module TLS_PROVIDER) -> unit

  val tls_providers : Caqti_connect_config.t -> (module TLS_PROVIDER) list

end

module type S = sig
  include CORE

  module Net : NET
    with type 'a fiber := 'a Fiber.t
     and type switch := Switch.t
     and type stdenv := stdenv
end
