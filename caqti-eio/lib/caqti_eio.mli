(* Copyright (C) 2022--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Establishing Connections for Eio without Unix

    {b Note that} the {!connect}, {!with_connection}, and {!connect_pool}
    functions from this module only provides support for caqti-driver-pgx (i.e.
    the [pgx] URI scheme).  The other database drivers are based on C bindings
    which require UNIX facilities provided by {!Caqti_eio_unix.connect},
    {!Caqti_eio_unix.with_connection}, and {!Caqti_eio_unix.connect_pool},
    respectively.

    {b The caqti-eio library should be considered unstable} for now.  Eio is in
    active development including its API, and the Caqti interface to it may
    benefit from further revision before the first major Eio release lands. *)

type stdenv = <
  net : [`Generic] Eio.Net.ty Eio.Std.r;
  clock : float Eio.Time.clock_ty Eio.Std.r;
  mono_clock : Eio.Time.Mono.ty Eio.Std.r;
>

module Stream : Caqti_stream_sig.S with type 'a fiber := 'a

(**/**) (* for private use by caqti-eio.unix *)
module System : Caqti_platform.System_sig.S
  with type 'a Fiber.t = 'a
   and type Switch.t = Eio.Switch.t
   and type stdenv = stdenv
   and module Stream = Stream
   and type Net.tcp_flow =
    [Eio.Flow.two_way_ty | Eio.Resource.close_ty] Eio.Resource.t
   and type Net.tls_flow =
    [Eio.Flow.two_way_ty | Eio.Resource.close_ty | `Tls] Eio.Resource.t
(**/**)

module Pool : sig
  include Caqti_pool_sig.S with type 'a fiber := 'a

  (**/**)
  val create :
    ?config: Caqti_pool_config.t ->
    ?check: ('a -> (bool -> unit) -> unit) ->
    ?validate: ('a -> bool) ->
    ?log_src: Logs.Src.t ->
    sw: Eio.Switch.t ->
    stdenv: stdenv ->
    (unit -> ('a, 'e) result) -> ('a -> unit) ->
    ('a, 'e) t
end

module type CONNECTION = Caqti_connection_sig.S
  with type 'a fiber := 'a
   and type ('a, 'e) stream := ('a, 'e) Stream.t

include Caqti_connect_sig.S
  with type 'a fiber := 'a
   and type 'a with_switch := sw: Eio.Switch.t -> 'a
   and type 'a with_stdenv := stdenv: stdenv -> 'a
   and type ('a, 'e) stream := ('a, 'e) Stream.t
   and type ('a, 'e) pool := ('a, 'e) Pool.t
   and type connection = (module CONNECTION)

val or_fail : ('a, [< Caqti_error.t]) result -> 'a
(** Eliminates the error-case by raising {!Caqti_error.Exn}. *)
