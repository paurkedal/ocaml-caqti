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

(** Establishing Connections for Eio without Unix

    This module provides connections to the PGX database for Eio applications.
    For other database systems, you will need {!Caqti_eio_unix}. *)

module Stream : Caqti_stream_sig.S with type 'a fiber := 'a

(**/**) (* for private use by caqti-eio.unix *)
module System : sig
  include Caqti_platform.System_sig.S
    with type 'a Fiber.t = 'a
     and type Switch.t = Eio.Switch.t
     and type connect_env = Eio.Stdenv.t
     and module Stream = Stream
end
(**/**)

module Pool : sig
  include Caqti_pool_sig.S with type 'a fiber := 'a

  (**/**)
  val create :
    ?max_size: int ->
    ?max_idle_size: int ->
    ?max_idle_age: Mtime.Span.t ->
    ?max_use_count: int option ->
    ?check: ('a -> (bool -> unit) -> unit) ->
    ?validate: ('a -> bool) ->
    ?log_src: Logs.Src.t ->
    sw: Eio.Switch.t ->
    connect_env: Eio.Stdenv.t ->
    (unit -> ('a, 'e) result) -> ('a -> unit) ->
    ('a, 'e) t
end

include Caqti_connect_sig.S
  with type 'a fiber := 'a
   and type 'a with_switch := sw: Eio.Switch.t -> 'a
   and type 'a with_stdenv := connect_env: Eio.Stdenv.t -> 'a
   and type ('a, 'e) stream := ('a, 'e) Stream.t
   and type ('a, 'e) pool := ('a, 'e) Pool.t

val or_fail : ('a, [< Caqti_error.t]) result -> 'a
(** Eliminates the error-case by raising {!Caqti_error.Exn}. *)
