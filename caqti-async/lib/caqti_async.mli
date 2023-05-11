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

(** Connector for Async. *)

open Async_kernel

module Stream : Caqti_stream_sig.S with type 'a future := 'a Deferred.t
module Pool : Caqti_pool_sig.S with type 'a future := 'a Deferred.t

include Caqti_connect_sig.S
  with type 'a future := 'a Deferred.t
   and type 'a connect_fun := Uri.t -> 'a
   and type 'a with_connection_fun := Uri.t -> 'a
   and type ('a, 'e) stream := ('a, 'e) Stream.t
   and type ('a, 'e) pool := ('a, 'e) Pool.t
