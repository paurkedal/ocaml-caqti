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

(** Establinging Connections for Eio with Unix

    This module provides database connections for applications using Eio.  It
    supports all database drivers. *)

include Caqti_connect_sig.S
  with type 'a fiber := 'a
   and type ('a, 'e) stream := ('a, 'e) Caqti_eio.Stream.t
   and type ('a, 'e) pool := ('a, 'e) Caqti_eio.Pool.t
   and type connection := Caqti_eio.connection
   and type 'a with_switch := sw: Eio.Switch.t -> 'a
   and type 'a with_stdenv := stdenv: Caqti_eio.stdenv -> 'a
