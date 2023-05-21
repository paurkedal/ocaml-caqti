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

(**/**)

include Caqti_platform.System_sig.S
  with type 'a future = 'a Lwt.t
   and type connect_env = unit
   and module Stream = Caqti_lwt.Stream
   and type Switch.t = Caqti_lwt.Switch.t

module Alarm : Caqti_platform.Pool.ALARM
  with type switch := Switch.t
   and type connect_env := unit

module Pool : Caqti_platform.Pool.S
  with type 'a future := 'a Lwt.t
   and type switch := Switch.t
   and type connect_env := unit
