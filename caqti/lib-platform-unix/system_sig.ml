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

module type S = sig
  type 'a fiber
  type connect_env

  module Unix : sig
    type file_descr
    val wrap_fd : (file_descr -> 'a fiber) -> Unix.file_descr -> 'a fiber
    val poll :
      connect_env: connect_env ->
      ?read: bool -> ?write: bool -> ?timeout: float ->
      file_descr -> (bool * bool * bool) fiber
  end

  module Preemptive : sig
    val detach : ('a -> 'b) -> 'a -> 'b fiber
    val run_in_main : (unit -> 'a fiber) -> 'a
  end

end
