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

module type S = sig
  type +'a future
  type (+'a, +'err) stream

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a future := 'a future
     and type ('a, 'err) stream := ('a, 'err) stream

  val driver_info : Caqti_driver_info.t

  val connect :
    ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
    tweaks_version: int * int ->
    Uri.t -> ((module CONNECTION), [> Caqti_error.connect]) result future
end

module type Loader = sig
  type +'a future
  type (+'a, +'e) stream

  module type DRIVER = S
    with type 'a future := 'a future
     and type ('a, 'e) stream := ('a, 'e) stream

  val load_driver :
    uri: Uri.t -> string -> ((module DRIVER), [> Caqti_error.load]) result
end
