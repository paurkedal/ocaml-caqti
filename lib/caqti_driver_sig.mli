(* Copyright (C) 2017--2018  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

(** (internal) Signature for driver implementation.

    This interface is unstable. *)

module type S = sig
  type 'a future

  module type CONNECTION =
    Caqti_connection_sig.Base with type 'a future := 'a future

  val driver_info : Caqti_driver_info.t

  val connect :
    Uri.t -> ((module CONNECTION), [> Caqti_error.connect]) result future
end

module type F =
  functor (System : Caqti_system_sig.S) ->
    S with type 'a future := 'a System.future
