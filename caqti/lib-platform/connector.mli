(* Copyright (C) 2017--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Connection functor and backend registration. *)

val define_loader : (string -> (unit, string) result) -> unit
(** Defines the function used to dynamically load driver libraries. This is
    normally called during initialization by the [caqti-dynload] library, if
    linked into the application. *)

val load_library : string -> (unit, string) result

module Make :
  functor (System : System_sig.S) ->
  functor (Pool : Pool.S
    with type 'a future := 'a System.future
     and type switch := System.Switch.t
     and type connect_env := System.connect_env) ->
  functor (Loader : Driver_sig.Loader
    with type 'a future := 'a System.future
     and type switch := System.Switch.t
     and type connect_env := System.connect_env
     and type ('a, 'e) stream := ('a, 'e) System.Stream.t) ->
  Caqti_connect_sig.S
    with type 'a future := 'a System.future
     and type ('a, 'e) stream := ('a, 'e) System.Stream.t
     and type ('a, 'e) pool := ('a, 'e) Pool.t
     and type 'a with_switch := sw: System.Switch.t -> 'a
     and type 'a with_stdenv := connect_env: System.connect_env -> 'a
(** Constructs the main module used to connect to a database for the given
    concurrency model. *)
