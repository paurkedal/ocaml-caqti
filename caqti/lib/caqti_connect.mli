(* Copyright (C) 2017--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

module Make_without_connect :
  functor (System : Caqti_driver_sig.System_common) ->
  Caqti_connect_sig.S_without_connect
    with type 'a future = 'a System.future
     and module Stream = System.Stream

module Make_connect :
  functor (System : Caqti_driver_sig.System_common) ->
  functor (Loader : Caqti_driver_sig.Loader
            with type 'a future := 'a System.future
             and type ('a, 'e) stream := ('a, 'e) System.Stream.t) ->
  Caqti_connect_sig.Connect
    with type 'a future = 'a System.future
     and type connection := Make_without_connect (System).connection
     and type ('a, 'e) pool := ('a, 'e) Make_without_connect (System).Pool.t
     and type 'a connect_fun :=
        ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
        ?tweaks_version: int * int ->
        Uri.t -> 'a
(** Constructs the main module used to connect to a database for the given
    concurrency model. *)
