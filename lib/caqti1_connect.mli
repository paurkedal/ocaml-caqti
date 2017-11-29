(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(** (v1) Connection functor and backend registration. *)

val define_driver : string -> (module Caqti1_driver_sig.F) -> unit
(** [register_scheme scheme m] installs [m] as a handler for the URI scheme
    [scheme].  This call must be done by a backend installed with findlib name
    caqti-driver-{i scheme} as part of its initialization. *)

module Make (System : Caqti1_system_sig.S) : Caqti_sigs.CAQTI
  with type 'a io := 'a System.io
