(* Copyright (C) 2019  Petter A. Urkedal <paurkedal@gmail.com>
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

(** {b Internal:} Connection Utilities for Drivers *)

module Make_helpers :
  functor (Sys : Caqti_driver_sig.System_common) ->
sig
  open Sys
  val assert_single_use : bool ref -> (unit -> 'a future) -> 'a future
end

module Make_convenience :
  functor (Sys : Caqti_driver_sig.System_common) ->
  functor (C : Caqti_connection_sig.Base
                with type 'a future := 'a Sys.future
                 and type ('a, 'err) stream := ('a, 'err) Sys.Stream.t) ->
  Caqti_connection_sig.Convenience with type 'a future := 'a Sys.future

module Make_populate :
  functor (Sys : Caqti_driver_sig.System_common) ->
  functor (C : Caqti_connection_sig.Base
                with type 'a future := 'a Sys.future
                 and type ('a, 'err) stream := ('a, 'err) Sys.Stream.t) ->
  Caqti_connection_sig.Populate with type 'a future := 'a Sys.future
