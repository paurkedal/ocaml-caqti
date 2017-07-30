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

(** Only for internal use, and to be removed. *)

open Caqti_sigs

module Connection_v2_of_v1
  (System : SYSTEM)
  (Conn : sig
    module Tuple : TUPLE
    include CONNECTION
      with module Tuple := Tuple
       and type 'a callback = Tuple.t -> 'a
       and type 'a io = 'a System.io
   end) :
  Caqti_connection_sig.S with type 'a io := 'a System.io
