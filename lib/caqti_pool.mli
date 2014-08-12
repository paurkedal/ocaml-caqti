(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

module Make (System : Caqti_sigs.SYSTEM) : sig
  open System

  type 'a t

  val create :
	?max_size: int ->
	?check: ('a -> (bool -> unit) -> unit) ->
	?validate: ('a -> bool io) ->
	(unit -> 'a io) -> ('a -> unit io) -> 'a t

  val size : 'a t -> int

  val use : ?priority: float -> ('a -> 'b io) -> 'a t -> 'b io

  val drain : 'a t -> unit io

end
