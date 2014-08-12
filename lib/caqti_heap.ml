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

module type S = sig
  type elt
  type t

  val empty : t
  val is_empty : t -> bool
  val card : t -> int
  val push : elt -> t -> t
  val merge : t -> t -> t
  val pop_e : t -> elt * t
end

module Make (Elt : Set.OrderedType) = struct

  type elt = Elt.t
  type t = O | Y of int * elt * t * t

  let empty = O

  let is_empty h = h = O

  let card = function
    | O -> 0
    | Y (n, _, _, _) -> n

  let rec push e' = function
    | O -> Y (1, e', O, O)
    | Y (n, e, hL, hR) ->
      let e_min, e_max = if Elt.compare e' e < 0 then e', e else e, e' in
      if card hL < card hR then Y (n + 1, e_min, push e_max hL, hR)
			   else Y (n + 1, e_min, hL, push e_max hR)

  let rec merge hL hR =
    match hL, hR with
    | O, h | h, O -> h
    | Y (nL, eL, hA, hB), Y (nR, eR, hC, hD) ->
      if Elt.compare eL eR < 0 then Y (nL + nR, eL, merge hA hB, hR)
			       else Y (nL + nR, eR, hL, merge hC hD)

  let pop_e = function
    | O -> invalid_arg "Caqti_heap.pop_e: Empty heap."
    | Y (_, e, hL, hR) -> e, merge hL hR

end
