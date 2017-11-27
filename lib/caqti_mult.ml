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

(** Multiplicities of zero, one, and many. *)

type +'m t = (* not GADT due to variance *)
 | Zero
 | One
 | Zero_to_one
 | Zero_to_many
constraint 'm = [< `Zero | `One | `Many]

let zero : [> `Zero] t = Zero
let one : [> `One] t = One
let zero_or_one : [> `Zero | `One] t = Zero_to_one
let many : ([> `Zero | `One | `Many] as 'a) t = Zero_to_many

let only_zero : [< `Zero] t -> unit =
  function Zero -> () | _ -> assert false
let only_one : [< `One] t -> unit =
  function One -> () | _ -> assert false
let only_zero_or_one : [< `Zero | `One] t -> unit =
  function Zero | One -> () | _ -> assert false

let expose = function
 | Zero -> `Zero
 | One -> `One
 | Zero_to_one -> `Zero_or_one
 | Zero_to_many -> `Zero_or_more
