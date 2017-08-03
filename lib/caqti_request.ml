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

type ('a, 'b, +'m) t = {
  id: int option;
  query_string: Caqti_driver_info.t -> string;
  params_type: 'a Caqti_type.t;
  row_type: 'b Caqti_type.t;
  row_mult: 'm Caqti_mult.t;
} constraint 'm = [< `Zero | `One | `Many]

let last_id = ref (-1)

let create ?(oneshot = false) params_type row_type row_mult query_string =
  let id = if oneshot then None else (incr last_id; Some !last_id) in
  {id; query_string; params_type; row_type; row_mult}

let params_type request = request.params_type
let row_type request = request.row_type
let row_mult request = request.row_mult

let query_id request = request.id
let query_string request = request.query_string
