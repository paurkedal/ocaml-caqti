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

let format_query di ?env sql =
  let n = String.length sql in
  let buf = Buffer.create n in
  let add_parameter =
    (match Caqti_driver_info.parameter_style di with
     | `None -> invalid_arg "Caqti_request.format_query: \
                             The driver does not support parameters."
     | `Linear s -> fun _ -> Buffer.add_string buf s
     | `Indexed f -> fun p -> Buffer.add_string buf (f p)) in
  let add_substring =
    (match env with
     | None -> Buffer.add_substring buf sql
     | Some env ->
        fun i n -> Buffer.add_substitute buf (env di) (String.sub sql i n)) in
  let rec skip_quoted j =
    if j = n then invalid_arg ("format_query: Unmatched quote: " ^ sql) else
    if sql.[j] = '\'' then
      if j + 1 < n && sql.[j + 1] = '\'' then
        skip_quoted (j + 2)
      else
        j + 1
    else
      skip_quoted (j + 1) in
  let rec loop p i j =
    if j = n then add_substring i (j - i) else
    (match sql.[j] with
     | '\'' ->
        add_substring i (j - i);
        let k = skip_quoted (j + 1) in
        Buffer.add_substring buf sql j (k - j);
        loop p k k
     | '?' ->
        add_substring i (j - i);
        add_parameter p;
        loop (p + 1) (j + 1) (j + 1)
     | _ ->
        loop p i (j + 1)) in
  loop 0 0 0;
  Buffer.contents buf

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

let create_p ?oneshot params_type row_type row_mult ?env query_string =
  create ?oneshot params_type row_type row_mult
    (fun di -> format_query di ?env (query_string di))

let params_type request = request.params_type
let row_type request = request.row_type
let row_mult request = request.row_mult

let query_id request = request.id
let query_string request = request.query_string
