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

open Printf

type template =
  | L of string
  | P of int
  | S of template list

let format_query qs =

  let n = String.length qs in

  let rec skip_quoted j =
    if j = n then
      ksprintf invalid_arg "Caqti_request.create_p: Unmatched quote in %S" qs
    else if qs.[j] = '\'' then
      if j + 1 < n && qs.[j + 1] = '\'' then
        skip_quoted (j + 2)
      else
        j + 1
    else
      skip_quoted (j + 1) in

  let rec loop p i j acc = (* acc is reversed *)
    if j = n then L (String.sub qs i (j - i)) :: acc else
    (match qs.[j] with
     | '\'' ->
        let k = skip_quoted (j + 1) in
        loop p k k
          (L (String.sub qs j (k - j)) ::
           L (String.sub qs i (j - i)) :: acc)
     | '?' ->
        loop (p + 1) (j + 1) (j + 1)
          (P p ::
           L (String.sub qs i (j - i)) :: acc)
     | _ ->
        loop p i (j + 1) acc) in

  (match loop 0 0 0 [] with
   | [] -> invalid_arg "Caqti_request.create_p: Empty query string."
   | [frag] -> frag
   | rev_frags -> S (List.rev rev_frags))

type ('a, 'b, +'m) t = {
  id: int option;
  template: Caqti_driver_info.t -> template;
  params_type: 'a Caqti_type.t;
  row_type: 'b Caqti_type.t;
  row_mult: 'm Caqti_mult.t;
} constraint 'm = [< `Zero | `One | `Many]

let last_id = ref (-1)

let create ?(oneshot = false) params_type row_type row_mult template =
  let id = if oneshot then None else (incr last_id; Some !last_id) in
  {id; template; params_type; row_type; row_mult}

let create_p ?oneshot params_type row_type row_mult qs =
  create ?oneshot params_type row_type row_mult
    (fun di ->
      (match Caqti_driver_info.parameter_style di with
       | `None ->
          ksprintf invalid_arg
            "The %s driver does not support query parameters."
            (Caqti_driver_info.uri_scheme di)
       | `Linear "?" -> L (qs di)
       | _ -> format_query (qs di)))

let params_type request = request.params_type
let row_type request = request.row_type
let row_mult request = request.row_mult

let query_id request = request.id
let query_template request = request.template
