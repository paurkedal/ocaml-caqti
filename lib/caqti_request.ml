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

type ('a, 'b, +'m) t = {
  id: int option;
  template: Caqti_driver_info.t -> template;
  param_type: 'a Caqti_type.t;
  row_type: 'b Caqti_type.t;
  row_mult: 'm Caqti_mult.t;
} constraint 'm = [< `Zero | `One | `Many]

let last_id = ref (-1)

let create ?(oneshot = false) param_type row_type row_mult template =
  let id = if oneshot then None else (incr last_id; Some !last_id) in
  {id; template; param_type; row_type; row_mult}

let param_type request = request.param_type
let row_type request = request.row_type
let row_mult request = request.row_mult

let query_id request = request.id
let query_template request = request.template

(* Convenience *)

let invalid_arg_f fmt = ksprintf invalid_arg fmt

let format_query ~env qs =

  let n = String.length qs in

  let rec skip_quoted j =
    if j = n then
      invalid_arg_f "Caqti_request.create_p: Unmatched quote in %S" qs
    else if qs.[j] = '\'' then
      if j + 1 < n && qs.[j + 1] = '\'' then
        skip_quoted (j + 2)
      else
        j + 1
    else
      skip_quoted (j + 1) in

  let rec scan_int i p =
    if i = n then (i, p) else
    (match qs.[i] with
     | '0'..'9' as ch ->
        scan_int (i + 1) (p * 10 + Char.code ch - Char.code '0')
     | _ -> (i, p)) in

  let rec skip_end_paren j =
    if j = n then invalid_arg_f "Unbalanced end-parenthesis in %S" qs else
    if qs.[j] = '(' then skip_end_paren (skip_end_paren (j + 1)) else
    if qs.[j] = ')' then j + 1 else
    skip_end_paren (j + 1) in

  let rec loop p i j acc = (* acc is reversed *)
    if j = n then L (String.sub qs i (j - i)) :: acc else
    (match qs.[j] with
     | '\'' ->
        let k = skip_quoted (j + 1) in
        loop p i k acc
     | '?' ->
        if p < 0 then invalid_arg "Mixed ? and $i style parameters." else
        let acc = L (String.sub qs i (j - i)) :: acc in
        loop (p + 1) (j + 1) (j + 1) (P p :: acc)
     | '$' ->
        if j + 1 = n then invalid_arg "$ at end of query" else
        let acc = L (String.sub qs i (j - i)) :: acc in
        (match qs.[j + 1] with
         | '0'..'9' ->
            if p > 0 then invalid_arg "Mixed ? and $i style parameters." else
            let k, p' = scan_int (j + 1) 0 in
            let acc = P (p' - 1) :: acc in
            loop (-1) k k acc
         | '(' ->
            let k = skip_end_paren (j + 2) in
            let acc = env (String.sub qs (j + 2) (k - j - 3)) :: acc in
            loop p k k acc
         | '.' ->
            let acc = env "." :: acc in
            loop p (j + 2) (j + 2) acc
         | '$' ->
            let acc = L"$" :: acc in
            loop p (j + 2) (j + 2) acc
         | _ ->
            invalid_arg "Unescaped $ in query string.")
     | _ ->
        loop p i (j + 1) acc) in

  (match loop 0 0 0 [] with
   | [] -> invalid_arg "Caqti_request.create_p: Empty query string."
   | [frag] -> frag
   | rev_frags -> S (List.rev rev_frags))

let no_env _ _ = raise Not_found

let create_p ?(env = no_env) ?oneshot param_type row_type row_mult qs =
  create ?oneshot param_type row_type row_mult
    (fun di ->
      (match Caqti_driver_info.parameter_style di with
       | `None ->
          ksprintf invalid_arg
            "The %s driver does not support query parameters."
            (Caqti_driver_info.uri_scheme di)
       | _ ->
          let env k = try env di k with
           | Not_found ->
              invalid_arg_f "The reference to $(%s) in %S is not defined by \
                             the environment handler." k (qs di) in
          format_query ~env (qs di)))

let exec ?env ?oneshot pt qs =
  create_p ?env ?oneshot pt Caqti_type.unit Caqti_mult.zero (fun _ -> qs)
let find ?env ?oneshot pt rt qs =
  create_p ?env ?oneshot pt rt Caqti_mult.one (fun _ -> qs)
let find_opt ?env ?oneshot pt rt qs =
  create_p ?env ?oneshot pt rt Caqti_mult.zero_or_one (fun _ -> qs)
let collect ?env ?oneshot pt rt qs =
  create_p ?env ?oneshot pt rt Caqti_mult.many (fun _ -> qs)
