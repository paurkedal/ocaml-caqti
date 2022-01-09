(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open Caqti_common_priv
open Printf

type query = Caqti_query.t =
  | L of string
  | Q of string
  | P of int
  | E of string
  | S of query list

type ('a, 'b, +'m) t = {
  id: int option;
  query: Caqti_driver_info.t -> query;
  param_type: 'a Caqti_type.t;
  row_type: 'b Caqti_type.t;
  row_mult: 'm Caqti_mult.t;
} constraint 'm = [< `Zero | `One | `Many]

let last_id = ref (-1)

let create ?(oneshot = false) param_type row_type row_mult query =
  let id = if oneshot then None else (incr last_id; Some !last_id) in
  {id; query; param_type; row_type; row_mult}

let param_type request = request.param_type
let row_type request = request.row_type
let row_mult request = request.row_mult

let query_id request = request.id
let query request = request.query

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

  let rec skip_idr s i =
    if i = String.length s then i else
    (match s.[i] with
     | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> skip_idr s (i + 1)
     | _ -> i)
  in

  let rec loop p i j acc = (* acc is reversed *)
    if j = n then L (String.sub qs i (j - i)) :: acc else
    (match qs.[j] with
     | '\'' ->
        let k = skip_quoted (j + 1) in
        loop p i k acc
     | '?' ->
        if p < 0 then invalid_arg "Mixed ? and $i style parameters." else
        let acc = L (String.sub qs i (j - i)) :: acc in
        if j + 1 < n && (match qs.[j+1] with '0'..'9' -> true | _ -> false) then
          invalid_arg "?i is not allowed"
        else
          loop (p + 1) (j + 1) (j + 1) (P p :: acc)
     | '$' ->
        if j + 1 = n then invalid_arg "$ at end of query" else
        let acc = L (String.sub qs i (j - i)) :: acc in
        (match qs.[j + 1] with
         | '$' ->
            Alog.err (fun f ->
              f "Invalid $$ in Caqti query strings, falling back to deprecated \
                 and undocumented behaviour for now. \
                 This will change in a future version.");
            let acc = L"$" :: acc in
            loop p (j + 2) (j + 2) acc
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
         | 'a'..'z' | 'A'..'Z' ->
            let k = skip_idr qs (j + 2) in
            if k = String.length qs then invalid_arg "Unterminated '$'." else
            (match qs.[k] with
             | '.' ->
                let idr = String.sub qs (j + 1) (k - j) in
                loop p (k + 1) (k + 1) (env idr :: acc)
             | '$' ->
                let quote = String.sub qs j (k - j + 1) in
                loop p (k + 1) (k + 1) (L quote :: acc)
             | ch ->
                invalid_arg_f
                  "Unterminated $ or invalid character %C in identifier." ch)
         | _ ->
            invalid_arg "Unescaped $ in query string.")
     | _ ->
        loop p i (j + 1) acc) in

  (match loop 0 0 0 [] with
   | [] -> invalid_arg "Caqti_request.create_p: Empty query string."
   | [frag] -> frag
   | rev_frags -> S (List.rev rev_frags))

let no_env _ _ = raise Not_found

let rec simplify = function
 | L "" -> S []
 | S frags -> S (frags |> List.map simplify |> List.filter ((<>) (S [])))
 | L _ | Q _ | P _ | E _ as frag -> frag

let create_p ?(env = no_env) ?oneshot param_type row_type row_mult qs =
  create ?oneshot param_type row_type row_mult
    (fun di ->
      let env k =
        (match simplify (env di k) with
         | exception Not_found ->
            let l = String.length k in
            if l = 0 || k.[l - 1] <> '.' then
              invalid_arg_f "No expansion provided for $(%s) \
                             as needed by query %S." k (qs di) else
            let k' = String.sub k 0 (l - 1) in
            (match simplify (env di k') with
             | exception Not_found ->
                invalid_arg_f "No expansion provided for $(%s) or $(%s) \
                               as needed by query %S." k k' (qs di)
             | S[] as v -> v
             | v -> S[v; L"."])
         | v -> v)
      in
      format_query ~env (qs di))

let exec ?env ?oneshot pt qs =
  create_p ?env ?oneshot pt Caqti_type.unit Caqti_mult.zero (fun _ -> qs)
let find ?env ?oneshot pt rt qs =
  create_p ?env ?oneshot pt rt Caqti_mult.one (fun _ -> qs)
let find_opt ?env ?oneshot pt rt qs =
  create_p ?env ?oneshot pt rt Caqti_mult.zero_or_one (fun _ -> qs)
let collect ?env ?oneshot pt rt qs =
  create_p ?env ?oneshot pt rt Caqti_mult.zero_or_more (fun _ -> qs)

let pp ppf req =
  Format.fprintf ppf "(%a -%s-> %a) {|%a|}"
    Caqti_type.pp req.param_type
    (match Caqti_mult.expose req.row_mult with
     | `Zero -> "!"
     | `One -> ""
     | `Zero_or_one -> "?"
     | `Zero_or_more -> "*")
    Caqti_type.pp req.row_type
    Caqti_query.pp (req.query Caqti_driver_info.dummy)

let pp_with_param_enabled =
  (match Sys.getenv "CAQTI_DEBUG_PARAM" with
   | "true" -> true
   | "false" -> false
   | s ->
      Alog.err (fun f ->
        f "Invalid value %s for CAQTI_DEBUG_PARAM, assuming false." s);
      false
   | exception Not_found -> false)

let pp_with_param ?(driver_info = Caqti_driver_info.dummy) ppf (req, param) =
  if pp_with_param_enabled then
    Format.fprintf ppf "{|%a|} %a"
      Caqti_query.pp (req.query driver_info)
      Caqti_type.pp_value (req.param_type, param)
  else
    pp ppf req
