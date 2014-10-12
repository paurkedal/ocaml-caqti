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

open Caqti_metadata

exception Missing_query_string

type oneshot_query = backend_info -> string

type prepared_query = {
  pq_index : int;
  pq_name : string;
  pq_encode : backend_info -> string;
}

type query =
  | Oneshot of oneshot_query
  | Prepared of prepared_query

let oneshot_full f = Oneshot f
let oneshot_fun f = Oneshot (fun {bi_dialect_tag} -> f bi_dialect_tag)
let oneshot_any s = Oneshot (fun _ -> s)
let oneshot_sql s =
  oneshot_fun (function #sql_dialect_tag -> s | _ -> raise Missing_query_string)

let next_prepared_index = ref 0

let prepare_full ?name pq_encode =
  let pq_index = !next_prepared_index in
  next_prepared_index := succ !next_prepared_index;
  let pq_name =
    match name with
    | None -> "_s" ^ (string_of_int pq_index)
    | Some name -> name in
  Prepared {pq_index; pq_name; pq_encode}

let prepare_fun ?name f =
  prepare_full ?name (fun {bi_dialect_tag} -> f bi_dialect_tag)

let prepare_any ?name qs = prepare_full ?name (fun _ -> qs)

let prepare_sql ?name s =
  prepare_fun ?name
    (function #sql_dialect_tag -> s | _ -> raise Missing_query_string)

type query_info = [ `Oneshot of string | `Prepared of string * string ]

let make_query_info backend_info = function
  | Oneshot qsf -> `Oneshot (qsf backend_info)
  | Prepared {pq_name; pq_encode} -> `Prepared (pq_name, pq_encode backend_info)
