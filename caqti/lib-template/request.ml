(* Copyright (C) 2017--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

[@@@alert "-caqti_private"]

module Log = (val Logs.src_log (Logs.Src.create "caqti"))

type prepare_policy =
  | Direct
  | Dynamic
  | Static

type ('a, 'b, +'m) t = {
  id: int option;
  prepare_policy: prepare_policy;
  query: Dialect.t -> Query.t;
  param_type: 'a Row_type.t;
  row_type: 'b Row_type.t;
  row_mult: 'm Row_mult.t;
} constraint 'm = [< `Zero | `One | `Many]

let last_id = ref (-1)

let create prepare_policy (param_type, row_type, row_mult) query =
  let query = Lru.memo ~cap:8 (fun _ -> query) in
  let id =
    (match prepare_policy with
     | Direct -> None
     | Static | Dynamic -> incr last_id; Some !last_id)
  in
  {id; prepare_policy; query; param_type; row_type; row_mult}

let prepare_policy request = request.prepare_policy
let param_type request = request.param_type
let row_type request = request.row_type
let row_mult request = request.row_mult

let query_id request = request.id
let query request = request.query

let empty_subst _ = raise Not_found

let default_dialect = Dialect.create_unknown ~purpose:`Printing ()

let make_pp ?(dialect = default_dialect) ?(subst = empty_subst) () ppf req =
  let query = Query.expand subst (req.query dialect) in
  Format.fprintf ppf "(%a -->%s %a) {|%a|}"
    Row_type.pp req.param_type
    (match Row_mult.expose req.row_mult with
     | `Zero -> "."
     | `One -> ""
     | `Zero_or_one -> "?"
     | `Zero_or_more -> "*")
    Row_type.pp req.row_type
    Query.pp query

let pp ppf = make_pp () ppf

let pp_with_param_enabled =
  (match Sys.getenv "CAQTI_DEBUG_PARAM" with
   | "true" -> true
   | "false" -> false
   | s ->
      Log.err (fun f ->
        f "Invalid value %s for CAQTI_DEBUG_PARAM, assuming false." s);
      false
   | exception Not_found -> false)

let make_pp_with_param ?dialect ?subst () ppf (req, param) =
  let pp = make_pp ?subst ?dialect () in
  pp ppf req;
  if pp_with_param_enabled then
    Format.fprintf ppf " %a" (Row.pp req.param_type) param

type liveness_witness = int option

let liveness_witness request =
  assert (request.id <> None);
  request.id
