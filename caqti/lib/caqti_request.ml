(* Copyright (C) 2017--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

let (%) f g x = f (g x)
module Log = (val Logs.src_log (Logs.Src.create "caqti"))

type ('a, 'b, +'m) t = {
  id: int option;
  query: Caqti_driver_info.t -> Caqti_query.t;
  param_type: 'a Caqti_type.t;
  row_type: 'b Caqti_type.t;
  row_mult: 'm Caqti_mult.t;
} constraint 'm = [< `Zero | `One | `Many]

let last_id = Atomic.make (-1)

let create ?(oneshot = false) param_type row_type row_mult query =
  let id = if oneshot then None else Some (Atomic.fetch_and_add last_id 1) in
  {id; query; param_type; row_type; row_mult}

let param_type request = request.param_type
let row_type request = request.row_type
let row_mult request = request.row_mult

let query_id request = request.id
let query request = request.query


(* Convenience Interface *)

module Infix = struct
  let (-->.) t u ?oneshot f = create ?oneshot t u Caqti_mult.zero f
  let (-->!) t u ?oneshot f = create ?oneshot t u Caqti_mult.one f
  let (-->?) t u ?oneshot f = create ?oneshot t u Caqti_mult.zero_or_one f
  let (-->*) t u ?oneshot f = create ?oneshot t u Caqti_mult.zero_or_more f

  let (@:-) f s =
    let q = Caqti_query.of_string_exn s in
    f (fun _ -> q)

  let (@@:-) f g =
    f (Caqti_query.of_string_exn % g % Caqti_driver_info.dialect_tag)

  let (->.) t u ?oneshot s = create ?oneshot t u Caqti_mult.zero @:- s
  let (->!) t u ?oneshot s = create ?oneshot t u Caqti_mult.one @:- s
  let (->?) t u ?oneshot s = create ?oneshot t u Caqti_mult.zero_or_one @:- s
  let (->*) t u ?oneshot s = create ?oneshot t u Caqti_mult.zero_or_more @:- s
end

let no_env _ _ = raise Not_found

let make_pp ?(env = no_env) ?(driver_info = Caqti_driver_info.dummy) ()
            ppf req =
  let query = Caqti_query.expand (env driver_info) (req.query driver_info) in
  Format.fprintf ppf "(%a -->%s %a) {|%a|}"
    Caqti_type.pp req.param_type
    (match Caqti_mult.expose req.row_mult with
     | `Zero -> "."
     | `One -> ""
     | `Zero_or_one -> "?"
     | `Zero_or_more -> "*")
    Caqti_type.pp req.row_type
    Caqti_query.pp query

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

let make_pp_with_param ?env ?driver_info () ppf (req, param) =
  let pp = make_pp ?env ?driver_info () in
  pp ppf req;
  if pp_with_param_enabled then
    Format.fprintf ppf " %a" Caqti_type.pp_value (req.param_type, param)
