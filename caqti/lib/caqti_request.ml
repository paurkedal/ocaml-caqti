(* Copyright (C) 2024--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_template

include Request

let create ?oneshot pt rt rm make_query =
  create ?oneshot (pt, rt, rm)
    (fun dialect -> make_query (Caqti_driver_info.of_dialect dialect))

let query req driver_info =
  query req (Caqti_driver_info.dummy_dialect driver_info)

module Infix = struct
  let (-->.) t u ?oneshot f = create ?oneshot t u Row_mult.zero f
  let (-->!) t u ?oneshot f = create ?oneshot t u Row_mult.one f
  let (-->?) t u ?oneshot f = create ?oneshot t u Row_mult.zero_or_one f
  let (-->*) t u ?oneshot f = create ?oneshot t u Row_mult.zero_or_more f

  let (@:-) f s =
    let q = Caqti_query.of_string_exn s in
    f (fun _ -> q)

  let (@@:-) f g =
    f (fun d -> Caqti_query.of_string_exn (g (Caqti_driver_info.dialect_tag d)))

  let (->.) t u ?oneshot s = create ?oneshot t u Row_mult.zero @:- s
  let (->!) t u ?oneshot s = create ?oneshot t u Row_mult.one @:- s
  let (->?) t u ?oneshot s = create ?oneshot t u Row_mult.zero_or_one @:- s
  let (->*) t u ?oneshot s = create ?oneshot t u Row_mult.zero_or_more @:- s
end

let no_env _ _ = raise Not_found

let make_pp ?(env = no_env) ?(driver_info = Caqti_driver_info.dummy) () =
  let dialect = Caqti_driver_info.dummy_dialect driver_info in
  make_pp ~subst:(env driver_info) ~dialect ()

let make_pp_with_param
      ?(env = no_env) ?(driver_info = Caqti_driver_info.dummy) () =
  let dialect = Caqti_driver_info.dummy_dialect driver_info in
  make_pp_with_param ~subst:(env driver_info) ~dialect ()
