(* Copyright (C) 2020--2026  Petter A. Urkedal <paurkedal@gmail.com>
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

open Printf
open Caqti.Template

let dialect =
  Dialect.create_unknown ~purpose:`Dummy () [@alert "-caqti_private"]

let expect_parse ~subst qs q' =
  let rq =
    let open Caqti.Templater in
    let q = qs |> Q.parse |> Q.expand subst in
    static_gen T.(unit -->. unit) @@ fun _ -> q
  in
  let q =
    (match Request.queries rq dialect with
     | [q] -> Query.normal q
     | _ -> assert false)
  in
  if not (Query.equal q q') then begin
    eprintf "Parsed:   %s\nExpected: %s\n" (Query.show q) (Query.show q');
    assert false
  end

let test_request_parse () =
  let open Caqti.Templater in
  let subst = function
   | "alpha" -> Q.lit "α"
   | "beta" -> Q.lit "β"
   | "beta." -> Q.lit "β[dot]"
   | "gamma" -> Q.lit "γ"
   | "delta" -> Q.lit "δ"
   | _ -> raise Not_found
  in
  expect_parse ~subst "$(alpha) $$ $beta. $(gamma) $delta. $$ $Q$ $beta. $Q$"
    (Q.lit "α $$ β[dot] γ δ. $$ $Q$ $beta. $Q$")

let test_cases = [
  "parse", `Quick, test_request_parse;
]
