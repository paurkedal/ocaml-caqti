(* Copyright (C) 2020  Petter A. Urkedal <paurkedal@gmail.com>
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

module Q = Caqti_query

let expect_parse ?env qs q' =
  let rq = Caqti_request.exec ?env ~oneshot:true Caqti_type.unit qs in
  let q = Caqti_query.normal (Caqti_request.query rq Caqti_driver_info.dummy) in
  if not (Q.equal q q') then begin
    eprintf "Parsed:   %s\nExpected: %s\n"
      (Q.show q) (Q.show q');
    assert false
  end

let test_request_parse () =
  let env _ = function
   | "alpha" -> Q.L "α"
   | "beta" -> Q.L "β"
   | "beta." -> Q.L "β[dot]"
   | "gamma" -> Q.L "γ"
   | "delta" -> Q.L "δ"
   | _ -> raise Not_found
  in
  expect_parse ~env "$(alpha) $beta. $(gamma) $delta." Q.(L"α β[dot] γ δ.")

let () =
  test_request_parse ()
