(* Copyright (C) 2017--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

let nonlin1_q = Caqti_request.create
  Caqti_type.(tup3 int int int) Caqti_type.int Caqti_mult.one @@ fun _ ->
  S[L"SELECT 2 * "; P 2; L" + "; P 2; L" - 3 * "; P 0; L" + 5 * "; P 1]

let nonlin2_q = Caqti_request.find
  Caqti_type.(tup3 int int int) Caqti_type.int
  "SELECT 2 * $3 + $3 - 3 * $1 + 5 * $2"

module Make (Ground : Testkit_sig.Ground) = struct
  open Ground

  let nonlin (p0, p1, p2) = 2 * p2 + p2 - 3 * p0 + 5 * p1

  let test_nonlin (module Db : Caqti_sys.CONNECTION) =
    let rec loop n =
      if n = 0 then return () else
      let p = (Random.int 1000, Random.int 1000, Random.int 1000) in
      (Db.find nonlin1_q p >>= or_fail >|= fun y -> assert (y = nonlin p))
        >>= fun () ->
      (Db.find nonlin2_q p >>= or_fail >|= fun y -> assert (y = nonlin p))
        >>= fun () ->
      loop (n - 1)
    in
    loop 1000

  let env1_q =
    let env _ = function
     | "." -> Caqti_query.L"100"
     | "fourty" -> Caqti_query.L"40"
     | _ -> raise Not_found in
    Caqti_request.find ~env Caqti_type.unit Caqti_type.int "SELECT $. - $(fourty)"

  let test_env (module Db : Caqti_sys.CONNECTION) =
    Db.find env1_q () >>= or_fail >|= fun y -> assert (y = 60)

  let test_cases = [
    "nonlinear", `Quick, test_nonlin;
    "environment", `Quick, test_env;
  ]

end
