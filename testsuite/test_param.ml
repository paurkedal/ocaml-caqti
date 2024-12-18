(* Copyright (C) 2017--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

module Q = struct
  open Caqti_template.Create

  let nonlin1 =
    t3 int int int -->! int @@ fun _ ->
    Q.concat [
      Q.lit "SELECT 2 * "; Q.param 2; Q.lit " + "; Q.param 2;
      Q.lit " - 3 * "; Q.param 0; Q.lit " + 5 * "; Q.param 1;
    ]

  let nonlin2 =
    t3 int int int -->! int @:-
    "SELECT 2 * $3 + $3 - 3 * $1 + 5 * $2"

  let env1 =
    let env = let open Caqti_template.Create in function
     | "." -> Q.lit "100"
     | "fourty" -> Q.lit "40"
     | _ -> raise Not_found
    in
    let q = "SELECT $. - $(fourty)"
      |> Caqti_query.of_string_exn
      |> Caqti_query.expand env
    in
    (Caqti_type.unit -->! Caqti_type.int) @@ fun _ -> q
end

module Make (Ground : Testlib.Sig.Ground) = struct
  open Ground
  open Ground.Fiber.Infix

  let nonlin (p0, p1, p2) = 2 * p2 + p2 - 3 * p0 + 5 * p1

  let test_nonlin (module Db : CONNECTION) =
    let rec loop n =
      if n = 0 then Fiber.return () else
      let p = (Random.int 1000, Random.int 1000, Random.int 1000) in
      (Db.find Q.nonlin1 p >>= or_fail >|= fun y -> assert (y = nonlin p))
        >>= fun () ->
      (Db.find Q.nonlin2 p >>= or_fail >|= fun y -> assert (y = nonlin p))
        >>= fun () ->
      loop (n - 1)
    in
    loop 1000

  let test_env (module Db : CONNECTION) =
    Db.find Q.env1 () >>= or_fail >|= fun y -> assert (y = 60)

  let test_cases = [
    "nonlinear", `Quick, test_nonlin;
    "environment", `Quick, test_env;
  ]

end
