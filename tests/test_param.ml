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

open Lwt.Infix
open Testkit

let (>>=?) m mf =
  m >>= (function Ok x -> mf x | Error err -> Lwt.return (Error err))
let (>|=?) m f =
  m >|= (function Ok x -> f x | Error err -> Error err)

let nonlin1_q = Caqti_request.create
  Caqti_type.(tup3 int int int) Caqti_type.int Caqti_mult.one @@ fun _ ->
  S[L"SELECT 2 * "; P 2; L" + "; P 2; L" - 3 * "; P 0; L" + 5 * "; P 1]

let nonlin2_q = Caqti_request.find
  Caqti_type.(tup3 int int int) Caqti_type.int
  "SELECT 2 * $3 + $3 - 3 * $1 + 5 * $2"

let nonlin (p0, p1, p2) = 2 * p2 + p2 - 3 * p0 + 5 * p1

let test_nonlin (module Db : Caqti_lwt.CONNECTION) =
  let rec loop n =
    if n = 0 then Lwt.return_ok () else
    let p = (Random.int 1000, Random.int 1000, Random.int 1000) in
    (Db.find nonlin1_q p >|=? fun y -> Ok (assert (y = nonlin p))) >>=? fun() ->
    (Db.find nonlin2_q p >|=? fun y -> Ok (assert (y = nonlin p))) >>=? fun() ->
    loop (n - 1) in
  loop 1000

let env1_q =
  let env _ = function
   | "." -> Caqti_sql.L"100"
   | "fourty" -> Caqti_sql.L"40"
   | _ -> raise Not_found in
  Caqti_request.find ~env Caqti_type.unit Caqti_type.int "SELECT $. - $(fourty)"

let test_env (module Db : Caqti_lwt.CONNECTION) =
  Db.find env1_q () >|=? fun y -> Ok (assert (y = 60))

let test db =
  test_nonlin db >>=? fun () ->
  test_env db

let report_error = function
 | Error err ->
    Lwt_io.eprintl (Caqti_error.show err) >|= fun () -> exit 69
 | Ok () ->
    Lwt.return_unit

let () = Lwt_main.run @@
  Lwt_list.iter_s
    (fun uri -> Caqti_lwt.connect uri >>=? test >>= report_error)
    (parse_common_args ())
