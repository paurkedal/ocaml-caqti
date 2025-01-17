(* Copyright (C) 2021--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

  let select_two = static T.(unit -->! int) "SELECT 2"
  let select_twice = static T.(int -->! int) "SELECT 2 * ?"

  let sleep =
    static_gen T.(unit -->! option int) @@ function
     | D.Pgsql _ -> Q.parse "SELECT pg_sleep(2)"
     | _ -> Q.parse "SELECT sleep(2)"
end

module Make (Ground : Testlib.Sig.Ground) = struct
  open Ground
  open Ground.Fiber.Infix

  let test_raise_in_call fail' (module Db : CONNECTION) =
    let test i =
      Fiber.catch
        (fun () ->
          Db.call ~f:(fun _ -> fail' Not_found) Q.select_two ()
            >|= fun _ -> assert false)
        (function
         | Not_found -> Fiber.return ()
         | exn -> failwith ("unexpected exception: " ^ Printexc.to_string exn))
        >>= fun () ->
      Db.find Q.select_twice i
        >|= (function Ok j -> assert (j = 2 * i) | _ -> assert false)
    in
    test 3 >>= fun () ->
    test 5 >>= fun () ->
    test 15

  let test_statement_timeout (module Db : CONNECTION) =
    if Caqti_driver_info.dialect_tag Db.driver_info = `Sqlite then
      Fiber.return () (* Does not support statement timeout. *)
    else
    let check_timed_out = function
      | Error (`Request_failed _) -> ()
      | Error err -> failwith ("unexpected error: " ^ Caqti_error.show err)
      | Ok _ -> assert false
    in
    let test i =
      Db.find Q.sleep () >|= check_timed_out >>= fun () ->
      Db.find Q.select_twice i
        >|= (function Ok j -> assert (j = 2 * i) | _ -> assert false)
    in
    Db.set_statement_timeout (Some 0.1) >>= or_fail >>= fun () ->
    test 3 >>= fun () ->
    test 5 >>= fun () ->
    Db.set_statement_timeout None >>= or_fail

  let test_cases = [
    "raise in call", `Quick, test_raise_in_call raise;
    "fail in call", `Quick, test_raise_in_call Fiber.fail;
    "statement timeout", `Quick, test_statement_timeout;
  ]
end
