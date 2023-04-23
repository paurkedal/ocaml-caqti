(* Copyright (C) 2021--2023  Petter A. Urkedal <paurkedal@gmail.com>
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
  open Caqti_request.Infix
  open Caqti_type.Std

  let select_two = unit -->! int @:- "SELECT 2"
  let select_twice = int -->! int @:- "SELECT 2 * ?"

  let sleep =
    unit -->! option int @@:- function
     | `Pgsql -> "SELECT pg_sleep(2)"
     | _ -> "SELECT sleep(2)"
end

module Make (Ground : Testlib.Sig.Ground) = struct
  open Ground

  let test_raise_in_call fail' (module Db : CONNECTION) =
    let test i =
      catch
        (fun () ->
          Db.call ~f:(fun _ -> fail' Not_found) Q.select_two ()
            >|= fun _ -> assert false)
        (function
         | Not_found -> return ()
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
      return () (* Does not support statement timeout. *)
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
    "fail in call", `Quick, test_raise_in_call fail;
    "statement timeout", `Quick, test_statement_timeout;
  ]
end
