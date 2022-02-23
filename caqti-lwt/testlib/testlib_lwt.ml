(* Copyright (C) 2021  Petter A. Urkedal <paurkedal@gmail.com>
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

type 'a future = 'a Lwt.t
let return = Lwt.return
let catch = Lwt.catch
let fail = Lwt.fail
let or_fail = Caqti_lwt.or_fail
let (>>=) = Lwt.Infix.(>>=)
let (>|=) = Lwt.Infix.(>|=)
let (>>=?) = Lwt_result.Infix.(>>=)
let (>|=?) = Lwt_result.Infix.(>|=)

module Caqti_sys = Caqti_lwt

module Alcotest_cli = Testlib.Make_alcotest_cli (Alcotest.Unix_platform) (Lwt)

module List_result_future = struct
  let rec iter_s f = function
   | [] -> return (Ok ())
   | x :: xs -> f x >>=? fun () -> iter_s f xs
end
