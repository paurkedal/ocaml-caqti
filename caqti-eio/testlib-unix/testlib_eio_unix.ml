(* Copyright (C) 2022--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

include Caqti_eio.System
include Caqti_eio
include Caqti_eio_unix

let fail = raise

let (>>=?) x f = match x with Ok x -> f x | Error _ as r -> r
let (>|=?) x f = match x with Ok x -> Ok (f x) | Error _ as r -> r

module Alcotest_cli =
  Testlib.Make_alcotest_cli
    (Alcotest.Unix_platform)
    (Alcotest_engine.Monad.Identity)

module List_result_future = struct
  let rec iter_s f = function
   | [] -> return (Ok ())
   | x :: xs -> f x >>=? fun () -> iter_s f xs
end
