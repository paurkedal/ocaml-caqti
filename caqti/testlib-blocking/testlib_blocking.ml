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

type 'a future = 'a
let return x = x
let catch f g = try f () with exn -> g exn
let fail = raise
let or_fail = Caqti_blocking.or_fail
let (>>=) x f = f x
let (>|=) x f = f x
let (>>=?) x f = match x with Ok x -> f x | Error _ as r -> r
let (>|=?) x f = match x with Ok x -> Ok (f x) | Error _ as r -> r

module Caqti_sys = Caqti_blocking

module Alcotest_cli =
  Testlib.Make_alcotest_cli
    (Alcotest.Unix_platform)
    (Alcotest_engine.Monad.Identity)
