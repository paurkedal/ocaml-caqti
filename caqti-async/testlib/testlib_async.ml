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

open Async_kernel
open Core

type 'a future = 'a Deferred.t

let return = return

let catch f g =
  try_with ~extract_exn:true f >>= function
   | Ok y -> return y
   | Error exn -> g exn

let fail exn = Error.raise (Error.of_exn exn)

let or_fail = function
 | Ok x -> return x
 | Error (#Caqti_error.t as err) ->
    Error.raise (Error.of_exn (Caqti_error.Exn err))

let (>>=) = (>>=)
let (>|=) = (>>|)
let (>>=?) m f = m >>= (function Ok x -> f x | Error _ as r -> return r)
let (>|=?) m f = m >|= (function Ok x -> Ok (f x) | Error _ as r -> r)

module Caqti_sys = Caqti_async

module Pool = Caqti_async.Pool

module Alcotest_cli =
  Testlib.Make_alcotest_cli
    (Alcotest.Unix_platform)
    (struct
      include Deferred
      let bind m f = bind m ~f
      let catch f g =
        try_with ~extract_exn:true f >>= function
         | Ok y -> return y
         | Error exn -> g exn
    end)

module List_result_future = struct
  let rec iter_s f = function
   | [] -> return (Ok ())
   | x :: xs -> f x >>=? fun () -> iter_s f xs
end
