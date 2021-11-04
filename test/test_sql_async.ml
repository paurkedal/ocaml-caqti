(* Copyright (C) 2014--2021  Petter A. Urkedal <paurkedal@gmail.com>
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
open Async_unix
open Core_kernel

module Sys = struct
  type 'a future = 'a Deferred.t

  let return = return

  let or_fail = function
   | Ok x -> return x
   | Error (#Caqti_error.t as err) ->
      Error.raise (Error.of_exn (Caqti_error.Exn err))

  module Infix = struct
    let (>>=) = (>>=)
    let (>|=) = (>>|)
  end
end

module Test = Test_sql.Make (Sys) (Caqti_async)

let main uris () =
  let open Deferred in
  let rec loop = function
   | [] -> return ()
   | uri :: uris ->
      Caqti_async.connect uri >>= Sys.or_fail >>= Test.run >>= fun () ->
      (match Caqti_async.connect_pool ~post_connect:Test.post_connect uri with
       | Ok pool ->
          Test.run_pool pool >>= fun () ->
          loop uris
       | Error err ->
          Error.raise (Error.of_exn (Caqti_error.Exn err))) in
  upon (loop uris) (fun () -> Shutdown.shutdown 0)

let () =
  let uris = Testkit.parse_common_args () in
  never_returns (Scheduler.go_main ~main:(main uris) ())
