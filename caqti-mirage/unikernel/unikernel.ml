(* Copyright (C) 2022--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

open Lwt.Infix
open Lwt.Syntax
let ( let/? ) = Result.bind
let ( let*? ) = Lwt_result.Syntax.( let* )
let ( let+? ) = Lwt_result.Syntax.( let+ )
let ( % ) f g x = f (g x)

let minus_req =
  Caqti_template.Create.(t2 int int ->! int) "SELECT ? - ?"

module Make
  (RANDOM : Mirage_random.S)
  (TIME : Mirage_time.S)
  (PCLOCK : Mirage_clock.PCLOCK)
  (MCLOCK : Mirage_clock.MCLOCK)
  (STACK : Tcpip.Stack.V4V6)
  (DNS : Dns_client_mirage.S) =
struct
  module Caqti_mirage_connect =
    Caqti_mirage.Make (RANDOM) (TIME) (MCLOCK) (PCLOCK) (STACK) (DNS)
  module Logs_reporter = Mirage_logs.Make (PCLOCK)
  module Log = (val Logs_lwt.src_log (Logs.Src.create "main"))

  let pclock_now_opt () = Some (Ptime.v (PCLOCK.now_d_ps ()))

  let connect stack dns =
    let*? config =
      let set_authenticator arg cfg =
        let/? authenticator = X509.Authenticator.of_string arg in
        let authenticator = authenticator pclock_now_opt in
        let tls_cfg = Tls.Config.client ~authenticator () in
        let cfg =
          Caqti_connect_config.set Caqti_tls.Config.client (Some tls_cfg) cfg
        in
        Ok cfg
      in
      Caqti_connect_config.default
        |> Option.fold ~none:Result.ok ~some:set_authenticator
            (Key_gen.x509_authenticator ())
        |> Lwt.return
    in
    let* () = Log.info (fun f -> f "Connecting to the database.") in
    let db_uri = Uri.of_string (Key_gen.database_uri ()) in
    Caqti_mirage_connect.connect ~config stack dns db_uri

  let test (module C : Caqti_lwt.CONNECTION) =
    let+? res = C.find minus_req (22, 17) in
    assert (res = 5)

  let start _random _time _pclock _mclock stack dns =
    Logs.(set_level (Some Info));
    Logs.set_reporter (Logs_reporter.create ());
    begin
      let* () = Log.info (fun f -> f "Running tests.") in
      let*? (module C) = connect stack dns in
      let*? () = test (module C) in
      let+ () = C.disconnect () in
      Ok ()
    end >>= function
     | Ok () -> Log.info (fun f -> f "Done.")
     | Error (`Msg msg) -> Log.err (fun f -> f "%s" msg)
     | Error (#Caqti_error.t as err) ->
        Log.err (fun f -> f "%a" Caqti_error.pp err)
end
