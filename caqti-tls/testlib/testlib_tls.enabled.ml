(* Copyright (C) 2024  Petter A. Urkedal <paurkedal@gmail.com>
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

open Testlib

let () =
  let open Cmdliner in
  let x509_authenticator_conv =
    let parse spec =
      X509.Authenticator.of_string spec
        |> Result.map (fun f -> f (fun () -> Some (Ptime_clock.now ())))
    in
    let pp ppf _ = Format.pp_print_string ppf "<authenticator>" in
    Arg.conv (parse, pp)
  in
  let x509_authenticator =
    let doc =
      "X509 authenticator for validating TLS connections when using the tls \
       library.  The input is parsed by X509.Authenticator.of_string"
    in
    let env = Cmd.Env.info "CAQTI_TEST_X509_AUTHENTICATOR" in
    Arg.(value @@ opt (some x509_authenticator_conv) None @@
         info ~doc ~env ["x509-authenticator"])
  in
  let process x509_authenticator common_args =
    (match x509_authenticator with
     | None -> common_args
     | Some authenticator ->
        let tls_client_config = Tls.Config.client ~authenticator () in
        let connect_config =
          common_args.connect_config
            |> Caqti_connect_config.set Caqti_tls.Config.client
                (Some tls_client_config)
        in
        {common_args with connect_config})
  in
  Testlib.register_common_arg Term.(const process $ x509_authenticator)
