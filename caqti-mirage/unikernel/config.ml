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

open Mirage

let pin =
  let rec locate_top p =
    let p, b = Filename.(dirname p, basename p) in
    assert (p <> "/");
    if b = "caqti-mirage" then p else locate_top p
  in
  let p =
    if Filename.is_relative Sys.argv.(0)
    then Filename.concat (Sys.getcwd ()) Sys.argv.(0)
    else Sys.argv.(0)
  in
  locate_top p

let pin_version =
  let fh = Unix.open_process_in "git describe" in
  let v = input_line fh in
  (match Unix.close_process_in fh with
   | Unix.WEXITED 0 ->
      let n = String.length v in
      assert (n > 1 && v.[0] = 'v');
      String.sub v 1 (n - 1)
   | _ -> failwith "git describe failed")

let packages = [
  package "caqti" ~pin ~pin_version;
  package "caqti-driver-pgx" ~pin ~pin_version;
  package "caqti-mirage" ~pin ~pin_version;
  package "caqti-lwt" ~pin ~pin_version;
  package "caqti-tls" ~pin ~pin_version;
  package "dns-client-mirage";
  package "logs";
  package "mirage-crypto-rng-mirage";
  package "mirage-clock-unix";
  package "mirage-logs";
]

let stack = generic_stackv4v6 default_network

let nameservers =
  let doc = Key.Arg.info ~doc:"Nameserver." ["nameserver"] in
  Key.(create "nameserver" Arg.(opt_all string doc))

let database_uri =
  let doc =
    Key.Arg.info ~doc:"URI for connecting to the database."
      ["u"; "database-uri"]
  in
  Key.(create "database-uri" Arg.(required string doc))

let x509_authenticator =
  let doc =
    Key.Arg.info ~doc:"X509 authenticator." ["x509-authenticator"]
  in
  (* TODO: Would be good to invoke X509.Authenticator.of_string to check the
   * argument here. How do we specify the x509 dependency? *)
  Key.(create "x509_authenticator" Arg.(opt (some string) None doc))

let keys = [
  Key.v nameservers;
  Key.v database_uri;
  Key.v x509_authenticator;
]

let unikernel_functor =
  foreign "Unikernel.Make" ~keys ~packages
    (random @-> time @-> pclock @-> mclock @-> stackv4v6 @-> dns_client @-> job)

let unikernel =
  unikernel_functor
    $ default_random
    $ default_time
    $ default_posix_clock
    $ default_monotonic_clock
    $ stack
    $ generic_dns_client ~nameservers stack

let () = register "caqti-test-unikernel" [unikernel]
