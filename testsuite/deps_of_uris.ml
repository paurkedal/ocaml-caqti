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
module String_set = Set.Make (String)

(* This has been edited to emit (package ...) instead of precise internal
 * dependencies, since the latter does not support runtest with -p. *)

let (^/) p q = if p = "." then q else if q = "." then p else Filename.concat p q
let (%) f g x = f (g x)
let fold_list f = Fun.flip (List.fold_left (Fun.flip f))
let failwithf = Format.kasprintf failwith

let get_install_dir () =
  let rec loop dir sw acc =
    (match Filename.basename dir with
     | "." | "/" -> failwith "Cannot determine toplevel build directory."
     | "_build" -> acc ^/ "_build/install" ^/ sw
     | sw -> loop (Filename.dirname dir) sw (acc ^/ ".."))
  in
  let cwd = Sys.getcwd () in
  loop (Filename.dirname cwd) (Filename.basename cwd) "."

let library_deps public_name =
  let libdir = get_install_dir () ^/ "lib" in
  let priv_name = String.map (function '-' | '.' -> '_' | c -> c) public_name in
  let package, library_dir =
    (match String.split_on_char '.' public_name with
     | [] -> assert false
     | (package :: _) as comps -> (package, String.concat "/" comps))
  in
  [
    libdir ^/ package ^/ "META";
    libdir ^/ library_dir ^/ priv_name ^ ".cma";
    libdir ^/ library_dir ^/ priv_name ^ ".cmxs";
  ]

let plugin_deps public_name =
  let libdir = get_install_dir () ^/ "lib" in
  let plugin_name = String.map (function '.' -> '-' | c -> c) public_name in
  let plugin_meta = libdir ^/ "caqti/plugins" ^/ plugin_name ^/ "META" in
  plugin_meta :: library_deps public_name

let driver_package_of_uri uri =
  (match Uri.scheme uri with
   | Some "mariadb" ->
      "caqti-driver-mariadb"
   | Some ("postgres" | "postgresql") ->
      "caqti-driver-postgresql"
   | Some "pgx" ->
      "caqti-driver-pgx"
   | Some "sqlite3" ->
      "caqti-driver-sqlite3"
   | _ ->
      failwithf "Cannot determine driver dependency for %a." Uri.pp uri)

let package_deps_of_uri uri =
  ["(package " ^ driver_package_of_uri uri ^ ")"]

let library_deps_of_uri uri =
  library_deps "caqti" @ plugin_deps (driver_package_of_uri uri)

let main common_args profile tls_library =
  let is_release = profile = "release" in
  let tls_deps =
    let tls_configured =
      Caqti_connect_config.mem_name "tls" common_args.connect_config
    in
    (match tls_configured, tls_library with
     | false, _ | _, None -> []
     | true, Some tls_library ->
        if is_release then
          ["(package " ^ List.hd (String.split_on_char '.' tls_library) ^ ")"]
        else
          plugin_deps tls_library)
  in
  let deps_of_uri =
    if is_release then package_deps_of_uri else library_deps_of_uri
  in
  String_set.of_list tls_deps
    |> fold_list (fold_list String_set.add % deps_of_uri) common_args.uris
    |> String_set.elements
    |> String.concat " "
    |> Printf.printf "(%s)\n"

let main_cmd =
  let open Cmdliner in
  let profile =
    let docv = "profile-name" in
    Arg.(value @@ opt string "dev" @@ info ~docv ["profile"])
  in
  let tls_library =
    let docv = "public-library-name" in
    Arg.(value @@ opt (some string) None @@ info ~docv ["tls-library"])
  in
  Cmd.v (Cmd.info "deps_of_uris")
    Term.(const main $ Testlib.common_args () $ profile $ tls_library)

let () =
  exit (Cmdliner.Cmd.eval main_cmd)
