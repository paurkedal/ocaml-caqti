(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

let common_uri_r = ref None

let common_args =
  [ "-u", Arg.String (fun s -> common_uri_r := Some (Uri.of_string s)),
    "URI Test against URI."; ]

let common_uri () =
  match !common_uri_r with
  | None -> Uri.of_string (try Unix.getenv "CAQTI_URI"
                           with Not_found -> "sqlite3::memory:")
  | Some uri -> uri

let parse_common_args () =
  Arg.parse common_args
    (fun _ -> raise (Arg.Bad "No positional arguments expected."))
    Sys.argv.(0);
  common_uri ()

let () =
  Random.self_init ();
  (* Needed for bytecode since plugins link against C libraries: *)
  Dynlink.allow_unsafe_modules true
