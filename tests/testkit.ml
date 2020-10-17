(* Copyright (C) 2015--2020  Petter A. Urkedal <paurkedal@gmail.com>
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
  | None ->
      Uri.of_string
        (try Unix.getenv "CAQTI_URI" with
         | Not_found -> "sqlite3:test.db?busy_timeout=60000")
  | Some uri -> uri

let load_uris () =
  (match open_in "uris.conf" with
   | exception Sys_error _ ->
      failwith "No -u option provided and missing tests/uris.conf."
   | ic ->
      let rec loop acc =
        (match input_line ic with
         | exception End_of_file -> close_in ic; List.rev acc
         | ln ->
            let uri = String.trim ln in
            if String.length uri = 0 || uri.[0] = '#' then
              loop acc
            else
              loop (Uri.of_string uri :: acc)) in
      loop [])

let parse_common_args () =
  Arg.parse common_args
    (fun _ -> raise (Arg.Bad "No positional arguments expected."))
    Sys.argv.(0);
  (match !common_uri_r with
   | Some uri -> [uri]
   | None -> load_uris ())

let init_list n f = (* List.init is available from OCaml 4.6.0 *)
 let rec loop acc i = if i < 0 then acc else loop (f i :: acc) (i - 1) in
 loop [] (n - 1)

let () =
  Random.self_init ();
  (* Needed for bytecode since plugins link against C libraries: *)
  Dynlink.allow_unsafe_modules true
