(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

exception Plugin_missing of string * string
exception Plugin_invalid of string * string

(* Because it lacks a cmxs? *)
module Really_link_CalendarLib = struct include CalendarLib end

let msg_invalid = "The plugin seemed to load, but did not register."

let debug =
  try bool_of_string (Sys.getenv "CAQTI_DEBUG_PLUGIN")
  with Not_found -> false

#if ocaml_version < (4, 04, 0)
let backend_predicates () =
  let ic = open_in Sys.executable_name in
  let is_byte = input_char ic = '#' in
  close_in ic;
  if is_byte then ["byte"] else ["native"]
#else
let backend_predicates () =
  (match Sys.backend_type with
   | Sys.Native -> ["native"]
   | Sys.Bytecode -> ["byte"]
   | Sys.Other _ -> [])
#endif

let init = lazy begin
  Findlib.init ();
  Findlib.record_package_predicates (backend_predicates ());
  List.iter (Findlib.record_package Record_core)
            (Findlib.package_deep_ancestors ["native"] ["caqti"]);
  if debug then
    Printf.eprintf "\
        [DEBUG] Caqti_plugin: recorded_predicates = %s\n\
        [DEBUG] Caqti_plugin: recorded_packages.core = %s\n\
        [DEBUG] Caqti_plugin: recorded_packages.load = %s\n%!"
      (String.concat " " (Findlib.recorded_predicates ()))
      (String.concat " " (Findlib.recorded_packages Record_core))
      (String.concat " " (Findlib.recorded_packages Record_load))
end

let ensure_plugin extract pkg =
  Lazy.force init;
  if debug then
    Printf.eprintf "[DEBUG] Caqti_plugin: requested package: %s\n" pkg;
  (match extract () with
   | Some x -> x
   | None ->
      try
        Fl_dynload.load_packages ~debug [pkg];
        (match extract () with
         | Some x -> x
         | None ->
            raise (Plugin_invalid (pkg, msg_invalid)))
      with
       | Dynlink.Error err ->
          raise (Plugin_missing (pkg, Dynlink.error_message err))
       | Findlib.No_such_package (pkg, info) ->
          raise (Plugin_missing (pkg, info)))
