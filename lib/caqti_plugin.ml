(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

open Printf

exception Plugin_missing of string list
exception Plugin_invalid of string * string

let rec iter_chopped_from f i s =
  let n = String.length s in
  if i < n then begin
    let j = try String.index_from s i ' ' with Not_found -> n in
    if i < j then f (String.sub s i (j - i));
    iter_chopped_from f (j + 1) s
  end

let iter_chopped f s = iter_chopped_from f 0 s

let processed_packages = Hashtbl.create 11

let rec depth_process_package f pkg =
  if not (Hashtbl.mem processed_packages pkg) then begin
    Hashtbl.add processed_packages pkg ();
    let requires = try Findlib.package_property [] pkg "requires"
		   with Not_found -> "" in
    iter_chopped (depth_process_package f) requires;
    f pkg
  end

(* Skip libraries on which we already depend; some lacks installed shared
 * objects, anyway.  However, the OCaml linker includes only modules needed by
 * the main application, so add bring in some modules from linked libraries
 * which may be needed by plugins. *)
let () = depth_process_package (fun _ -> ()) "caqti"
module Link_also = struct
  module M2 = CalendarLib
  module M3 = Caqti_prereq
  module M4 = Caqti_pool
end

let findlib_load =
  depth_process_package begin fun pkg ->
    let open Findlib in
    let co = if Dynlink.is_native then "native" else "byte" in
    let dir = package_directory pkg in
    try
      let arch = package_property [co; "plugin"] pkg "archive" in
      Dynlink.loadfile (Filename.concat dir arch)
    with Not_found -> ()
  end

let invalid_f pkg fmt =
  ksprintf (fun msg -> raise (Plugin_invalid (pkg, msg))) fmt

let rec load_plugin = function
  | [] -> None
  | pkg :: pkgs ->
    begin try findlib_load pkg; Some pkg with
    | Findlib.No_such_package _ -> load_plugin pkgs
    | Dynlink.Error err ->
      invalid_f pkg "Failed to link plugin: %s" (Dynlink.error_message err)
    end

let ensure_plugin extract pkgs =
  match extract () with
  | Some x -> x
  | None ->
    begin match load_plugin pkgs with
    | Some pkg ->
      begin match extract () with
      | Some x -> x
      | None ->
	invalid_f pkg "The plugin did not provide the expected functionality."
      end
    | None -> raise (Plugin_missing pkgs)
    end
