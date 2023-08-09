(* Copyright (C) 2023  Petter A. Urkedal <paurkedal@gmail.com>
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

module Log = (val Logs.src_log (Logs.Src.create "caqti.config"))

type t = {
  max_size: int option;
  max_idle_size: int option;
  max_idle_age: Mtime.Span.t option option;
  max_use_count: int option option;
}

type _ key =
  | Max_size : int key
  | Max_idle_size : int key
  | Max_idle_age : Mtime.Span.t option key
  | Max_use_count : int option key

type any_key = Any : _ key -> any_key

let keys = [
  Any Max_size;
  Any Max_idle_size;
  Any Max_idle_age;
  Any Max_use_count;
]

let create
      ?max_size
      ?max_idle_size
      ?max_idle_age
      ?max_use_count
      () =
  {max_size; max_idle_size; max_idle_age; max_use_count}

let option_of_string f = function
 | "" | "none" -> None
 | s -> Some (f s)

let mtime_span_of_string s =
  let x = float_of_string s in
  (match Mtime.Span.of_float_ns (x *. 1e9) with
   | None -> failwith "Mtime.Span.of_float_ns"
   | Some x -> x)

let default = create ()

let create_from_env pfx =
  let get conv sfx =
    let var = pfx ^ sfx in
    (match Sys.getenv_opt var with
     | None -> None
     | Some str ->
        (match conv str with
         | value -> Some value
         | exception Failure _ ->
            Log.err (fun m -> m "Failed to parse $%s = %s." var str);
            None))
  in
  {
    max_size = get int_of_string "_MAX_SIZE";
    max_idle_size = get int_of_string "_MAX_IDLE_SIZE";
    max_idle_age = get (option_of_string mtime_span_of_string) "_MAX_IDLE_AGE";
    max_use_count = get (option_of_string int_of_string) "_MAX_USE_COUNT";
  }

let default_from_env () = create_from_env "CAQTI_POOL"

let max_size = Max_size
let max_idle_size = Max_idle_size
let max_idle_age = Max_idle_age
let max_use_count = Max_use_count

let get (type a) (k : a key) config : a option =
  (match k with
   | Max_size -> config.max_size
   | Max_idle_size -> config.max_idle_size
   | Max_idle_age -> config.max_idle_age
   | Max_use_count -> config.max_use_count)

let modify (type a) (k : a key) (v : a option) config =
  (match k with
   | Max_size -> {config with max_size = v}
   | Max_idle_size -> {config with max_idle_size = v}
   | Max_idle_age -> {config with max_idle_age = v}
   | Max_use_count -> {config with max_use_count = v})

let set k v config = modify k (Some v) config
let unset k config = modify k None config

let merge_left cL cR =
  let add acc (Any k) =
    match get k cL with None -> acc | Some v -> (set k v acc)
  in
  List.fold_left add cR keys
