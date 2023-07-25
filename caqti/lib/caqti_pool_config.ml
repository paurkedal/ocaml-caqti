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

let fold_option f value = Option.fold ~none:Fun.id ~some:f value

let set_max_size x config = {config with max_size = Some x}
let set_max_idle_size x config = {config with max_idle_size = Some x}
let set_max_idle_age x config = {config with max_idle_age = Some x}
let set_max_use_count x config = {config with max_use_count = Some x}

let unset_max_size config = {config with max_size = None}
let unset_max_idle_size config = {config with max_idle_size = None}
let unset_max_idle_age config = {config with max_idle_age = None}
let unset_max_use_count config = {config with max_use_count = None}

let merge_left cL cR = cR
  |> fold_option set_max_size cL.max_size
  |> fold_option set_max_idle_size cL.max_idle_size
  |> fold_option set_max_idle_age cL.max_idle_age
  |> fold_option set_max_use_count cL.max_use_count

let get_max_size c = c.max_size
let get_max_idle_size c = c.max_idle_size
let get_max_idle_age c = c.max_idle_age
let get_max_use_count c = c.max_use_count
