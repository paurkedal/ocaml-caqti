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

let foldopt f acc = Option.fold ~none:Fun.id ~some:f acc

let update
      ?max_size
      ?max_idle_size
      ?max_idle_age
      ?max_use_count
      c =
  c |> foldopt (fun max_size c -> {c with max_size}) max_size
    |> foldopt (fun max_idle_size c -> {c with max_idle_size}) max_idle_size
    |> foldopt (fun max_idle_age c -> {c with max_idle_age}) max_idle_age
    |> foldopt (fun max_use_count c -> {c with max_use_count}) max_use_count

let default = create ()

let max_size c = c.max_size
let max_idle_size c = c.max_idle_size
let max_idle_age c = c.max_idle_age
let max_use_count c = c.max_use_count
