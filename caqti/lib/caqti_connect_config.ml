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

(* Map Implementation *)

type 'a tag = ..

let int_of_tag tag = Obj.Extension_constructor.(id (of_val tag))

module type KEY = sig
  type value
  type 'a tag += Tag : value tag
  val name : string [@@warning "-32"]
  val default : value
end

type 'a key = (module KEY with type value = 'a)

let create_key (type a) name (default : a) : a key =
  let module Key = struct
    type value = a
    type _ tag += Tag : value tag
    let name = name
    let default = default
  end in
  (module Key : KEY with type value = a)

module Int_map = Map.Make (Int)

type binding = Binding : 'a tag * 'a -> binding

type t = binding Int_map.t

let default = Int_map.empty

let get : type a. a key -> t -> a = fun (module Key) m ->
  (match Int_map.find_opt (int_of_tag Key.Tag) m with
   | Some (Binding (Key.Tag, v)) -> v
   | _ -> Key.default)

let set : type a. a key -> a -> t -> t = fun (module Key) v m ->
  Int_map.add (int_of_tag Key.Tag) (Binding (Key.Tag, v)) m

let reset : type a. a key -> t -> t = fun (module Key) m ->
  Int_map.remove (int_of_tag Key.Tag) m

(* Configuration Keys *)

let tweaks_version : (int * int) key = create_key "tweaks_version" (1, 7)
