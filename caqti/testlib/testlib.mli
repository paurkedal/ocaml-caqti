(* Copyright (C) 2021  Petter A. Urkedal <paurkedal@gmail.com>
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

module Sig = Sig

type common_args = {
  uris: Uri.t list;
  tweaks_version: (int * int) option;
}

val common_args : common_args Cmdliner.Term.t

val test_name_of_uri : Uri.t -> string

val init_list : int -> (int -> 'a) -> 'a list

module Make_alcotest_cli :
  functor (Platform : Alcotest_engine.Platform.MAKER) ->
  functor (Monad : Alcotest_engine.Monad.S) ->
  Sig.Alcotest_cli with type return = unit Monad.t
