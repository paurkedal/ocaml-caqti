(* Copyright (C) 2025  Petter A. Urkedal <paurkedal@gmail.com>
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

open Shims

type 'a return = ('a, string) result

type ('i, 'j) unifier =
  | Equal : ('i return, 'i return) unifier
  | Assume : (('a, 'b) Type.eq -> ('i, 'j) unifier) ->
      ('a -> 'i, 'b -> 'j) unifier

type (_, _) tag = ..

type ('i, 'a) t = {
  tag: ('i, 'a) tag;
  unify_tag: 'j 'b. ('j, 'b) tag -> ('i, 'j) unifier option;
  construct: 'i;
}

let unify : type i j a b. (i, a) t -> (j, b) t -> (i, j) unifier option =
  fun x y -> x.unify_tag y.tag
