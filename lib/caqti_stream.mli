(* Copyright (C) 2018--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

module type S = sig
  type +'a future

  type ('a, 'err) t = unit -> ('a, 'err) node future
  and ('a, 'err) node =
    | Nil
    | Error of 'err
    | Cons of 'a * ('a, 'err) t

  val fold :
    f: ('a -> 'state -> 'state) ->
    ('a, 'err) t ->
    'state ->
    ('state, 'err) result future

  val fold_s :
    f: ('a -> 'state -> ('state, 'err) result future) ->
    ('a, 'err) t ->
    'state ->
    ('state, 'err) result future

  val iter_s :
    f:('a -> (unit, 'err) result future) ->
    ('a, 'err) t ->
    (unit, 'err) result future

  val to_rev_list : ('a, 'err) t -> ('a list, 'err) result future

  val to_list : ('a, 'err) t -> ('a list, 'err) result future
end

module type FUTURE = sig
  type +'a future

  val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
  val (>|=) : 'a future -> ('a -> 'b) -> 'b future
  val return : 'a -> 'a future
end

module Make (X : FUTURE) : S with type 'a future := 'a X.future
