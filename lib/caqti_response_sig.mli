(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signature of a response from a database. *)

module type S = sig
  type 'b io
  type ('b, +'m) t

  val returned_count :
    ('b, 'm) t -> (int, [Caqti_error.t | `Unsupported]) result io

  val affected_count :
    ('b, 'm) t -> (int, [Caqti_error.t | `Unsupported]) result io

  val exec :
    (unit, [< `Zero]) t -> (unit, Caqti_error.t) result io

  val find :
    ('b, [< `One]) t -> ('b, Caqti_error.t) result io

  val find_opt :
    ('b, [< `Zero | `One]) t -> ('b option, Caqti_error.t) result io

  val fold :
    ('b -> 'c -> 'c) ->
    ('b, 'm) t -> 'c -> ('c, Caqti_error.t) result io

  val fold_s :
    ('b -> 'c -> ('c, 'e) result io) ->
    ('b, 'm) t -> 'c -> ('c, [> Caqti_error.t] as 'e) result io

  val iter_s :
    ('b -> (unit, 'e) result io) ->
    ('b, 'm) t -> (unit, [> Caqti_error.t] as 'e) result io
end
