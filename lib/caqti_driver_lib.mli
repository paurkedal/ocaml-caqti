(* Copyright (C) 2017--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

(** {b Internal:} Library for Drivers *)

val linear_param_length : Caqti_query.t -> int
(** [linear_param_length templ] is the number of linear parameters expected by a
    query represented by [templ]. *)

val linear_param_order : Caqti_query.t -> int list list * (int * string) list
(** [linear_param_order templ] describes the parameter bindings expected for
    [templ] after linearizing parameters and lifting quoted strings out of the
    query:

      - The first is a list where item number [i] is a list of linear parameter
        positions to which to bind the [i]th incoming parameter.

      - The second is a list of pairs where the first component is the position
        of the linear parameter taking the place of a quoted string, and the
        second component is the quoted string to bind to this parameter.

    All positions are zero-based. *)

val linear_query_string : Caqti_query.t -> string
(** [linear_query_string templ] is [templ] where ["?"] is substituted for
    parameters and quoted strings. *)
