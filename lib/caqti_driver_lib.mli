(* Copyright (C) 2017--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** (internal) Library for Drivers *)

val linear_param_length : Caqti_request.query -> int
(** [linear_param_length templ] is the number of linear parameters expected by a
    query represented by [templ]. *)

val linear_param_order : Caqti_request.query -> int list list
(** [linear_param_order templ] is a list where item number [i] is a list of
    positions of the linearized query which refer to the [i]th incoming
    parameter.  Positions are zero-based. *)

val linear_query_string : Caqti_request.query -> string
(** [linear_query_string templ] is [templ] where ["?"] is substituted for
    parameters. *)
