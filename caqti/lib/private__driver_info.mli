(* Copyright (C) 2017--2026  Petter A. Urkedal <paurkedal@gmail.com>
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

[@@@alert caqti_private "For re-export via the caqti.classic library."]

type dialect_tag = private [> `Mysql | `Pgsql | `Sqlite]
type sql_dialect_tag = [`Mysql | `Pgsql | `Sqlite]

type parameter_style = private [>
  | `None
  | `Linear of string
  | `Indexed of (int -> string)
]

type t

val create :
  uri_scheme: string ->
  ?dialect_tag: dialect_tag ->
  ?parameter_style: parameter_style ->
  can_pool: bool ->
  can_concur: bool ->
  can_transact: bool ->
  dummy_dialect: Template.Dialect.t ->
  unit -> t

val dummy : t

val uri_scheme : t -> string
val dialect_tag : t -> dialect_tag
val parameter_style : t -> parameter_style
val can_pool : t -> bool
val can_concur : t -> bool
val can_transact : t -> bool

(* Not re-exported from caqti.classic. *)
val dummy_dialect : t -> Template.Dialect.t
val of_dialect : Template.Dialect.t -> t
