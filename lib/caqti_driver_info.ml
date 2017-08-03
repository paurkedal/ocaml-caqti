(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

type dialect_tag = [`Mysql | `Pgsql | `Sqlite | `Other]
type sql_dialect_tag = [`Mysql | `Pgsql | `Sqlite]

type parameter_style =
  [ `None
  | `Linear of string
  | `Indexed of (int -> string) ]

type t = {
  index : int;
  uri_scheme : string;
  dialect_tag : dialect_tag;
  parameter_style : parameter_style;
  can_pool : bool;
  can_concur : bool;
  can_transact : bool;
}

let last_index = ref (-1)

let create
      ~uri_scheme
      ?(dialect_tag = `Other)
      ?(parameter_style = `None)
      ~can_pool
      ~can_concur
      ~can_transact
      () =
  incr last_index;
  {
    index = !last_index;
    uri_scheme;
    dialect_tag;
    parameter_style;
    can_pool;
    can_concur;
    can_transact;
  }

let uri_scheme di = di.uri_scheme
let dialect_tag di = di.dialect_tag
let parameter_style di = di.parameter_style
let can_pool di = di.can_pool
let can_concur di = di.can_concur
let can_transact di = di.can_transact
