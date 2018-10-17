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

type dialect_tag = [`Mysql | `Pgsql | `Sqlite | `Other]
type sql_dialect_tag = [`Mysql | `Pgsql | `Sqlite]
type parameter_style =
  [ `None
  | `Linear of string
  | `Indexed of (int -> string) ]

type t = {
  index: int;
  uri_scheme: string;
  dialect_tag: dialect_tag;
  parameter_style: parameter_style;
  describe_has_typed_params: bool;
  describe_has_typed_fields: bool;
  can_transact: bool;
  can_pool: bool;
  can_concur: bool;
}

let next_backend_index = ref 0

let create
    ~uri_scheme
    ?(dialect_tag = `Other)
    ?(parameter_style = `None)
    ~can_pool
    ~can_concur
    ~can_transact
    ~describe_has_typed_params
    ~describe_has_typed_fields
    () =
  {
    index = (let i = !next_backend_index in incr next_backend_index; i);
    uri_scheme;
    dialect_tag;
    parameter_style;
    describe_has_typed_params;
    describe_has_typed_fields;
    can_transact;
    can_pool;
    can_concur;
  }

let dummy = create
  ~uri_scheme:"dummy"
  ~can_pool:false ~can_concur:false ~can_transact:false
  ~describe_has_typed_params:false ~describe_has_typed_fields:false ()

let uri_scheme di = di.uri_scheme
let dialect_tag di = di.dialect_tag
let parameter_style di = di.parameter_style
let can_pool di = di.can_pool
let can_concur di = di.can_concur
let can_transact di = di.can_transact
let describe_has_typed_params di = di.describe_has_typed_params
let describe_has_typed_fields di = di.describe_has_typed_fields
