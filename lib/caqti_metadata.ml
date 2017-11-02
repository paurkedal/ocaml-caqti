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
let internal__default_dialect_tag = `Other
type sql_dialect_tag = [`Mysql | `Pgsql | `Sqlite]

type parameter_style =
  [ `None
  | `Linear of string
  | `Indexed of (int -> string) ]

type backend_info = {
  bi_index : int;
  bi_uri_scheme : string;
  bi_dialect_tag : dialect_tag;
  bi_parameter_style : parameter_style;
  bi_default_max_pool_size : int;
  bi_describe_has_typed_parameters : bool;
  bi_describe_has_typed_fields : bool;
  bi_has_transactions : bool;
  internal__can_concur : bool;
}
