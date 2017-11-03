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

[@@@ocaml.warning "-3"]

type dialect_tag = Caqti_metadata.dialect_tag
type sql_dialect_tag = Caqti_metadata.sql_dialect_tag
type parameter_style = Caqti_metadata.parameter_style

type t = Caqti_metadata.backend_info

let next_backend_index = ref 0

let create
      ~uri_scheme
      ?(dialect_tag = Caqti_metadata.internal__default_dialect_tag)
      ?(parameter_style = `None)
      ~can_pool
      ~can_concur
      ~can_transact
      ~describe_has_typed_params
      ~describe_has_typed_fields
      () =
  Caqti_metadata.{
    bi_index = (let i = !next_backend_index in incr next_backend_index; i);
    bi_uri_scheme = uri_scheme;
    bi_dialect_tag = dialect_tag;
    bi_parameter_style = parameter_style;
    bi_default_max_pool_size = (if can_pool then 8 else 1); (* FIXME: else 0 *)
    bi_describe_has_typed_parameters = describe_has_typed_params;
    bi_describe_has_typed_fields = describe_has_typed_fields;
    bi_has_transactions = can_transact;
    internal__can_concur = can_concur;
  }

let uri_scheme di = di.Caqti_metadata.bi_uri_scheme
let dialect_tag di = di.Caqti_metadata.bi_dialect_tag
let parameter_style di = di.Caqti_metadata.bi_parameter_style
let can_pool di = di.Caqti_metadata.bi_default_max_pool_size > 1
let can_concur di = di.Caqti_metadata.internal__can_concur
let can_transact di = di.Caqti_metadata.bi_has_transactions
let describe_has_typed_params di =
  di.Caqti_metadata.bi_describe_has_typed_parameters
let describe_has_typed_fields di =
  di.Caqti_metadata.bi_describe_has_typed_fields
