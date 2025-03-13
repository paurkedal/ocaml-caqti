(* Copyright (C) 2017--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Internal request-related utilities. *)

open Caqti_template

(** {2 Queries} *)

type linear_param =
  Linear_param : int * 'a Field_type.t * 'a -> linear_param

val linear_param_length : ?subst: Query.subst -> Query.t -> int
(** [linear_param_length templ] is the number of linear parameters expected by a
    query represented by [templ]. *)

val linear_param_order :
  ?subst: Query.subst -> Query.t -> int list list * linear_param list
(** [linear_param_order templ] describes the parameter bindings expected for
    [templ] after linearizing parameters and lifting quoted strings out of the
    query:

      - The first is a list where item number [i] is a list of linear parameter
        positions to which to bind the [i]th incoming parameter.

      - The second is a list of [Linear_param (i, t, v)] where [i] is the
        position of the linear parameter taking the place of a embedded value,
        [t] is its field type, and [v] is the value itself.

    All positions are zero-based. *)

val linear_query_string : ?subst: Query.subst -> Query.t -> string
(** [linear_query_string templ] is [templ] where ["?"] is substituted for
    parameters and quoted strings. *)

(** {2 Parameter Encoding and Row Decoding} *)

val raise_encode_missing :
  uri: Uri.t -> field_type: 'a Field_type.t -> unit -> 'counit
val raise_encode_rejected :
  uri: Uri.t -> typ: 'a Row_type.t -> Caqti_error.msg -> 'counit
val raise_encode_failed :
  uri: Uri.t -> typ: 'a Row_type.t -> Caqti_error.msg -> 'counit
val raise_decode_missing :
  uri: Uri.t -> field_type: 'a Field_type.t -> unit -> 'counit
val raise_decode_rejected :
  uri: Uri.t -> typ: 'a Row_type.t -> Caqti_error.msg -> 'counit
val raise_response_failed :
  uri: Uri.t -> query: string -> Caqti_error.msg -> 'counit
val raise_response_rejected :
  uri: Uri.t -> query: string -> Caqti_error.msg -> 'counit

type 'a field_encoder = {
  write_value: 'b. uri: Uri.t -> 'b Field_type.t -> 'b -> 'a -> 'a;
  write_null: 'b. uri: Uri.t -> 'b Field_type.t -> 'a -> 'a;
}
constraint 'e = [> `Encode_rejected of Caqti_error.coding_error]

val encode_param :
  uri: Uri.t -> 'a field_encoder -> 'b Row_type.t -> 'b -> 'a -> 'a

type 'a field_decoder = {
  read_value: 'b. uri: Uri.t -> 'b Field_type.t -> 'a -> 'b * 'a;
  skip_null: int -> 'a -> 'a option;
}
constraint 'e = [> `Decode_rejected of Caqti_error.coding_error]

val decode_row :
  uri: Uri.t -> 'a field_decoder -> 'b Row_type.t -> 'a -> 'b * 'a

val fresh_name_generator : string -> (unit -> string)
