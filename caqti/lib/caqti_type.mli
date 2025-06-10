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

(** Type descriptors for fields and tuples. *)

[@@@alert "-caqti_private"]
open Caqti_template

exception Reject of string

type ('a, 'b) eq = ('a, 'b) Caqti_template.Shims.Type.eq = Equal : ('a, 'a) eq

(** {2 Primitive Field Types}

    The following is normally only needed for drivers and to define new field
    types.  Everything needed for common usage is covered in {!row_types}. *)

(** Facilities for extending and using primitive field types. *)
module Field : sig
  type 'a t = 'a Field_type.t =
    | Bool : bool t
    | Int : int t
    | Int16 : int t
    | Int32 : int32 t
    | Int64 : int64 t
    | Float : float t
    | String : string t
    | Octets : string t
    | Pdate : Ptime.t t
    | Ptime : Ptime.t t
    | Ptime_span : Ptime.span t
    | Enum : string -> string t

  val unify : 'a t -> 'b t -> ('a, 'b) eq option

  val equal_value : 'a t -> 'a -> 'a -> bool

  val to_string : 'a t -> string

  val pp : Format.formatter -> 'a t -> unit

  val pp_value : Format.formatter -> 'a t * 'a -> unit
end

(** {2:row_types Row Types} *)

type 'a t = 'a Row_type.t
(** Type descriptor for row types. *)

type ('a, 'i) product =
  | Proj_end : ('a, 'a) product
  | Proj : 'b t * ('a -> 'b) * ('a, 'i) product -> ('a, 'b -> 'i) product
(** Type descriptor for building cartesian products of row types. *)

(** {!t} with existentially wrapped static type. *)

type any = Any : 'a t -> any

val unify : 'a t -> 'b t -> ('a, 'b) eq option
(** If [t1] and [t2] are the same row type representations, then [unify t1 t2]
    is the witness of the unification of their static type parameters, otherwise
    it is [None]. *)

val equal_value : 'a t -> 'a -> 'a -> bool
(** [equal_value t] is the equality predicate for values of row type [t]. *)

val length : 'a t -> int
(** [length t] is the number of fields used to represent [t]. *)

val pp : Format.formatter -> 'a t -> unit
(** [pp ppf t] prints a human presentation of [t] on [ppf]. *)

val pp_any : Format.formatter -> any -> unit
(** [pp_any ppf t] prints a human presentation of [t] on [ppf]. *)

val pp_value : Format.formatter -> 'a t * 'a -> unit
(** [pp_value ppf (t, v)] prints a human representation of [v] given the type
    descriptor [t]. This function is meant for debugging; the output is neither
    guaranteed to be consistent across releases nor to contain a complete record
    of the data. *)

val show : 'a t -> string
(** [show t] is a human presentation of [t]. *)

val field : 'a Field.t -> 'a t
(** [field ft] is a row of a single field of type [ft]. This function can be
    used when adding new field types; use the below functions otherwise. *)

module Std : Caqti_type_sig.Std
  with type 'a t := 'a t and type ('a, 'i) product := ('a, 'i) product
(** Standard type descriptors provided as a submodule for easy inclusion. *)

include Caqti_type_sig.Std
  with type 'a t := 'a t and type ('a, 'i) product := ('a, 'i) product
