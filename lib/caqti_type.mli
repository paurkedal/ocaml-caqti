(* Copyright (C) 2017--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

(** {2 Primitive Field Types}

    The following is normally only needed for drivers and to define new field
    types.  Everything needed for common usage is covered in {!row_types}. *)

type 'a field = ..
(** An extensible type describing primitive SQL types and types which can be
    converted to and from such types.  When adding a new constructor, register
    the coding with {!Field.define_coding} if possible.  Otherwise, the type
    will only work with drivers which handle it themselves.  The shipped drivers
    only handle the constructors listed here. *)

(** Primitive field types handled by the shipped drivers. *)
type _ field +=
  | Bool : bool field
  | Int : int field
  | Int16 : int field
  | Int32 : int32 field
  | Int64 : int64 field
  | Float : float field
  | String : string field
  | Octets : string field
  | Pdate : Ptime.t field
  | Ptime : Ptime.t field
  | Ptime_span : Ptime.span field
  | Enum : string -> string field

(** Facilities for extending and using primitive field types. *)
module Field : sig
  type 'a t = 'a field

  type _ coding = Coding : {
    rep: 'b t;
    encode: 'a -> ('b, string) result;
    decode: 'b -> ('a, string) result;
  } -> 'a coding

  type get_coding = {get_coding: 'a. Caqti_driver_info.t -> 'a t -> 'a coding}

  val define_coding : 'a field -> get_coding -> unit

  val coding : Caqti_driver_info.t -> 'a field -> 'a coding option

  val to_string : 'a t -> string
end

(** {2:row_types Row Types} *)

(** Type descriptor for row types.

    {b Note.} The concrete representation of this type should be considered
    private, including pattern-matching usage; use the below functions for
    compatibility with future versions. *)
type _ t = private
  | Unit : unit t
  | Field : 'a field -> 'a t
  | Option : 'a t -> 'a option t
  | Tup2 : 'a t * 'b t -> ('a * 'b) t
  | Tup3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Tup4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
  | Custom : {
      rep: 'b t;
      encode: 'a -> ('b, string) result;
      decode: 'b -> ('a, string) result;
    } -> 'a t
  | Annot : [`Redacted] * 'a t -> 'a t

(** {!t} with existentially wrapped static type. *)
type any = Any : 'a t -> any

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

val field : 'a field -> 'a t
(** [field ft] is a row of a single field of type [ft]. This function can be
    used when adding new field types; use the below functions otherwise. *)

module Std : Caqti_type_sig.Std with type 'a t := 'a t
(** Standard type descriptors provided as a submodule for easy inclusion. *)

include Caqti_type_sig.Std with type 'a t := 'a t
