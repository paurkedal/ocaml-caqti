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
  | Int32 : int32 field
  | Int64 : int64 field
  | Float : float field
  | String : string field
  | Octets : string field
  | Pdate : Ptime.t field
  | Ptime : Ptime.t field

(** Facilities for extending and using primitive field types. *)
module Field : sig
  type 'a t = 'a field

  type ex = Ex : 'a t -> ex

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

(** {!t} with existentially wrapped static type. *)
type ex = Ex : 'a t -> ex

val length : 'a t -> int
(** [length t] is the number of fields used to represent [t]. *)

val pp : Format.formatter -> 'a t -> unit
(** [pp ppf t] prints a human presentation of [t] on [ppf]. *)

val pp_ex : Format.formatter -> ex -> unit
(** [pp_ex ppf t] prints a human presentation of [t] on [ppf]. *)

val show : 'a t -> string
(** [show t] is a human presentation of [t]. *)

(** {3 Composite} *)

val option : 'a t -> 'a option t

val unit : unit t
(** A holding no fields. This is used to pass no parameters and as the result
    for queries which does not return any rows. It can also be nested in tuples,
    in which case it will not contribute to the total number of fields. *)

val tup2 : 'a t -> 'b t -> ('a * 'b) t
(** Creates a pair type. *)

val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** Creates a 3-tuple type. *)

val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** Creates a 4-tuple type. *)

val custom :
  encode: ('a -> ('b, string) result) ->
  decode: ('b -> ('a, string) result) ->
  'b t -> 'a t
(** [custom ~encode ~decode rep] creates a custom type represented by [rep],
    where [encode] is used to encode parameters into [rep] and [decode] is used
    to decode result rows from [rep].

    {b Note.} This should be considered experimental and may be revised or
    removed in a future version. *)

(** {3 Singular} *)

val field : 'a field -> 'a t
(** [field ft] is a row of a single field of type [ft]. This function can be
    used when adding new field types; use the below functions otherwise. *)

val bool : bool t
(** A boolean. *)

val int : int t
(** An integer. *)

val int32 : int32 t
(** A 32 bit integer. *)

val int64 : int64 t
(** A 64 bit integer. *)

val float : float t
(** A float. *)

val string : string t
(** An UTF-8 string. The database should accept UTF-8 if non-ASCII characters
    are present. *)

val octets : string t
(** A binary string. *)

val pdate : Ptime.t t
(** A date. This corresponds to the SQL [date] type. *)

val ptime : Ptime.t t
(** An absolute time with driver-dependent precision. This corresponds to an SQL
    [timestamp] type with UTC time zone. *)
