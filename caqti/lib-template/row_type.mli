(* Copyright (C) 2018--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Database row types, also used for parameters. *)

[@@@alert "-caqti_private"]

open Shims

type annot = [`Redacted] (* TODO: Consider open type. *)

(**/**)
module Private : sig

  type _ t =
    | Field : 'a Field_type.t -> 'a t
    | Option : 'a t -> 'a option t
    | Product : ('i, 'a) Constructor_type.t * 'i * ('i, 'a) product -> 'a t
    | Annot : annot * 'a t -> 'a t
  and (_, _) product =
    | Proj_end : ('a Constructor_type.return, 'a) product
    | Proj : 'b t * ('a -> 'b) * ('i, 'a) product -> ('b -> 'i, 'a) product
end
[@@alert caqti_private
  "This module exposes the internal representation of row types, which may \
   change between minor relases without prior notice."]
(**/**)

(** {2:row_types Row Types} *)

type 'a t = 'a Private.t
(** Type descriptor for row types. *)

type ('i, 'a) product = ('i, 'a) Private.product
(** Type descriptor used for building cartesian products of row types. *)

type any = Any : 'a t -> any
(** {!t} with existentially wrapped static type. *)

val unify : 'a t -> 'b t -> ('a, 'b) Type.eq option
(** If [t1] and [t2] are the same row type representations, then [unify t1 t2]
    is the witness of the unification of their static type parameters, otherwise
    it is [None]. *)

val length : 'a t -> int
(** [length t] is the number of fields used to represent [t]. *)

val pp : Format.formatter -> 'a t -> unit
(** [pp ppf t] prints a human presentation of [t] on [ppf]. *)

val pp_any : Format.formatter -> any -> unit
(** [pp_any ppf t] prints a human presentation of [t] on [ppf]. *)

val show : 'a t -> string
(** [show t] is a human presentation of [t]. *)

val field : 'a Field_type.t -> 'a t
(** [field ft] is a row of a single field of type [ft]. This function can be
    used when adding new field types; use the below functions otherwise. *)

exception Reject of string
(** Implementers of {!val-product} types may raise this exception to signal that
    a conversion cannot be carried out. *)

val product : 'i -> ('i, 'a) product -> 'a t
val product' : ('i, 'a) Constructor_type.t -> 'i -> ('i, 'a) product -> 'a t
val proj : 'b t -> ('a -> 'b) -> ('i, 'a) product -> ('b -> 'i, 'a) product
val proj_end : ('a Constructor_type.return, 'a) product
(** Given a set of projection functions [p1 : t -> t1], ..., [pN : t -> tN] and
    a function [intro : t1 -> ... -> tN -> t] to reconstruct values of [t] from
    the projections,
    {@ocaml skip[
      product intro
        @@ proj t1 p1
        @@ ...
        @@ proj tN pN
        @@ proj_end
    ]}
    defines a Caqti type for [t], which on the database side will be represented
    by a consecutive list of fields corresponding to the types [t1], ..., [tN],
    each of which may be represented by multiple fields.
    That is, [intro [project1 x] ...  [projectN x]] is equivalent to [x]
    according to an enforced or effective abstraction of [t] deemed adequate for
    the application logic.

    [intro] may raise {!Reject} to indicate that a value cannot be constructed
    from the given arguments.
    Projection operators may also raise this exception to indicate that an
    object cannot be represented in the database, e.g. due to an overflow.

    The above only states that [intro] is a left (pseudo-)inverse of the
    projections, which is what matters for a faithful representation of OCaml
    values.
    The opposite (projection functions being the left inverse of [intro]) may
    be relevant if the application needs preserve the database representation
    when updating objects. *)

val enum :
  encode: ('a -> string) ->
  decode: (string -> ('a, string) result) ->
  string -> 'a t
(** [enum ~encode ~decode name] creates an enum type which on the SQL side is
    named [name], with cases which are converted with [encode] and [decode]
    functions. This is implemented in terms of the {!Field_type.Enum} field
    type. *)

val custom :
  encode: ('a -> ('b, string) result) ->
  decode: ('b -> ('a, string) result) ->
  'b t -> 'a t
(** [custom ~encode ~decode rep] creates a custom type represented by [rep],
    where [encode] is used to encode parameters into [rep] and [decode] is used
    to decode result rows from [rep]. *)

(** Standard type descriptors to use for request construction. *)
module type STD = sig

  (** {3 Field Types}

      The following types correspond to what usually fits in a single field of a
      result row or input parameter set. *)

  val bool : bool t
  (** A [bool] mapped to [boolean] on the SQL side if supported, otherwise
      mapped to an integer. *)

  val int : int t
  (** An [int] mapped to a sufficiently wide integer on the SQL side. *)

  val int16 : int t
  (** An [int] mapped to a [smallint] (16 bits) on the SQL side. *)

  val int32 : int32 t
  (** An [int32] mapped to an [integer] (32 bits) on the SQL side. *)

  val int64 : int64 t
  (** An [int64] mapped to a [bigint] (64 bits) on the SQL side. *)

  val float : float t
  (** A [float] mapped to [double precision] or (best alternative) on the SQL
      side. Serialization may be lossy (e.g. base 10 may be used), so even if
      both sides support IEEE 754 double precision numbers, there may be
      discrepancies in the last digits of the binary representaton. *)

  val string : string t
  (** An UTF-8 string. The database should accept UTF-8 if non-ASCII characters
      are present. *)

  val octets : string t
  (** A [string] mapped to whichever type is used to represent binary data on
      the SQL side. *)

  val pdate : Ptime.t t
  (** A time truncated to a date and mapped to the SQL [date] type. *)

  val ptime : Ptime.t t
  (** An absolute time with driver-dependent precision. This corresponds to an
      SQL [timestamp with time zone] or a suitable alternative where not
      available:

        - MariaDB has [datetime] which is similar to the SQL [timestamp] and
          [timestamp] which is similar to the SQL [timestamp with time zone],
          but the driver does not make the distinction.  Caqti sets the session
          time zone to UTC to avoid misinterpretation, since time values are
          passed in both directions without time zones.  Values have microsecond
          precision, but you will need to specify the desired precision in the
          database schema to avoid truncation.

        - PostgreSQL supports this type and it's a good option to avoid any time
          zone issues if used conistently both on the client side, in SQL
          expressions, and in the database schema.
          Note that [timestamp with time zone] is stored as UTC without time
          zone, taking up no more space then [timestamp].
          The PostgreSQL [timestamp] type is problematic since how conversions
          work and the manual indicate that it is meant to be a local time, and
          since database columns of this type stores the value without
          conversion to UTC, it becomes prone to time zone changes.
          To mitigate the issue, Caqti sets the time zone of sessions to UTC.

        - Sqlite3 does not have a dedicated type for absolute time.  The date
          and time is sent as strings expressed at the UTC time zone using same
          format that the SQLite {{:https://sqlite.org/lang_datefunc.html}
          datetime} function and [CURRENT_TIMESTAMP] return, except for an
          additional three decimals to achive millisecond precision.

          It might seem better to use standard RFC3339 format, since it is
          accepted by the SQLite functions, but that would misorder some time
          values if mixed with the results of these functions, even just the "Z"
          suffix would misorder values with different precision.

      Date and time values which comes from the database without time zone are
      interpreted as UTC. This is not necessarily correct, and it is highly
      recommended to use SQL types which are transmitted with time zone
      information, even if this is UTC. *)

  val ptime_span : Ptime.span t
  (** A period of time. If the database lacks a dedicated representation, the
      integer number of seconds is used. *)


  (** {3 Composite Types} *)

  val option : 'a t -> 'a option t
  (** [option t] turns a set of fields encoded as [t] into a correspending set
      of nullable fields.  The encoder will encode [None] as into a tuple of
      [NULL] values and the decoder will return [None] if all fields are [NULL].

      If the type [t] itself is [option t'] for some [t'], or contains nested
      tuples and options such that all field types are nested under an option
      type, then it would have been possible to decode an all-[NULL] segment of
      a row as [Some x] where [x] is a corresponding tuple-option-tree
      terminating in [None] values.  The above paragraph resolves this ambiguity
      since it implies that the outermost option possible will be decoded as
      [None]. *)

  val redacted : 'a t -> 'a t
  (** [redacted t] is the same type as [t] but sealed as potentially containing
      sensitive information to be redacted from pretty-printers and logs. *)


  (** {3 Tuple Types}

      As a common case of composite types, constructors for tuples up to 12
      components are predefined here.  Higher tuples can be created with
      {!Row_type.val-product}. *)

  val unit : unit t
  (** A type holding no fields. This is used to pass no parameters and as the
      result for queries which does not return any rows. It can also be nested
      in tuples, in which case it will not contribute to the total number of
      fields. *)

  val t2 : 'a1 t -> 'a2 t -> ('a1 * 'a2) t
  (** Creates a pair type. *)

  val elim_t2 : ('a1 * 'a2) t -> ('a1 t * 'a2 t) option

  val t3 : 'a1 t -> 'a2 t -> 'a3 t -> ('a1 * 'a2 * 'a3) t
  (** Creates a 3-tuple type. *)

  val elim_t3 : ('a1 * 'a2 * 'a3) t -> ('a1 t * 'a2 t * 'a3 t) option

  val t4 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> ('a1 * 'a2 * 'a3 * 'a4) t
  (** Creates a 4-tuple type. *)

  val elim_t4 :
    ('a1 * 'a2 * 'a3 * 'a4) t -> ('a1 t * 'a2 t * 'a3 t * 'a4 t) option

  val t5 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5) t
  (** Creates a 5-tuple type. *)

  val elim_t5 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t) option

  val t6 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) t
  (** Creates a 6-tuple type. *)

  val elim_t6 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t) option

  val t7 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) t
  (** Creates a 7-tuple type. *)

  val elim_t7 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t) option

  val t8 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) t
  (** Creates a 8-tuple type. *)

  val elim_t8 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t * 'a8 t) option

  val t9 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t ->
    'a9 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) t
  (** Creates a 9-tuple type. *)

  val elim_t9 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t * 'a8 t * 'a9 t)
      option

  val t10 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t ->
    'a9 t -> 'a10 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) t
  (** Creates a 10-tuple type. *)

  val elim_t10 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t * 'a8 t * 'a9 t
      * 'a10 t) option

  val t11 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t ->
    'a9 t -> 'a10 t -> 'a11 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11) t
  (** Creates a 11-tuple type. *)

  val elim_t11 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t * 'a8 t * 'a9 t
      * 'a10 t * 'a11 t) option

  val t12 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t ->
    'a9 t -> 'a10 t -> 'a11 t -> 'a12 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11 * 'a12) t
  (** Creates a 12-tuple type. *)

  val elim_t12 :
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10 * 'a11
      * 'a12) t ->
    ('a1 t * 'a2 t * 'a3 t * 'a4 t * 'a5 t * 'a6 t * 'a7 t * 'a8 t * 'a9 t
      * 'a10 t * 'a11 t * 'a12 t) option

end

include STD
