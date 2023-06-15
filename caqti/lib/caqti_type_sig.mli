(* Copyright (C) 2018--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signatures for {!Caqti_type}. *)

(** Standard type descriptors. *)
module type Std = sig

  type 'a t
  type ('a, 'i) product

  (** {3 Composite}

      The following provides constructors for narrow tuple types; to describe
      wider tuple types, use nested application. *)

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

  val unit : unit t
  (** A type holding no fields. This is used to pass no parameters and as the
      result for queries which does not return any rows. It can also be nested
      in tuples, in which case it will not contribute to the total number of
      fields. *)

  val t2 : 'a1 t -> 'a2 t -> ('a1 * 'a2) t
  (** Creates a pair type. *)

  val t3 : 'a1 t -> 'a2 t -> 'a3 t -> ('a1 * 'a2 * 'a3) t
  (** Creates a 3-tuple type. *)

  val t4 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> ('a1 * 'a2 * 'a3 * 'a4) t
  (** Creates a 4-tuple type. *)

  val t5 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5) t
  (** Creates a 5-tuple type. This is implemented in terms of lower tuples. *)

  val t6 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) t
  (** Creates a 6-tuple type. This is implemented in terms of lower tuples. *)

  val t7 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) t
  (** Creates a 7-tuple type. This is implemented in terms of lower tuples. *)

  val t8 :
    'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t ->
    ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) t
  (** Creates a 8-tuple type. This is implemented in terms of lower tuples. *)

  val product : 'i -> ('a, 'i) product -> 'a t
  val proj : 'b t -> ('a -> 'b) -> ('a, 'i) product -> ('a, 'b -> 'i) product
  val proj_end : ('a, 'a) product

  val custom :
    encode: ('a -> ('b, string) result) ->
    decode: ('b -> ('a, string) result) ->
    'b t -> 'a t
  (** [custom ~encode ~decode rep] creates a custom type represented by [rep],
      where [encode] is used to encode parameters into [rep] and [decode] is
      used to decode result rows from [rep]. *)

  val redacted : 'a t -> 'a t
  (** [redacted t] is the same type as [t] but sealed as potentially containing
      sensitive information to be redacted from pretty-printers and logs. *)

  (** {3 Singular} *)

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
          expressions, and in the database schema.  Note that [timestamp with
          time zone] is stored as UTC without time zone, taking up no more space
          then [timestamp].  The PostgreSQL [timestamp] type is problematic
          since how conversions work and the manual indicate that it is meant to
          be a local time, and since database columns of this type stores the
          value without conversion to UTC, it becomes prone to time zone
          changes.  To mitigate the issue, Caqti sets the time zone of sessions
          to UTC.

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

  val enum :
    encode: ('a -> string) ->
    decode: (string -> ('a, string) result) ->
    string -> 'a t
  (** [enum ~encode ~decode name] creates an enum type which on the SQL side is
      named [name], with cases which are converted with [encode] and [decode]
      functions. This is implemented in terms of the {!Caqti_type.Enum} field
      type. *)

  (**/**)
  val tup2 : 'a1 t -> 'a2 t -> ('a1 * 'a2) t
  [@@deprecated "Renamed to t2."]
  val tup3 : 'a1 t -> 'a2 t -> 'a3 t -> ('a1 * 'a2 * 'a3) t
  [@@deprecated "Renamed to t3."]
  val tup4 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> ('a1 * 'a2 * 'a3 * 'a4) t
  [@@deprecated "Renamed to t4."]
end
