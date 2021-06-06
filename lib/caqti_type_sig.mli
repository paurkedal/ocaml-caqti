(* Copyright (C) 2018--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signatures for {!Caqti_type}. *)

(** Standard type descriptors. *)
module type Std = sig

  type 'a t

  (** {3 Composite}

      The following provides constructors for narrow tuple types; to describe
      wider tuple types, use nested application. *)

  val option : 'a t -> 'a option t

  val unit : unit t
  (** A type holding no fields. This is used to pass no parameters and as the
      result for queries which does not return any rows. It can also be nested
      in tuples, in which case it will not contribute to the total number of
      fields. *)

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
      where [encode] is used to encode parameters into [rep] and [decode] is
      used to decode result rows from [rep].

      {b Note.} This should be considered experimental and may be revised or
      removed in a future version. *)

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
end
