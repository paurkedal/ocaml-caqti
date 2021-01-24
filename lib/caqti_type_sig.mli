(* Copyright (C) 2018  Petter A. Urkedal <paurkedal@gmail.com>
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
  (** An absolute time with driver-dependent precision. This corresponds to an
      SQL [timestamp] type with UTC time zone. *)

  val ptime_span : Ptime.span t
  (** A period of time. If the database lacks a dedicated representation, the
      integer number of seconds is used. *)
end
