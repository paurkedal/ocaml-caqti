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

(** (v1) Types and utility functions for query descriptions. *)

(** Identification of a remote type.  These correspond to the types described
    in {!Caqti_sigs.CONNECTION.Param}.  [`Other s] represents an
    backend-specific type [s].  The additional [`Unknown] tag is used by
    backends which are unable to determine parameter types. *)
type typedesc =
  [ `Bool
  | `Int
  | `Float
  | `String
  | `Bytes
  | `Date
  | `Utc
  | `Other of string
  | `Unknown ]

type querydesc = {
  querydesc_params : typedesc array;
  (** The types of the expected parameters for a query.  The number of
      parameters should always correspond to the length of the array, but the
      parameters may be [`Unknown] in case the backend is unable to determine
      them. *)

  querydesc_fields : (string * typedesc) array;
  (** The names and types of components of tuples returned by the query. *)
}
(** The result of {!Caqti_sigs.CONNECTION.describe}. *)

val string_of_typedesc : typedesc -> string
(** An informative textual representation of a type. *)

val string_of_querydesc : querydesc -> string
(** An informative textual description of query parameters and results. *)
