(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** SQL IO utility functions.

    @deprecated New code should instead use {!Caqti_query.angstrom_parser} with
    the appropriate parsing function from the Angstrom library. *)

[@@@deprecated "Use Caqti_query.angstrom_parser."]

(** The concurrency monad assumed by {!Make}. *)
module type MONAD = sig
  type +'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

(** The interface implemented by {!Make}. *)
module type S = sig
  type +'a future

  val read_sql_statement :
    ('a -> char option future) -> 'a -> string option future
  (** [read_sql_statement read_char chan] reads the next semicolon-terminated
      SQL statement from [chan], taking care to skip over quoted semicolons.
      [read_char chan] shall return the next character from [chan], or [None]
      when the end of file has been reached.  A final semicolon is optional and
      any trailing white space after it will be ignored.

      This can be used e.g. to read in SQL schemas or schema updates from a file
      for automatic initialization and updates of tables, sequences, functions,
      views, etc.

      @deprecated New code should instead use {!Caqti_query.angstrom_parser}
      with the appropriate parsing function from the Angstrom library. *)
end

(** The implementation. *)
module Make (Io : MONAD) : S with type 'a future := 'a Io.t
