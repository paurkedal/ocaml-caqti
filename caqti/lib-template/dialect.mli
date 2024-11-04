(* Copyright (C) 2024  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Identification of SQL Dialects and Related Information *)

type t = ..
(** This type identifies the SQL dialect and other differences which may be
    relevant when composing query strings.  This is an open type to allow future
    additions, either defined below or externally.  Each case has the form of a
    constructor identifying the software which interprets the SQL code and its
    version number, if available, followed by any backend-specific details. *)

type t += private
  | Pgsql of {
      server_version: Version.t;
      (** The version number of the server, currently only available when using
          caqti-driver-postgresql. *)
      client_library: [`postgresql | `pgx];
      (** Which client library is being used to communicate with the server. *)
    }
    (** Identifies the backend as a PostgreSQL server. *)
  | Mysql of {
      server_version: Version.t;
      (** The version number of the server, but curretly unavailable, awaiting
          ocaml-mariadb support. *)
    }
    (** Identifies the backend as a MariaDB or MySQL server. No information is
        currently provided about the variant and version. *)
  | Sqlite of {
      server_version: Version.t;
      (** The version number of the Sqlite3 library. *)
    }
    (** Identifies the backend as an Sqlite3 library. *)
  | Unknown of {
      purpose: [`Dummy | `Printing];
    }
    (** The query is to be used for logging or other display purposes, and no
        information is been provided about a potential SQL backend. *)

(**/**)

val create_pgsql :
  server_version: Version.t ->
  client_library: [`postgresql | `pgx] ->
  unit -> t
[@@alert caqti_private "Private function for used by Caqti drivers."]

val create_mysql : server_version: Version.t -> unit -> t
[@@alert caqti_private "Private function for used by Caqti drivers."]

val create_sqlite : server_version: Version.t -> unit -> t
[@@alert caqti_private "Private function for used by Caqti drivers."]

val create_unknown : purpose: [`Dummy | `Printing] -> unit -> t
[@@alert caqti_private "Private function for used by Caqti drivers."]
