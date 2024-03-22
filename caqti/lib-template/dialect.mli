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

type t +=
  | Pgsql of {
      server_version_opt: (int * int) option;
      ocaml_library: [`postgresql | `pgx];
      reserved: unit;
    }
    (** Identifies the backend as a PostgreSQL server. The server version is
        currently only available when using caqti-driver-postgresql. *)
  | Mysql of {
      reserved: unit;
    }
    (** Identifies the backend as a MariaDB or MySQL server. No information is
        currently provided about the variant and version. *)
  | Sqlite of {
      server_version: int * int * int;
      reserved: unit;
    }
    (** Identifies the backend as an Sqlite3 library. *)
  | Unknown of {
      reserved: unit;
    }
    (** The query is to be used for logging or other display purposes, and no
        information is been provided about a potential SQL backend. *)
(** These constructors are semi-private; public usage is limited to pattern
    matching without extracting the [reserved] field. *)
