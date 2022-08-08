(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Caqti driver for MariaDB (bindings).

    This driver is implemented in terms of the mariadb OPAM package.
    It handles URIs of the form
    [mariadb://<user>:<password>@<host>:<port>/?<query>]
    with the following query arguments:

      - [socket=<socket>] requests a UNIX domain socket connection
      - [config-group=<name>] loads additional settings from the section
        [<name>] of the default configuration file. Default: ["caqti"].

    Except for the latter, options are passed to the correspondingly named
    arguments to the [connect] function of MariaDB.
    All parts of the URL are optional, except that the chosen combination must
    suffice to establish a connection.

    The interface provided by this module {e should normally not be used by
    applications}, but provides access to some MariaDB specifics in case they
    are needed. *)

module Config_keys = Config_keys

(** {1 Error Details}

    The following provides access to diagnostics collected from the MariaDB
    connection and statement objects. *)

type Caqti_error.msg += Error_msg of {
  errno: int;
    (** The error number returned by [mysql_errno] or [mysql_stmt_errno]. *)
  error: string;
    (** The error message returned by [mysql_error] or [mysql_stmt_errno]. *)
}
