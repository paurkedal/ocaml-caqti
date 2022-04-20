(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** PostgreSQL driver for Caqti based on pgx

    This driver is implemented in terms of the pgx library.

    It handles URIs of the form
    {[
      pgx://<user>:<password>@<host-or-directory>:<port>/<database>
    ]}
    where [<host-or-directory>] is either a host name, an IP number, or a
    directory containing the Unix domain socket of a locally running PostgreSQL
    server.  In the latter case, the directory must be percent-encoded, e.g.
    {[
      pgx://jdoe@%2fvar%2frun%2fpostgresql
    ]}

    The interface provided by this module {e should normally not be used by
    applications}, but provides access to some pgx specifics in case they are
    needed. *)

(** {1 Error Details} *)

type Caqti_error.msg += Pgx_msg of string * Pgx.Error_response.t
