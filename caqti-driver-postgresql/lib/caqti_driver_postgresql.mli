(* Copyright (C) 2017--2020  Petter A. Urkedal <paurkedal@gmail.com>
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

(** PostgreSQL driver for Caqti (bindings).

    This driver is implemented in terms of the postgresql OPAM package which
    provides bindings for the PostgreSQL C client library.

    It handles URIs of the form
    {[
      postgresql://<user>:<password>@<host>:<port>/<rest>
    ]}
    which are passed verbatim to {!Postgresql.connection}, and URIs of the form
    [postgresql://<query>] which are first split into [<key> = '<value>'] form.

    Additionally the query option [notice_processing] can be set to [quiet] (the
    default) to suppress notices or to [stderr] to emit notices to standard
    error.  This option is interpreted by the Caqti driver and stripped off
    before passing the URI to libpq.

    The interface provided by this module {e should normally not be used by
    applications}, but provides access to some PostgreSQL specifics in case they
    are needed. *)

module Config_keys = Config_keys

(** {1 Error Details}

    The following gives access to diagnostics collected from the PostgreSQL
    connection and result objects. *)

type Caqti_error.msg +=
  | Connect_error_msg of {
      error: Postgresql.error;
        (** The exception raised by postgresql-ocaml. *)
    }
    (** An exception was raised while attempting to connect to the database. *)

  | Connection_error_msg of {
      error: Postgresql.error;
        (** The exception raised by postgresql-ocaml. *)
      connection_status: Postgresql.connection_status;
        (** The connection status reported by [PQstatus]. *)
    }
    (** An exception was raised while operating on the database connection. *)

  | Result_error_msg of {
      error_message: string;
        (** The error message from [PQresultErrorMessage]. *)
      sqlstate: string;
        (** The SQLSTATE error field. *)
    }
    (** An error was reported on the result from a database operation. *)
