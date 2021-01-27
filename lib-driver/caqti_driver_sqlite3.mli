(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Sqlite3 driver for Caqti (bindings).

    This driver is implemented in terms of the sqlite3 OPAM package, which
    provides bindings for libsqlite3.  It handles URIs of the form
    {[
      sqlite3://<path>?create=<bool>&write=<bool>
    ]}
    where [<path>] is passed to {!Sqlite3.db_open} and the query string is used
    to determine its [mode] parameter.  The [<bool>] parameters take the values
    [true] and [false], and default to [true]. *)
