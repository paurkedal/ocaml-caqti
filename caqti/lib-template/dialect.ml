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

type t = ..

type t +=
  | Pgsql of {
      server_version: Version.t;
      client_library: [`postgresql | `pgx];
    }
  | Mysql of {server_version: Version.t}
  | Sqlite of {server_version: Version.t}
  | Unknown of {purpose: [`Dummy | `Printing]}

let create_pgsql ~server_version ~client_library () =
  Pgsql {server_version; client_library}
let create_mysql ~server_version () = Mysql {server_version}
let create_sqlite ~server_version () = Sqlite {server_version}
let create_unknown ~purpose () = Unknown {purpose}
