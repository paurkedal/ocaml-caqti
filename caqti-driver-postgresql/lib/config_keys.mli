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

(** Settings for {!caqti-driver-postgresql}. *)

open Caqti_config_map

(** {2 Settings} *)

type endpoint =
  | Unix of {
      directory: string;
      extension: string option;
    }
  | Inet of {
      hostname: [`host] Domain_name.t option;
      address: Ipaddr.t option;
      port: int option;
    }

val endpoints : ([> `Postgresql], endpoint list) Key.t
val dbname : ([> `Postgresql], string) Key.t
val user : ([> `Postgresql], string) Key.t
val passfile : ([> `Postgresql], string) Key.t
val channel_binding : ([> `Postgresql], [`Disable | `Prefer | `Require]) Key.t
val connect_timeout : ([> `Postgresql], int) Key.t
val client_encoding : ([> `Postgresql], string) Key.t
val options : ([> `Postgresql], string) Key.t
val application_name : ([> `Postgresql], string) Key.t
val fallback_application_name : ([> `Postgresql], string) Key.t
val keepalives : ([> `Postgresql], bool) Key.t
val keepalives_idle : ([> `Postgresql], int) Key.t
val keepalives_interval : ([> `Postgresql], int) Key.t
val keepalives_count : ([> `Postgresql], int) Key.t
val tcp_user_timeout_ms : ([> `Postgresql], int) Key.t
val replication : ([> `Postgresql], [`On | `Database | `Off]) Key.t
val gssencmode : ([> `Postgresql], [`Disable | `Prefer | `Require]) Key.t
val sslmode : ([> `Postgresql], [`Disable | `Allow | `Prefer | `Require | `Verify_ca | `Verify_full]) Key.t
val sslcompression : ([> `Postgresql], bool) Key.t
val sslcert : ([> `Postgresql], string) Key.t
val sslkey : ([> `Postgresql], string) Key.t
val sslpassword : ([> `Postgresql], string) Key.t
val sslrootcert : ([> `Postgresql], string) Key.t
val sslcrl : ([> `Postgresql], string) Key.t
val sslcrldir : ([> `Postgresql], string) Key.t
val sslsni : ([> `Postgresql], bool) Key.t
val requirepeer : ([> `Postgresql], string) Key.t
val ssl_min_protocol_version : ([> `Postgresql], [`TLS_1_0 | `TLS_1_1 | `TLS_1_2 | `TLS_1_3]) Key.t
val ssl_max_protocol_version : ([> `Postgresql], [`TLS_1_0 | `TLS_1_1 | `TLS_1_2 | `TLS_1_3]) Key.t
val krbsrvname : ([> `Postgresql], string) Key.t
val gsslib : ([> `Postgresql], string) Key.t
val service : ([> `Postgresql], string) Key.t
val target_session_attrs : ([> `Postgresql], [`Any | `Read_write | `Read_only | `Primary | `Standby | `Prefer_standby]) Key.t
val notice_processing : ([> `Postgresql], [`Stderr | `Quiet]) Key.t

(** {2 Configuration} *)

type _ Driver.t += Driver : [`Generic | `Postgresql] Driver.t

val all : [> `Generic | `Postgresql] Key_set.t

(**/**)

module String_map : Map.S with type key = string

val extract_conninfo : [> `Postgresql] Caqti_config_map.t -> string String_map.t
