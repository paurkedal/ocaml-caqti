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

type specific = [`Specific of [`Postgresql]]

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

val endpoints : ([> specific], endpoint list) Key.t
val dbname : ([> specific], string) Key.t
val user : ([> specific], string) Key.t
val passfile : ([> specific], string) Key.t
val channel_binding : ([> specific], [`Disable | `Prefer | `Require]) Key.t
val connect_timeout : ([> specific], int) Key.t
val client_encoding : ([> specific], string) Key.t
val options : ([> specific], string) Key.t
val application_name : ([> specific], string) Key.t
val fallback_application_name : ([> specific], string) Key.t
val keepalives : ([> specific], bool) Key.t
val keepalives_idle : ([> specific], int) Key.t
val keepalives_interval : ([> specific], int) Key.t
val keepalives_count : ([> specific], int) Key.t
val tcp_user_timeout_ms : ([> specific], int) Key.t
val replication : ([> specific], [`On | `Database | `Off]) Key.t
val gssencmode : ([> specific], [`Disable | `Prefer | `Require]) Key.t
val sslmode : ([> specific], [`Disable | `Allow | `Prefer | `Require | `Verify_ca | `Verify_full]) Key.t
val sslcompression : ([> specific], bool) Key.t
val sslcert : ([> specific], string) Key.t
val sslkey : ([> specific], string) Key.t
val sslpassword : ([> specific], string) Key.t
val sslrootcert : ([> specific], string) Key.t
val sslcrl : ([> specific], string) Key.t
val sslcrldir : ([> specific], string) Key.t
val sslsni : ([> specific], bool) Key.t
val requirepeer : ([> specific], string) Key.t
val ssl_min_protocol_version : ([> specific], [`TLS_1_0 | `TLS_1_1 | `TLS_1_2 | `TLS_1_3]) Key.t
val ssl_max_protocol_version : ([> specific], [`TLS_1_0 | `TLS_1_1 | `TLS_1_2 | `TLS_1_3]) Key.t
val krbsrvname : ([> specific], string) Key.t
val gsslib : ([> specific], string) Key.t
val service : ([> specific], string) Key.t
val target_session_attrs : ([> specific], [`Any | `Read_write | `Read_only | `Primary | `Standby | `Prefer_standby]) Key.t
val notice_processing : ([> specific], [`Stderr | `Quiet]) Key.t
val use_single_row_mode : ([> specific], bool) Key.t
val endpoint_uri : ([> specific], Uri.t) Key.t

(** {2 Configuration} *)

type _ Driver.id += Driver_id : [`Postgresql] Driver.id

val all : [> `Generic | specific] Key_set.t

(**/**)

module String_map : Map.S with type key = string

val add_uri :
  Uri.t -> [`Generic | `Specific of [`Postgresql]] t ->
  ([`Generic | `Specific of [`Postgresql]] t, Caqti_error.preconnect) result

val extract_conninfo :
  [> specific] Caqti_config_map.t -> string String_map.t
