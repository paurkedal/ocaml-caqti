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

open Caqti_config_map

type specific = [`Specific of [`Mariadb]]

(** {2 Connection Parameters} *)

val socket : ([> specific], string) Key.t
val host : ([> specific], string) Key.t
val port : ([> specific], int) Key.t
val user : ([> specific], string) Key.t
val password : ([> specific], string) Key.t
val dbname : ([> specific], string) Key.t

(** {2 Client Options} *)

val connect_timeout : ([> specific], int) Key.t
val compress : ([> specific], bool) Key.t
val init_command : ([> specific], string) Key.t
val read_default_file : ([> specific], string) Key.t
val read_default_group : ([> specific], string) Key.t
val set_charset_dir : ([> specific], string) Key.t
val set_charset_name : ([> specific], string) Key.t
val local_inifile : ([> specific], bool) Key.t
val shared_memory_base_name : ([> specific], string) Key.t
val read_timeout : ([> specific], int) Key.t
val write_timeout : ([> specific], int) Key.t
val secure_auth : ([> specific], bool) Key.t
val report_data_truncation : ([> specific], bool) Key.t
val reconnect : ([> specific], bool) Key.t
val ssl_verify_server_cert : ([> specific], bool) Key.t
val plugin_dir : ([> specific], string) Key.t
val default_auth : ([> specific], string) Key.t
val bind : ([> specific], string) Key.t
val ssl_key : ([> specific], string) Key.t
val ssl_cert : ([> specific], string) Key.t
val ssl_ca : ([> specific], string) Key.t
val ssl_capath : ([> specific], string) Key.t
val ssl_cipher : ([> specific], string) Key.t
val ssl_crl : ([> specific], string) Key.t
val ssl_crlpath : ([> specific], string) Key.t
val server_public_key : ([> specific], string) Key.t
val enable_cleartext_plugin : ([> specific], bool) Key.t

(** {2 Configuration} *)

type _ Driver.id += Driver_id : [`Mariadb] Driver.id

val all : [> `Generic | specific] Key_set.t

(**/**)
val add_uri :
  Uri.t -> [`Generic | `Specific of [`Mariadb]] t ->
  ([`Generic | `Specific of [`Mariadb]] t, Caqti_error.preconnect) result

val extract_client_options :
  [> specific] t -> Mariadb.Blocking.client_option list
