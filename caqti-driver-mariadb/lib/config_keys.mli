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

(** {2 Settings} *)

val connect_timeout : ([> `Mariadb], int) Key.t
val compress : ([> `Mariadb], bool) Key.t
val init_command : ([> `Mariadb], string) Key.t
val read_default_file : ([> `Mariadb], string) Key.t
val read_default_group : ([> `Mariadb], string) Key.t
val set_charset_dir : ([> `Mariadb], string) Key.t
val set_charset_name : ([> `Mariadb], string) Key.t
val local_inifile : ([> `Mariadb], bool) Key.t
val shared_memory_base_name : ([> `Mariadb], string) Key.t
val read_timeout : ([> `Mariadb], int) Key.t
val write_timeout : ([> `Mariadb], int) Key.t
val secure_auth : ([> `Mariadb], bool) Key.t
val report_data_truncation : ([> `Mariadb], bool) Key.t
val reconnect : ([> `Mariadb], bool) Key.t
val ssl_verify_server_cert : ([> `Mariadb], bool) Key.t
val plugin_dir : ([> `Mariadb], string) Key.t
val default_auth : ([> `Mariadb], string) Key.t
val bind : ([> `Mariadb], string) Key.t
val ssl_key : ([> `Mariadb], string) Key.t
val ssl_cert : ([> `Mariadb], string) Key.t
val ssl_ca : ([> `Mariadb], string) Key.t
val ssl_capath : ([> `Mariadb], string) Key.t
val ssl_cipher : ([> `Mariadb], string) Key.t
val ssl_crl : ([> `Mariadb], string) Key.t
val ssl_crlpath : ([> `Mariadb], string) Key.t
val server_public_key : ([> `Mariadb], string) Key.t
val enable_cleartext_plugin : ([> `Mariadb], bool) Key.t

(** {2 Configuration} *)

type _ Driver.t += Driver : [`Generic | `Mariadb] Driver.t

val all : [> `Generic | `Mariadb] Key_set.t

(**/**)
val extract_client_options :
  [> `Mariadb] t -> Mariadb.Blocking.client_option list
