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

open Caqti_platform
open Caqti_config_map

let (|>?) = Result.bind

type specific = [`Specific of [`Mariadb]]

let socket = Key.create Conv.string "socket"
let host = Key.create Conv.string "host"
let port = Key.create Conv.int "port"
let user = Key.create Conv.string "user"
let password = Key.create Conv.string "password"
let dbname = Key.create Conv.string "dbname"
(* TODO: flags *)

let connect_timeout = Key.create Conv.int "connect_timeout"
let compress = Key.create Conv.bool "compress"
(* Omitted: named_pipe *)
let init_command = Key.create Conv.string "init_command"
let read_default_file = Key.create Conv.string "read_default_file"
let read_default_group = Key.create Conv.string "read_default_group"
let set_charset_dir = Key.create Conv.string "set_charset_dir"
let set_charset_name = Key.create Conv.string "set_charset_name"
let local_inifile = Key.create Conv.bool "local_inifile"
(* Omitted: protocol *)
let shared_memory_base_name = Key.create Conv.string "shared_memory_base_name"
let read_timeout = Key.create Conv.int "read_timeout"
let write_timeout = Key.create Conv.int "write_timeout"
let secure_auth = Key.create Conv.bool "secure_auth"
let report_data_truncation = Key.create Conv.bool "report_data_truncation"
let reconnect = Key.create Conv.bool "reconnect"
let ssl_verify_server_cert = Key.create Conv.bool "ssl_verify_server_cert"
let plugin_dir = Key.create Conv.string "plugin_dir"
let default_auth = Key.create Conv.string "default_auth"
let bind = Key.create Conv.string "bind"
let ssl_key = Key.create Conv.string "ssl_key"
let ssl_cert = Key.create Conv.string "ssl_cert"
let ssl_ca = Key.create Conv.string "ssl_ca"
let ssl_capath = Key.create Conv.string "ssl_capath"
let ssl_cipher = Key.create Conv.string "ssl_cipher"
let ssl_crl = Key.create Conv.string "ssl_crl"
let ssl_crlpath = Key.create Conv.string "ssl_crlpath"
(* Omitted: connect_attr *)
let server_public_key = Key.create Conv.string "server_public_key"
let enable_cleartext_plugin = Key.create Conv.bool "enable_cleartext_plugin"

type 'a key_with_extract =
  Client_key :
    ('a, 'b) Key.t * ('b -> Mariadb.Blocking.client_option) ->
    'a key_with_extract

let all_with_extract = [
  Client_key (connect_timeout, (fun x -> Connect_timeout x));
  Client_key (compress, (fun x -> assert x; Compress));
  Client_key (init_command, (fun x -> Init_command x));
  Client_key (read_default_file, (fun x -> Read_default_file x));
  Client_key (read_default_group, (fun x -> Read_default_group x));
  Client_key (set_charset_dir, (fun x -> Set_charset_dir x));
  Client_key (set_charset_name, (fun x -> Set_charset_name x));
  Client_key (local_inifile, (fun x -> Local_infile x));
  Client_key (shared_memory_base_name, (fun x -> Shared_memory_base_name x));
  Client_key (read_timeout, (fun x -> Read_timeout x));
  Client_key (write_timeout, (fun x -> Write_timeout x));
  Client_key (secure_auth, (fun x -> Secure_auth x));
  Client_key (report_data_truncation, (fun x -> Report_data_truncation x));
  Client_key (reconnect, (fun x -> Reconnect x));
  Client_key (ssl_verify_server_cert, (fun x -> Ssl_verify_server_cert x));
  Client_key (plugin_dir, (fun x -> Plugin_dir x));
  Client_key (default_auth, (fun x -> Default_auth x));
  Client_key (bind, (fun x -> Bind x));
  Client_key (ssl_key, (fun x -> Ssl_key x));
  Client_key (ssl_cert, (fun x -> Ssl_cert x));
  Client_key (ssl_ca, (fun x -> Ssl_ca x));
  Client_key (ssl_capath, (fun x -> Ssl_capath x));
  Client_key (ssl_cipher, (fun x -> Ssl_cipher x));
  Client_key (ssl_crl, (fun x -> Ssl_crl x));
  Client_key (ssl_crlpath, (fun x -> Ssl_crlpath x));
  Client_key (server_public_key, (fun x -> Server_public_key x));
  Client_key (enable_cleartext_plugin, (fun x -> Enable_cleartext_plugin x));
]

let all =
  let add_key (Client_key (k, _)) = Key_set.add (Key.Any k) in
  Caqti_config_keys.all |> List_ext.fold add_key all_with_extract

let add_uri uri config =
  (match Uri.path uri with
   | "" | "/" -> Ok None
   | path ->
      if Filename.dirname path <> "/" then
        let msg = Caqti_error.Msg "Bad URI path." in
        Error (Caqti_error.connect_rejected ~uri msg)
      else
        Ok (Some (Filename.basename path))) |>? fun db ->
  config
    |> Option.fold ~none:Fun.id ~some:(add socket)
        (Uri.get_query_param uri "socket")
    |> Option.fold ~none:Fun.id ~some:(add host) (Uri.host uri)
    |> Option.fold ~none:Fun.id ~some:(add user) (Uri.user uri)
    |> Option.fold ~none:Fun.id ~some:(add password) (Uri.password uri)
    |> Option.fold ~none:Fun.id ~some:(add port) (Uri.port uri)
    |> Option.fold ~none:Fun.id ~some:(add read_default_group)
        (Uri.get_query_param uri "config-group")
    |> Caqti_config_map.add_default read_default_group "caqti"
    |> Option.fold ~none:Fun.id ~some:(add dbname) db
    |> Result.ok

let extract_client_options =
  let remove_if_false k config =
    (match find k config with
     | None | Some true -> config
     | Some false -> Caqti_config_map.remove k config)
  in
  let extract_client_option config (Client_key (k, f)) =
    (match find k config with
     | None -> None
     | Some v -> Some (f v))
  in
  fun config ->
    let config = remove_if_false compress config in
    List.filter_map (extract_client_option config) all_with_extract

type _ Driver.id += Driver_id : [`Mariadb] Driver.id

let () = Driver.register ~name:"mariadb" ~keys:all ~add_uri Driver_id
