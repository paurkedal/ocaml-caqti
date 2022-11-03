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

(** Connection Options *)

open Caqti_platform
open Caqti_config_map
open Sexplib0
open Sexplib0.Sexp_conv

let ( |>? ) = Result.bind

type specific = [`Specific of [`Postgresql]]

type 'a conv = 'a Conv.t * (string -> 'a -> (string * string) list)

let conninfo_of_atomic f name value = [name, f value]

let bool_conv =
  (Conv.bool, conninfo_of_atomic (function false -> "0" | true -> "1"))
let int_conv =
  (Conv.int, conninfo_of_atomic string_of_int)

let string_conv =
  let escape value =
    let buf = Buffer.create (String.length value) in
    let add_escaped_char = function
     | '\\' -> Buffer.add_string buf {|\\|}
     | '\'' -> Buffer.add_string buf {|\'|}
     | ch -> Buffer.add_char buf ch
    in
    Buffer.add_char buf '\'';
    String.iter add_escaped_char value;
    Buffer.add_char buf '\'';
    Buffer.contents buf
  in
  (Conv.string, conninfo_of_atomic escape)

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

let hostname_of_sexp sexp =
  (match Domain_name.of_string (string_of_sexp sexp) with
   | Ok d ->
      (match Domain_name.host d with
       | Ok h -> h
       | Error (`Msg msg) -> of_sexp_error msg sexp)
   | Error (`Msg msg) ->
      of_sexp_error msg sexp)

let sexp_of_hostname x = sexp_of_string (Domain_name.to_string x)

let ipaddr_of_sexp sexp =
  (match Ipaddr.of_string (string_of_sexp sexp) with
   | Ok ipaddr -> ipaddr
   | Error (`Msg msg) -> of_sexp_error msg sexp)

let sexp_of_ipaddr x = sexp_of_string (Ipaddr.to_string x)

let port_of_sexp sexp =
  (match int_of_sexp sexp with
   | i when 0 <= i && i < 65536 -> i
   | _ ->
      of_sexp_error "port number out of range" sexp
   | exception Failure _ ->
      of_sexp_error "the port number must be an integer" sexp)

let omittable_of_sexp f = function
 | Sexp.Atom "_" -> None
 | sexp -> Some (f sexp)

let sexp_of_omittable f = function
 | None -> Sexp.Atom "_"
 | Some x -> f x

let endpoint_of_sexp sexp =
  (match sexp with
   | Sexp.Atom _ as sexp ->
      of_sexp_error "a list is expected" sexp
   | Sexp.List [Sexp.Atom "unix"; path] ->
      Unix {
        directory = string_of_sexp path;
        extension = None;
      }
   | Sexp.List [Sexp.Atom "unix"; path; ext] ->
      Unix {
        directory = string_of_sexp path;
        extension = Some (string_of_sexp ext);
      }
   | Sexp.List [Sexp.Atom "inet"; hostname] ->
      Inet {
        hostname = omittable_of_sexp hostname_of_sexp hostname;
        address = None;
        port = None;
      }
   | Sexp.List [Sexp.Atom "inet"; hostname; address] ->
      Inet {
        hostname = omittable_of_sexp hostname_of_sexp hostname;
        address = omittable_of_sexp ipaddr_of_sexp address;
        port = None;
      }
   | Sexp.List [Sexp.Atom "inet"; hostname; address; port] ->
      Inet {
        hostname = omittable_of_sexp hostname_of_sexp hostname;
        address = omittable_of_sexp ipaddr_of_sexp address;
        port = omittable_of_sexp port_of_sexp port;
      }
   | Sexp.List _ ->
      of_sexp_error "invalid endpoint specification" sexp)

let sexp_of_endpoint = function
 | Unix {directory; extension = None} ->
    Sexp.List [Sexp.Atom "unix"; Sexp.Atom directory]
 | Unix {directory; extension = Some extension} ->
    Sexp.List [Sexp.Atom "unix"; Sexp.Atom directory; Sexp.Atom extension]
 | Inet {hostname = None; address = None; port = None} ->
    Sexp.List [Sexp.Atom "inet"; Sexp.Atom "localhost"]
 | Inet {hostname = Some hostname; address = None; port = None} ->
    Sexp.List [Sexp.Atom "inet"; sexp_of_hostname hostname]
 | Inet {hostname; address = Some address; port = None} ->
    Sexp.List [
      Sexp.Atom "inet";
      sexp_of_omittable sexp_of_hostname hostname;
      sexp_of_ipaddr address;
    ]
 | Inet {hostname; address; port = Some port} ->
    Sexp.List [
      Sexp.Atom "inet";
      sexp_of_omittable sexp_of_hostname hostname;
      sexp_of_omittable sexp_of_ipaddr address;
      sexp_of_int port;
    ]

let conninfo_of_endpoints _ endpoints =
  let extract_endpoint = function
   | Unix {directory; extension} ->
      fun (acc1, acc2, acc3) ->
        (Some directory :: acc1, None :: acc2, extension :: acc3)
   | Inet {hostname; address; port} ->
      fun (acc1, acc2, acc3) ->
        let host = Option.map Domain_name.to_string hostname in
        let hostaddr = Option.map Ipaddr.to_string address in
        let port = Option.map string_of_int port in
        (host :: acc1, hostaddr :: acc2, port :: acc3)
  in
  let hosts, hostaddrs, ports =
    List_ext.fold extract_endpoint endpoints ([], [], [])
  in
  let add_setting name = function
   | [] -> Fun.id
   | values when List.for_all Stdlib.Option.is_none values -> Fun.id
   | values ->
      let values = List.map (Stdlib.Option.value ~default:"") values in
      List.cons (name, String.concat "," values)
  in
  [] |> add_setting "port" ports
     |> add_setting "hostaddr" hostaddrs
     |> add_setting "host" hosts

let endpoints_conv =
  let of_sexps = List.map endpoint_of_sexp in
  let sexps_of = List.map sexp_of_endpoint in
  (Conv.create_with_args ~of_sexps ~sexps_of "endpoint", conninfo_of_endpoints)

let int_ms_conv =
  let of_sexp sexp =
    let s = string_of_sexp sexp in
    let n = String.length s in
    if n < 3 || s.[n - 2] <> 'm' || s.[n - 1] <> 's' then
      of_sexp_error "ms unit expected" sexp
    else
    (try int_of_string (String.sub s 0 (n - 2)) with
     | Failure _ -> of_sexp_error "invalid time specification" sexp)
  in
  let sexp_of x = Sexp.Atom (string_of_int x ^ "ms") in
  (Conv.create ~of_sexp ~sexp_of "time-ms", conninfo_of_atomic string_of_int)

type channel_binding = [`Disable | `Prefer | `Require ]

let channel_binding_conv : channel_binding conv =
  let of_string = function
   | "disable" -> `Disable
   | "prefer" -> `Prefer
   | "require" -> `Require
   | _ -> failwith "invalid channel binding"
  in
  let to_string = function
   | `Disable -> "disable"
   | `Prefer -> "prefer"
   | `Require -> "require"
  in
  let conv = Conv.create_atomic "channel-binding" ~of_string ~to_string in
  (conv, conninfo_of_atomic to_string)

type replication = [`On | `Database | `Off]

let replication_conv : replication conv =
  let of_string = function
   | "on" -> `On
   | "database" -> `Database
   | "off" -> `Off
   | _ -> failwith "invalid replication mode"
  in
  let to_string = function
   | `On -> "on"
   | `Database -> "database"
   | `Off -> "off"
  in
  let conv = Conv.create_atomic "replication" ~of_string ~to_string in
  (conv, conninfo_of_atomic to_string)

type gssencmode = [`Disable | `Prefer | `Require]

let gssencmode_conv : gssencmode conv =
  let of_string = function
   | "disable" -> `Disable
   | "prefer" -> `Prefer
   | "require" -> `Require
   | _ -> failwith "invalid gssencmode"
  in
  let to_string = function
   | `Disable -> "disable"
   | `Prefer -> "prefer"
   | `Require -> "require"
  in
  let conv = Conv.create_atomic "gssencmode" ~of_string ~to_string in
  (conv, conninfo_of_atomic to_string)

type sslmode =
  [`Disable | `Allow | `Prefer | `Require | `Verify_ca | `Verify_full ]

let sslmode_conv : sslmode conv =
  let of_string = function
   | "disable" -> `Disable
   | "allow" -> `Allow
   | "prefer" -> `Prefer
   | "require" -> `Require
   | "verify-ca" -> `Verify_ca
   | "verify-full" -> `Verify_full
   | _ -> failwith "invalid sslmode"
  in
  let to_string = function
   | `Disable -> "disable"
   | `Allow -> "allow"
   | `Prefer -> "prefer"
   | `Require -> "require"
   | `Verify_ca -> "verify-ca"
   | `Verify_full -> "verify-full"
  in
  let conv = Conv.create_atomic "sslmode" ~of_string ~to_string in
  (conv, conninfo_of_atomic to_string)

type tls_protocol_version = [`TLS_1_0 | `TLS_1_1 | `TLS_1_2 | `TLS_1_3]

let tls_protocol_version_conv : tls_protocol_version conv =
  let of_string = function
   | "TLSv1.0" -> `TLS_1_0
   | "TLSv1.1" -> `TLS_1_1
   | "TLSv1.2" -> `TLS_1_2
   | "TLSv1.3" -> `TLS_1_3
   | _ -> failwith "invalid tls-protocol-version"
  in
  let to_string = function
   | `TLS_1_0 -> "TLSv1.0"
   | `TLS_1_1 -> "TLSv1.1"
   | `TLS_1_2 -> "TLSv1.2"
   | `TLS_1_3 -> "TLSv1.3"
  in
  let conv = Conv.create_atomic "tls-protocol-version" ~of_string ~to_string in
  (conv, conninfo_of_atomic to_string)

type target_session_attrs =
  [`Any | `Read_write | `Read_only | `Primary | `Standby | `Prefer_standby]

let target_session_attrs_conv : target_session_attrs conv =
  let of_string = function
   | "any" -> `Any
   | "read-write" -> `Read_write
   | "read-only" -> `Read_only
   | "primary" -> `Primary
   | "standby" -> `Standby
   | "prefer-standby" -> `Prefer_standby
   | _ -> failwith "invalid target-session-attrs"
  in
  let to_string = function
   | `Any -> "any"
   | `Read_write -> "read-write"
   | `Read_only -> "read-only"
   | `Primary -> "primary"
   | `Standby -> "standby"
   | `Prefer_standby -> "prefer-standby"
  in
  let conv = Conv.create_atomic "target-session-attrs" ~of_string ~to_string in
  (conv, conninfo_of_atomic to_string)

type +'a setting =
  | Setting :
      ('a, 'b) Key.t * (string -> 'b -> (string * string) list) -> 'a setting

let add_setting conv name settings =
  let key = Key.create (fst conv) name in
  (Setting (key, snd conv) :: settings, key)

let acc = []
let acc, endpoints = add_setting endpoints_conv "endpoints" acc
let acc, dbname = add_setting string_conv "dbname" acc
let acc, user = add_setting string_conv "user" acc
let acc, passfile = add_setting string_conv "passfile" acc
let acc, channel_binding = add_setting channel_binding_conv "channel_binding" acc
let acc, connect_timeout = add_setting int_conv "connect_timeout" acc
let acc, client_encoding = add_setting string_conv "client_encoding" acc
let acc, options = add_setting string_conv "options" acc
let acc, application_name = add_setting string_conv "application_name" acc
let acc, fallback_application_name = add_setting string_conv "fallback_application_name" acc
let acc, keepalives = add_setting bool_conv "keepalives" acc
let acc, keepalives_idle = add_setting int_conv "keepalives_idle" acc
let acc, keepalives_interval = add_setting int_conv "keepalives_interval" acc
let acc, keepalives_count = add_setting int_conv "keepalives_count" acc
let acc, tcp_user_timeout_ms = add_setting int_ms_conv "tcp_user_timeout" acc
let acc, replication = add_setting replication_conv "replication" acc
let acc, gssencmode = add_setting gssencmode_conv "gssencmode" acc
let acc, sslmode = add_setting sslmode_conv "sslmode" acc
let acc, sslcompression = add_setting bool_conv "sslcompression" acc
let acc, sslcert = add_setting string_conv "sslcert" acc
let acc, sslkey = add_setting string_conv "sslkey" acc
let acc, sslpassword = add_setting string_conv "sslpassword" acc
let acc, sslrootcert = add_setting string_conv "sslrootcert" acc
let acc, sslcrl = add_setting string_conv "sslcrl" acc
let acc, sslcrldir = add_setting string_conv "sslcrldir" acc
let acc, sslsni = add_setting bool_conv "sslsni" acc
let acc, requirepeer = add_setting string_conv "requirepeer" acc
let acc, ssl_min_protocol_version = add_setting tls_protocol_version_conv "ssl_min_protocol_version" acc
let acc, ssl_max_protocol_version = add_setting tls_protocol_version_conv "ssl_max_protocol_version" acc
let acc, krbsrvname = add_setting string_conv "krbsrvname" acc
let acc, gsslib = add_setting string_conv "gsslib" acc
let acc, service = add_setting string_conv "service" acc
let acc, target_session_attrs = add_setting target_session_attrs_conv "target_session_attrs" acc
let settings = List.rev acc

let notice_processing =
  let of_string = function
   | "quiet" -> `Quiet
   | "stderr" -> `Stderr
   | _ -> failwith "invalid notice_processing"
  in
  let to_string = function
   | `Quiet -> "quiet"
   | `Stderr -> "stderr"
  in
  let conv = Conv.create_atomic ~of_string ~to_string "notice_processing" in
  Key.create conv "notice_processing"

let use_single_row_mode = Key.create Conv.bool "use_single_row_mode"

let endpoint_uri =
  let of_string = Uri.of_string in
  let to_string = Uri.to_string in
  let conv = Conv.create_atomic ~of_string ~to_string "uri" in
  Key.create conv "endpoint_uri"

let all = Caqti_config_keys.all
  |> Key_set.add (Key.Any notice_processing)
  |> Key_set.add (Key.Any endpoint_uri)
  |> Key_set.add (Key.Any use_single_row_mode)
  |> List_ext.fold (fun (Setting (k, _)) -> Key_set.add (Key.Any k)) settings

let pop_uri_param present absent param uri =
  (match Uri.get_query_param uri param with
   | None ->
      Ok (absent, uri)
   | Some value_str ->
      (match present value_str with
       | value -> Ok (value, Uri.remove_query_param uri param)
       | exception Failure msg ->
          let msg = Caqti_error.Msg msg in
          Error (Caqti_error.connect_rejected ~uri msg)))

let parse_notice_processing = function
 | "quiet" -> `Quiet
 | "stderr" -> `Stderr
 | _ -> failwith "Invalid argument for notice_processing."

let add_uri uri config =
  pop_uri_param parse_notice_processing `Quiet "notice_processing" uri
    |>? fun (notice_processing', uri) ->
  pop_uri_param bool_of_string false "use_single_row_mode" uri
    |>? fun (use_single_row_mode', uri) ->
  let config = config
    |> add endpoint_uri uri
    |> add_default notice_processing notice_processing'
    |> add_default use_single_row_mode use_single_row_mode'
  in
  Ok config

type _ Driver.id += Driver_id : [`Postgresql] Driver.id

let () = Driver.register ~name:"postgresql" ~keys:all ~add_uri Driver_id

module String_map = Map.Make (String)

let extract_conninfo config =
  let add_setting (Setting (k, conninfo_of_t)) =
    (match Caqti_config_map.find k config with
     | None -> Fun.id
     | Some v ->
        List_ext.fold
          (fun (k, v) -> String_map.add k v)
          (conninfo_of_t (Key.name k) v))
  in
  String_map.empty |> List_ext.fold add_setting settings
