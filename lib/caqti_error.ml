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

type driver_msg = ..

let driver_msg_pp = Hashtbl.create 7

let define_driver_msg ~pp ec = Hashtbl.add driver_msg_pp ec pp

let pp_driver_msg ppf driver_msg =
  let c = Obj.extension_constructor driver_msg in
  try
    let pp = Hashtbl.find driver_msg_pp c in
    pp ppf driver_msg
  with Not_found ->
    Format.fprintf ppf
      "[FIXME: missing printer for (%s _ : Caqti_error.driver_msg)]"
      (Obj.extension_name c)


type connect_msg = {
  uri : Uri.t;
  msg : driver_msg;
}
type connect = [ `Connect_failed of connect_msg ]

let connect_failed ~uri msg =
  `Connect_failed {uri; msg}

let pp_connect_msg ppf err =
  Format.fprintf ppf "Failed to connect to %a, " Uri.pp_hum err.uri;
  pp_driver_msg ppf err.msg;
  Format.pp_print_char ppf '.'


type request_msg = {
  uri : Uri.t;
  query_string : string;
  msg : driver_msg;
}
type request =
  [ `Request_rejected of request_msg
  | `Request_failed of request_msg ]

let request_rejected ~uri ~query_string msg =
  `Request_rejected {uri; query_string; msg}
let request_failed ~uri ~query_string msg =
  `Request_failed {uri; query_string; msg}

let pp_request_msg ppf ~fmt err =
  Format.fprintf ppf fmt Uri.pp_hum err.uri;
  Format.pp_print_string ppf ", ";
  pp_driver_msg ppf err.msg;
  Format.fprintf ppf ", for query %S." err.query_string


type response_msg = {
  uri : Uri.t;
  query_string : string;
  msg : string;
}
type response = [ `Response_rejected of response_msg ]

let response_rejected ~uri ~query_string msg =
  `Response_rejected {uri; query_string; msg}

let pp_response_msg ppf err =
  Format.fprintf ppf "Unexpected result from %a, %s."
    Uri.pp_hum err.uri err.msg;

type t = [ connect | request | response ]
type request_or_response = [ request | response ]

let uri = function
 | `Connect_failed ({uri; _} : connect_msg) -> uri
 | `Request_rejected ({uri; _} : request_msg) -> uri
 | `Request_failed ({uri; _} : request_msg) -> uri
 | `Response_rejected ({uri; _} : response_msg) -> uri

let pp_hum ppf = function
 | `Connect_failed err ->
    pp_connect_msg ppf err
 | `Request_rejected err ->
    pp_request_msg ppf ~fmt:"Request rejected by %a" err
 | `Request_failed err ->
    pp_request_msg ppf ~fmt:"Request to %a failed" err
 | `Response_rejected err ->
    pp_response_msg ppf err

let to_string_hum err =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  pp_hum ppf err;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
