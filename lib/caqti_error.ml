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

type driver_detail = ..

let driver_detail_pp = Hashtbl.create 7

let register_driver_detail ~pp ec = Hashtbl.add driver_detail_pp ec pp

let pp_driver_detail ppf driver_detail =
  let c = Obj.extension_constructor driver_detail in
  try
    let pp = Hashtbl.find driver_detail_pp c in
    pp ppf driver_detail
  with Not_found ->
    Format.fprintf ppf
      "[FIXME: missing printer for (%s _ : Caqti_error.driver_detail)]"
      (Obj.extension_name c)

type connect_error = {
  uri : Uri.t;
  driver_detail : driver_detail;
}

let connect_failed ~uri driver_detail =
  `Connect_failed {uri; driver_detail}

let pp_connect_error ppf err =
  Format.fprintf ppf "Failed to connect to %a, " Uri.pp_hum err.uri;
  pp_driver_detail ppf err.driver_detail;
  Format.pp_print_char ppf '.'

type request_error = {
  uri : Uri.t;
  query_string : string;
  driver_detail : driver_detail;
}

let request_rejected ~uri ~query_string driver_detail =
  `Request_rejected {uri; query_string; driver_detail}
let request_failed ~uri ~query_string driver_detail =
  `Request_failed {uri; query_string; driver_detail}

let pp_request_error ppf ~fmt err =
  Format.fprintf ppf fmt Uri.pp_hum err.uri;
  Format.pp_print_string ppf ", ";
  pp_driver_detail ppf err.driver_detail;
  Format.fprintf ppf ", for query %S." err.query_string

type response_error = {
  uri : Uri.t;
  query_string : string;
  detail : string;
}

let response_rejected ~uri ~query_string detail =
  `Response_rejected {uri; query_string; detail}

let pp_response_error ppf err =
  Format.fprintf ppf "Unexpected result from %a, %s."
    Uri.pp_hum err.uri err.detail;

type t = [
 | `Connect_failed of connect_error
 | `Request_rejected of request_error
 | `Request_failed of request_error
 | `Response_rejected of response_error
]

let uri = function
 | `Connect_failed ({uri; _} : connect_error) -> uri
 | `Request_rejected ({uri; _} : request_error) -> uri
 | `Request_failed ({uri; _} : request_error) -> uri
 | `Response_rejected ({uri; _} : response_error) -> uri

let pp_hum ppf = function
 | `Connect_failed err ->
    pp_connect_error ppf err
 | `Request_rejected err ->
    pp_request_error ppf ~fmt:"Request rejected by %a" err
 | `Request_failed err ->
    pp_request_error ppf ~fmt:"Request to %a failed" err
 | `Response_rejected err ->
    pp_response_error ppf err

let to_string_hum err =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  pp_hum ppf err;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
