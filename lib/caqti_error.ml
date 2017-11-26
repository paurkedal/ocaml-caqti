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


type load_msg = {
  uri : Uri.t;
  msg : string;
}
let pp_load_msg ppf ~fmt err =
  Format.fprintf ppf fmt Uri.pp_hum err.uri;
  Format.pp_print_string ppf ": ";
  Format.pp_print_string ppf err.msg

let load_rejected ~uri msg = `Load_rejected {uri; msg}
let load_failed ~uri msg = `Load_failed {uri; msg}

type load =
  [ `Load_rejected of load_msg
  | `Load_failed of load_msg ]


type connect_msg = {
  uri : Uri.t;
  msg : driver_msg;
}
let pp_connect_msg ppf err =
  Format.fprintf ppf "Failed to connect to %a: " Uri.pp_hum err.uri;
  pp_driver_msg ppf err.msg;
  Format.pp_print_char ppf '.'

let connect_failed ~uri msg =
  `Connect_failed {uri; msg}

type connect =
  [ `Connect_failed of connect_msg ]


type coding_msg = {
  uri : Uri.t;
  field_type : Caqti_type.Field.ex;
  msg : string option;
}
let pp_coding_msg ppf ~fmt err =
  let Caqti_type.Field.Ex field_type = err.field_type in
  Format.fprintf ppf fmt (Caqti_type.Field.to_string field_type);
  Format.fprintf ppf " for %a" Uri.pp_hum err.uri;
  (match err.msg with
   | None -> Format.pp_print_char ppf '.'
   | Some msg ->
      Format.pp_print_string ppf ": ";
      Format.pp_print_string ppf msg)

type request_msg = {
  uri : Uri.t;
  query : string;
  msg : driver_msg;
}
type call =
  [ `Encode_missing of coding_msg
  | `Encode_rejected of coding_msg
  | `Request_rejected of request_msg
  | `Request_failed of request_msg ]

let encode_missing ~uri ~field_type () =
  `Encode_missing {uri; field_type = Caqti_type.Field.Ex field_type; msg = None}
let encode_rejected ~uri ~field_type msg =
  let msg = Some msg in
  `Encode_rejected {uri; field_type = Caqti_type.Field.Ex field_type; msg}
let request_rejected ~uri ~query msg =
  `Request_rejected {uri; query; msg}
let request_failed ~uri ~query msg =
  `Request_failed {uri; query; msg}

let pp_request_msg ppf ~fmt err =
  Format.fprintf ppf fmt Uri.pp_hum err.uri;
  Format.pp_print_string ppf ", ";
  pp_driver_msg ppf err.msg;
  Format.fprintf ppf ", for query %S." err.query


type response_msg = {
  uri : Uri.t;
  query : string;
  msg : string;
}
type retrieve =
  [ `Decode_missing of coding_msg
  | `Decode_rejected of coding_msg
  | `Response_rejected of response_msg ]

let decode_missing ~uri ~field_type () =
  `Decode_missing {uri; field_type = Caqti_type.Field.Ex field_type; msg = None}
let decode_rejected ~uri ~field_type msg =
  let msg = Some msg in
  `Decode_rejected {uri; field_type = Caqti_type.Field.Ex field_type; msg}
let response_rejected ~uri ~query msg =
  `Response_rejected {uri; query; msg}

let pp_response_msg ppf err =
  Format.fprintf ppf "Unexpected result from %a, %s."
    Uri.pp_hum err.uri err.msg;

type t = [load | connect | call | retrieve]
type call_or_retrieve = [call | retrieve]
type load_or_connect = [load | connect]

let uri = function
 | `Load_rejected ({uri; _} : load_msg) -> uri
 | `Load_failed ({uri; _} : load_msg) -> uri
 | `Connect_failed ({uri; _} : connect_msg) -> uri
 | `Encode_missing ({uri; _} : coding_msg) -> uri
 | `Encode_rejected ({uri; _} : coding_msg) -> uri
 | `Request_rejected ({uri; _} : request_msg) -> uri
 | `Request_failed ({uri; _} : request_msg) -> uri
 | `Decode_missing ({uri; _} : coding_msg) -> uri
 | `Decode_rejected ({uri; _} : coding_msg) -> uri
 | `Response_rejected ({uri; _} : response_msg) -> uri

let pp_hum ppf = function
 | `Load_rejected err -> pp_load_msg ppf ~fmt:"Cannot load \"%a\"" err
 | `Load_failed err -> pp_load_msg ppf ~fmt:"Failed to load \"%a\"" err
 | `Connect_failed err -> pp_connect_msg ppf err
 | `Encode_missing err -> pp_coding_msg ppf ~fmt:"Encoder missing for %s" err
 | `Encode_rejected err -> pp_coding_msg ppf ~fmt:"Failed to encode %s" err
 | `Decode_missing err -> pp_coding_msg ppf ~fmt:"Decoder missing for %s" err
 | `Decode_rejected err -> pp_coding_msg ppf ~fmt:"Failed to decode %s" err
 | `Request_rejected err -> pp_request_msg ppf ~fmt:"Request rejected by %a" err
 | `Request_failed err -> pp_request_msg ppf ~fmt:"Request to %a failed" err
 | `Response_rejected err -> pp_response_msg ppf err

let to_string_hum err =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  pp_hum ppf err;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
