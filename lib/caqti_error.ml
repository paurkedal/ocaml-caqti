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

(* Driver *)

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

type driver_msg += Driver_msg : string -> driver_msg

let () =
  let pp ppf = function
   | Driver_msg s -> Format.pp_print_string ppf s
   | _ -> assert false in
  define_driver_msg ~pp [%extension_constructor Driver_msg]

(* Records *)

type load_error = {
  uri : Uri.t;
  msg : string;
}
let pp_load_msg ppf fmt err =
  Format.fprintf ppf fmt Uri.pp_hum err.uri;
  Format.pp_print_string ppf ": ";
  Format.pp_print_string ppf err.msg

type connect_error = {
  uri : Uri.t;
  msg : driver_msg;
}
let pp_connect_msg ppf err =
  Format.fprintf ppf "Failed to connect to %a: " Uri.pp_hum err.uri;
  pp_driver_msg ppf err.msg;
  Format.pp_print_char ppf '.'

type local_error = {
  uri : Uri.t;
  query : string;
  msg : string;
}
let pp_local_msg ppf err =
  Format.fprintf ppf "Unexpected result from %a, %s."
    Uri.pp_hum err.uri err.msg;

type remote_error = {
  uri : Uri.t;
  query : string;
  msg : driver_msg;
}
let pp_remote_msg ppf fmt err =
  Format.fprintf ppf fmt Uri.pp_hum err.uri;
  Format.pp_print_string ppf ", ";
  pp_driver_msg ppf err.msg;
  Format.fprintf ppf ", for query %S." err.query

type local_coding_error = {
  uri : Uri.t;
  field_type : Caqti_type.Field.ex;
  msg : string option;
}
let pp_local_coding_error ppf fmt err =
  let Caqti_type.Field.Ex field_type = err.field_type in
  Format.fprintf ppf fmt (Caqti_type.Field.to_string field_type);
  Format.fprintf ppf " for %a" Uri.pp_hum err.uri;
  (match err.msg with
   | None -> Format.pp_print_char ppf '.'
   | Some msg ->
      Format.pp_print_string ppf ": ";
      Format.pp_print_string ppf msg)

type remote_coding_error = {
  uri : Uri.t;
  field_type : Caqti_type.Field.ex;
  msg : driver_msg;
}
let pp_remote_coding_error ppf fmt err =
  let Caqti_type.Field.Ex field_type = err.field_type in
  Format.fprintf ppf fmt (Caqti_type.Field.to_string field_type);
  Format.fprintf ppf " for %a" Uri.pp_hum err.uri;
  Format.pp_print_string ppf ": ";
  pp_driver_msg ppf err.msg

(* Load *)

let load_rejected ~uri msg = `Load_rejected ({uri; msg} : load_error)
let load_failed ~uri msg = `Load_failed ({uri; msg} : load_error)

type load =
  [ `Load_rejected of load_error
  | `Load_failed of load_error ]

(* Connect *)

let connect_failed ~uri msg =
  `Connect_failed ({uri; msg} : connect_error)

type connect =
  [ `Connect_failed of connect_error ]

(* Call *)

type call =
  [ `Encode_missing of local_coding_error
  | `Encode_rejected of local_coding_error
  | `Encode_failed of remote_coding_error
  | `Request_rejected of remote_error
  | `Request_failed of remote_error ]

let encode_missing ~uri ~field_type () =
  let field_type = Caqti_type.Field.Ex field_type in
  `Encode_missing ({uri; field_type; msg = None} : local_coding_error)
let encode_rejected ~uri ~field_type msg =
  let field_type = Caqti_type.Field.Ex field_type in
  `Encode_rejected ({uri; field_type; msg = Some msg} : local_coding_error)
let encode_failed ~uri ~field_type msg =
  let field_type = Caqti_type.Field.Ex field_type in
  `Encode_failed ({uri; field_type; msg} : remote_coding_error)
let request_rejected ~uri ~query msg =
  `Request_rejected ({uri; query; msg} : remote_error)
let request_failed ~uri ~query msg =
  `Request_failed ({uri; query; msg} : remote_error)

(* Retrieve *)

type retrieve =
  [ `Decode_missing of local_coding_error
  | `Decode_rejected of local_coding_error
  | `Response_failed of remote_error
  | `Response_rejected of local_error ]

let decode_missing ~uri ~field_type () =
  let field_type = Caqti_type.Field.Ex field_type in
  `Decode_missing ({uri; field_type; msg = None} : local_coding_error)
let decode_rejected ~uri ~field_type msg =
  let field_type = Caqti_type.Field.Ex field_type in
  `Decode_rejected ({uri; field_type; msg = Some msg} : local_coding_error)
let response_failed ~uri ~query msg =
  `Response_failed ({uri; query; msg} : remote_error)
let response_rejected ~uri ~query msg =
  `Response_rejected ({uri; query; msg} : local_error)

(* Common *)

type t = [load | connect | call | retrieve]
type call_or_retrieve = [call | retrieve]
type load_or_connect = [load | connect]

let uri = function
 | `Load_rejected ({uri; _} : load_error) -> uri
 | `Load_failed ({uri; _} : load_error) -> uri
 | `Connect_failed ({uri; _} : connect_error) -> uri
 | `Encode_missing ({uri; _} : local_coding_error) -> uri
 | `Encode_rejected ({uri; _} : local_coding_error) -> uri
 | `Encode_failed ({uri; _} : remote_coding_error) -> uri
 | `Request_rejected ({uri; _} : remote_error) -> uri
 | `Request_failed ({uri; _} : remote_error) -> uri
 | `Decode_missing ({uri; _} : local_coding_error) -> uri
 | `Decode_rejected ({uri; _} : local_coding_error) -> uri
 | `Response_failed ({uri; _} : remote_error) -> uri
 | `Response_rejected ({uri; _} : local_error) -> uri

let pp_hum ppf = function
 | `Load_rejected err -> pp_load_msg ppf "Cannot load \"%a\"" err
 | `Load_failed err -> pp_load_msg ppf "Failed to load \"%a\"" err
 | `Connect_failed err -> pp_connect_msg ppf err
 | `Encode_missing err -> pp_local_coding_error ppf "Encoder missing for %s" err
 | `Encode_rejected err -> pp_local_coding_error ppf "Failed to encode %s" err
 | `Encode_failed err -> pp_remote_coding_error ppf "Failed to encode %s" err
 | `Decode_missing err -> pp_local_coding_error ppf "Decoder missing for %s" err
 | `Decode_rejected err -> pp_local_coding_error ppf "Failed to decode %s" err
 | `Request_rejected err -> pp_remote_msg ppf "Request rejected by %a" err
 | `Request_failed err -> pp_remote_msg ppf "Request to %a failed" err
 | `Response_failed err -> pp_remote_msg ppf "Response from %a failed" err
 | `Response_rejected err -> pp_local_msg ppf err

let to_string_hum err =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  pp_hum ppf err;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
