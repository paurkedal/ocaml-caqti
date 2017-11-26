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

type msg = ..

let msg_pp = Hashtbl.create 7

let define_msg ~pp ec = Hashtbl.add msg_pp ec pp

let pp_msg ppf msg =
  let c = Obj.extension_constructor msg in
  try
    let pp = Hashtbl.find msg_pp c in
    pp ppf msg
  with Not_found ->
    Format.fprintf ppf
      "[FIXME: missing printer for (%s _ : Caqti_error.msg)]"
      (Obj.extension_name c)

type msg += Msg : string -> msg

let () =
  let pp ppf = function
   | Msg s -> Format.pp_print_string ppf s
   | _ -> assert false in
  define_msg ~pp [%extension_constructor Msg]

(* Records *)

type load_error = {
  uri : Uri.t;
  msg : msg;
}
let pp_load_msg ppf fmt err =
  Format.fprintf ppf fmt Uri.pp_hum err.uri;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg

type connection_error = {
  uri : Uri.t;
  msg : msg;
}
let pp_connection_msg ppf fmt err =
  Format.fprintf ppf fmt Uri.pp_hum err.uri;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg;
  Format.pp_print_char ppf '.'

type query_error = {
  uri : Uri.t;
  query : string;
  msg : msg;
}
let pp_query_msg ppf fmt err =
  Format.fprintf ppf fmt Uri.pp_hum err.uri;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg;
  Format.fprintf ppf " Query: %S." err.query

type coding_error = {
  uri : Uri.t;
  typ : Caqti_type.ex;
  msg : msg;
}
let pp_coding_error ppf fmt err =
  let Caqti_type.Ex typ = err.typ in
  Format.fprintf ppf fmt (Caqti_type.to_string_hum typ);
  Format.fprintf ppf " for %a" Uri.pp_hum err.uri;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg

(* Load *)

let load_rejected ~uri msg = `Load_rejected ({uri; msg} : load_error)
let load_failed ~uri msg = `Load_failed ({uri; msg} : load_error)

type load =
  [ `Load_rejected of load_error
  | `Load_failed of load_error ]

(* Connect *)

let connect_failed ~uri msg =
  `Connect_failed ({uri; msg} : connection_error)

type connect =
  [ `Connect_failed of connection_error ]

let disconnect_rejected ~uri msg =
  `Disconnect_rejected ({uri; msg} : connection_error)

type disconnect =
  [ `Disconnect_rejected of connection_error ]

(* Call *)

type call =
  [ `Encode_rejected of coding_error
  | `Encode_failed of coding_error
  | `Request_rejected of query_error
  | `Request_failed of query_error ]

let encode_missing ~uri ~field_type () =
  let typ = Caqti_type.Ex (Caqti_type.field field_type) in
  let msg = Msg "Field type not supported and no fallback provided." in
  `Encode_rejected ({uri; typ; msg} : coding_error)
let encode_rejected ~uri ~typ msg =
  let typ = Caqti_type.Ex typ in
  `Encode_rejected ({uri; typ; msg} : coding_error)
let encode_failed ~uri ~typ msg =
  let typ = Caqti_type.Ex typ in
  `Encode_failed ({uri; typ; msg} : coding_error)
let request_rejected ~uri ~query msg =
  `Request_rejected ({uri; query; msg} : query_error)
let request_failed ~uri ~query msg =
  `Request_failed ({uri; query; msg} : query_error)

(* Retrieve *)

type retrieve =
  [ `Decode_rejected of coding_error
  | `Response_failed of query_error
  | `Response_rejected of query_error ]

let decode_missing ~uri ~field_type () =
  let typ = Caqti_type.Ex (Caqti_type.field field_type) in
  let msg = Msg "Field type not supported and no fallback provided." in
  `Decode_rejected ({uri; typ; msg} : coding_error)
let decode_rejected ~uri ~typ msg =
  let typ = Caqti_type.Ex typ in
  `Decode_rejected ({uri; typ; msg} : coding_error)
let response_failed ~uri ~query msg =
  `Response_failed ({uri; query; msg} : query_error)
let response_rejected ~uri ~query msg =
  `Response_rejected ({uri; query; msg} : query_error)

(* Common *)

type t = [load | connect | disconnect | call | retrieve]
type load_or_connect = [load | connect]
type call_or_retrieve = [call | retrieve]
type transact = call_or_retrieve

let uri = function
 | `Load_rejected ({uri; _} : load_error) -> uri
 | `Load_failed ({uri; _} : load_error) -> uri
 | `Connect_failed ({uri; _} : connection_error) -> uri
 | `Disconnect_rejected ({uri; _} : connection_error) -> uri
 | `Encode_rejected ({uri; _} : coding_error) -> uri
 | `Encode_failed ({uri; _} : coding_error) -> uri
 | `Request_rejected ({uri; _} : query_error) -> uri
 | `Request_failed ({uri; _} : query_error) -> uri
 | `Decode_rejected ({uri; _} : coding_error) -> uri
 | `Response_failed ({uri; _} : query_error) -> uri
 | `Response_rejected ({uri; _} : query_error) -> uri

let pp_hum ppf = function
 | `Load_rejected err -> pp_load_msg ppf "Cannot load <%a>" err
 | `Load_failed err -> pp_load_msg ppf "Failed to load <%a>" err
 | `Connect_failed err -> pp_connection_msg ppf "Failed to connect to <%a>" err
 | `Disconnect_rejected err ->pp_connection_msg ppf "Cannot disconnect <%a>" err
 | `Encode_rejected err -> pp_coding_error ppf "Failed to encode %s" err
 | `Encode_failed err -> pp_coding_error ppf "Failed to encode %s" err
 | `Decode_rejected err -> pp_coding_error ppf "Failed to decode %s" err
 | `Request_rejected err -> pp_query_msg ppf "Request rejected by <%a>" err
 | `Request_failed err -> pp_query_msg ppf "Request to <%a> failed" err
 | `Response_failed err -> pp_query_msg ppf "Response from <%a> failed" err
 | `Response_rejected err -> pp_query_msg ppf "Unexpected result from <%a>" err

let to_string_hum err =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  pp_hum ppf err;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
