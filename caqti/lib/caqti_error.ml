(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(* Error Cause *)

type integrity_constraint_violation = [
  | `Restrict_violation
  | `Not_null_violation
  | `Foreign_key_violation
  | `Unique_violation
  | `Check_violation
  | `Exclusion_violation
  | `Integrity_constraint_violation__don't_match
]

type insufficient_resources = [
  | `Disk_full
  | `Out_of_memory
  | `Too_many_connections
  | `Configuration_limit_exceeded
  | `Insufficient_resources__don't_match
]

type cause = [
  | integrity_constraint_violation
  | insufficient_resources
  | `Unspecified__don't_match
]

let show_cause = function
 | `Restrict_violation -> "RESTRICT violation"
 | `Not_null_violation -> "NOT NULL constraint violation"
 | `Foreign_key_violation -> "FOREIGN KEY constraint violation"
 | `Unique_violation -> "UNIQUE constraint violation"
 | `Check_violation -> "CHECK constraint violation"
 | `Exclusion_violation -> "exclusion violation"
 | `Integrity_constraint_violation__don't_match ->
    "integrity constraint violation"
 | `Disk_full -> "disk full"
 | `Out_of_memory -> "out of memory"
 | `Too_many_connections -> "too many connections"
 | `Configuration_limit_exceeded -> "configuration limit exceeded"
 | `Insufficient_resources__don't_match -> "insufficient resources"
 | `Unspecified__don't_match -> "unknown cause"

(* Driver *)

type msg = ..

type msg_impl = {
  msg_pp: Format.formatter -> msg -> unit;
  msg_cause: msg -> cause;
}
let msg_impl = Hashtbl.create 7

let default_cause _ = `Unspecified__don't_match

let define_msg ~pp ?(cause = default_cause) ec =
  Hashtbl.add msg_impl ec {msg_pp = pp; msg_cause = cause}

let find_impl msg =
  let c = Obj.Extension_constructor.of_val msg in
  try
    Hashtbl.find msg_impl c
  with Not_found ->
    Printf.ksprintf failwith
      "Missing call to Caqti_error.define_msg for (%s _ : Caqti_error.msg)]"
      (Obj.Extension_constructor.name c)

type msg += Msg : string -> msg

let is_punct = function
 | '.' | '!' | '?' -> true
 | _ -> false

let () =
  let pp ppf = function
   | Msg s ->
      Format.pp_print_string ppf s;
      if s <> "" && not (is_punct s.[String.length s - 1]) then
        Format.pp_print_char ppf '.'
   | _ -> assert false
  in
  define_msg ~pp ~cause:default_cause [%extension_constructor Msg]

let pp_msg ppf msg =
  (find_impl msg).msg_pp ppf msg

(* We don't want to expose any DB password in error messages. *)
let pp_uri ppf uri =
  (match Uri.password uri with
   | None -> Uri.pp_hum ppf uri
   | Some _ -> Uri.pp_hum ppf (Uri.with_password uri (Some "_")))

(* Records *)

type load_error = {
  uri: Uri.t;
  msg: msg;
}
let pp_load_msg ppf fmt err =
  Format.fprintf ppf fmt pp_uri err.uri;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg

type connection_error = {
  uri: Uri.t;
  msg: msg;
}
let pp_connection_msg ppf fmt err =
  Format.fprintf ppf fmt pp_uri err.uri;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg

type query_error = {
  uri: Uri.t;
  query: string;
  msg: msg;
}

let pp_query_msg ppf fmt err =
  Format.fprintf ppf fmt pp_uri err.uri;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg;
  Format.fprintf ppf " Query: %S." err.query

type coding_error = {
  uri: Uri.t;
  typ: Caqti_type.any;
  msg: msg;
}
let pp_coding_error ppf fmt err =
  Format.fprintf ppf fmt Caqti_type.pp_any err.typ pp_uri err.uri;
  Format.pp_print_string ppf ": ";
  pp_msg ppf err.msg

(* Load *)

let load_rejected ~uri msg = `Load_rejected ({uri; msg} : load_error)
let load_failed ~uri msg = `Load_failed ({uri; msg} : load_error)

(* Connect *)

let connect_rejected ~uri msg =
  `Connect_rejected ({uri; msg} : connection_error)

let connect_failed ~uri msg =
  `Connect_failed ({uri; msg} : connection_error)

(* Call *)

let encode_missing ~uri ~field_type () =
  let typ = Caqti_type.Any (Caqti_type.field field_type) in
  let msg = Msg "Field type not supported and no fallback provided." in
  `Encode_rejected ({uri; typ; msg} : coding_error)
let encode_rejected ~uri ~typ msg =
  let typ = Caqti_type.Any typ in
  `Encode_rejected ({uri; typ; msg} : coding_error)
let encode_failed ~uri ~typ msg =
  let typ = Caqti_type.Any typ in
  `Encode_failed ({uri; typ; msg} : coding_error)
let request_failed ~uri ~query msg =
  `Request_failed ({uri; query; msg} : query_error)

(* Retrieve *)

let decode_missing ~uri ~field_type () =
  let typ = Caqti_type.Any (Caqti_type.field field_type) in
  let msg = Msg "Field type not supported and no fallback provided." in
  `Decode_rejected ({uri; typ; msg} : coding_error)
let decode_rejected ~uri ~typ msg =
  let typ = Caqti_type.Any typ in
  `Decode_rejected ({uri; typ; msg} : coding_error)
let response_failed ~uri ~query msg =
  `Response_failed ({uri; query; msg} : query_error)
let response_rejected ~uri ~query msg =
  `Response_rejected ({uri; query; msg} : query_error)

(* Common *)

type call =
  [ `Encode_rejected of coding_error
  | `Encode_failed of coding_error
  | `Request_failed of query_error
  | `Response_rejected of query_error ]

type retrieve =
  [ `Decode_rejected of coding_error
  | `Request_failed of query_error
  | `Response_failed of query_error
  | `Response_rejected of query_error ]

type call_or_retrieve = [call | retrieve]

type transact = call_or_retrieve

type load =
  [ `Load_rejected of load_error
  | `Load_failed of load_error ]

type connect =
  [ `Connect_rejected of connection_error
  | `Connect_failed of connection_error
  | `Post_connect of call_or_retrieve ]

type load_or_connect = [load | connect]

type t = [load | connect | call | retrieve]

let rec uri : 'a. ([< t] as 'a) -> Uri.t = function
 | `Load_rejected ({uri; _} : load_error) -> uri
 | `Load_failed ({uri; _} : load_error) -> uri
 | `Connect_rejected ({uri; _} : connection_error) -> uri
 | `Connect_failed ({uri; _} : connection_error) -> uri
 | `Post_connect err -> uri err
 | `Encode_rejected ({uri; _} : coding_error) -> uri
 | `Encode_failed ({uri; _} : coding_error) -> uri
 | `Request_failed ({uri; _} : query_error) -> uri
 | `Decode_rejected ({uri; _} : coding_error) -> uri
 | `Response_failed ({uri; _} : query_error) -> uri
 | `Response_rejected ({uri; _} : query_error) -> uri

let rec pp : 'a. _ -> ([< t] as 'a) -> unit = fun ppf -> function
 | `Load_rejected err -> pp_load_msg ppf "Cannot load driver for <%a>" err
 | `Load_failed err -> pp_load_msg ppf "Failed to load driver for <%a>" err
 | `Connect_rejected err -> pp_connection_msg ppf "Cannot connect to <%a>" err
 | `Connect_failed err -> pp_connection_msg ppf "Failed to connect to <%a>" err
 | `Post_connect err ->
    Format.pp_print_string ppf "During post-connect: ";
    pp ppf err
 | `Encode_rejected err -> pp_coding_error ppf "Cannot encode %a for <%a>" err
 | `Encode_failed err -> pp_coding_error ppf "Failed to bind %a for <%a>" err
 | `Decode_rejected err -> pp_coding_error ppf "Cannot decode %a from <%a>" err
 | `Request_failed err -> pp_query_msg ppf "Request to <%a> failed" err
 | `Response_failed err -> pp_query_msg ppf "Response from <%a> failed" err
 | `Response_rejected err -> pp_query_msg ppf "Unexpected result from <%a>" err

let show_of_pp pp err =
  let buf = Buffer.create 128 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf err;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let show err = show_of_pp pp err

let cause = function
 | `Request_failed err | `Response_failed err ->
    (find_impl (err : query_error).msg).msg_cause err.msg

type counit = |

[@@@warning "-56"]
let uncongested = function
 | Error #t | Ok _ as x -> x
 | Error (`Congested (nothingness : counit)) -> (match nothingness with _ -> .)
[@@@warning "+56"]

exception Exn of t

let () = Printexc.register_printer @@ function
 | Exn err ->
    Some (show err)
 | Caqti_query.Expand_error err ->
    Some (show_of_pp Caqti_query.pp_expand_error err)
 | _ ->
    None
