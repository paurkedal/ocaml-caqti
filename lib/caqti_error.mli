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

(** (v2) Error descriptors. *)


(** {2 Driver-Specific Error Messages} *)

type driver_msg = ..
(** In this type, drivers can stash information about an errors in ther own
    format, which can later be used for pretty-printing and or future
    operations. Drivers must {!define_driver_msg} on each constructor added to
    this type. *)

val define_driver_msg :
  pp: (Format.formatter -> driver_msg -> unit) ->
  extension_constructor -> unit
(** Mandatory registration of pretty-printer for a driver-supplied error
    descriptor.  *)

val pp_driver_msg : Format.formatter -> driver_msg -> unit


(** {2 Errors during Driver Loading} *)

type load_msg = private {
  uri: Uri.t;
  msg: string;
}
type load =
  [ `Load_rejected of load_msg
  | `Load_failed of load_msg ]

val load_rejected : uri: Uri.t -> string -> [> `Load_rejected of load_msg]
(** [load_rejected ~uri msg] indicates that the driver to load could not be
    identified from [uri]. *)

val load_failed : uri: Uri.t -> string -> [> `Load_failed of load_msg]
(** [load_failed ~uri msg] indicates that a driver for [uri] could not be
    loaded. *)

(** {2 Errors during Connect} *)

type connect_msg = private {
  uri: Uri.t;
  msg: driver_msg;
}
type connect =
  [ `Connect_failed of connect_msg ]

val connect_failed : uri: Uri.t -> driver_msg ->
  [> `Connect_failed of connect_msg]
(** [connect_failed ~uri msg] indicates that the driver failed to establish a
    connection to the database. *)


(** {2 Errors during Request Processing} *)

type coding_msg = private {
  uri: Uri.t;
  field_type: Caqti_type.Field.ex;
  msg: string option;
}
type request_msg = private {
  uri: Uri.t;
  query: string;
  msg: driver_msg;
}
type call =
  [ `Encode_missing of coding_msg
  | `Encode_rejected of coding_msg
  | `Request_rejected of request_msg
  | `Request_failed of request_msg ]

val encode_missing : uri: Uri.t -> field_type: 'a Caqti_type.field -> unit ->
  [> `Encode_missing of coding_msg]
(** [encode_missing ~uri ~field_type ()] indicates that the driver does not
    support [field_type] and no fallback encoding is avaliable for the type. *)

val encode_rejected : uri: Uri.t -> field_type: 'a Caqti_type.field -> string ->
  [> `Encode_rejected of coding_msg]
(** [encode_rejected ~uri ~field_type msg] indicates that encoding a value to
    [field_type] failed, e.g. due to being out of range. *)

val request_rejected : uri: Uri.t -> query: string -> driver_msg ->
  [> `Request_rejected of request_msg]
(** [request_rejected ~uri ~query msg] indicates that [query] was not accepted
    by the database or driver. *)

val request_failed : uri: Uri.t -> query: string -> driver_msg ->
  [> `Request_failed of request_msg]
(** [request_failed ~uri ~query msg] indicates that the request to could not be
    transmitted to the database, that the database was not ready to process the
    request, or that something went wrong while the requset was being
    processed. *)


(** {2 Errors during Response Processing} *)

type response_msg = private {
  uri: Uri.t;
  query: string;
  msg: string;
}
type retrieve =
  [ `Decode_missing of coding_msg
  | `Decode_rejected of coding_msg
  | `Response_rejected of response_msg ]

val decode_missing : uri: Uri.t -> field_type: 'a Caqti_type.field -> unit ->
  [> `Decode_missing of coding_msg]
(** [decode_missing ~uri ~field_type ()] indicates that the driver does not
    support [field_type] for decoding result rows. *)

val decode_rejected : uri: Uri.t -> field_type: 'a Caqti_type.field -> string ->
  [> `Decode_rejected of coding_msg]
(** [decode_rejected ~uri ~field_type msg] indicates that the driver could not
    decode a field of type [field_type] from the returnd row, e.g. due to an
    invalid value or limited range of the target type. *)

val response_rejected : uri: Uri.t -> query: string -> string ->
  [> `Response_rejected of response_msg]
(** [response_rejected ~uri ~query msg] indicates that the response from the
    database was rejected due to requirements posed by client code. *)


(** {2 Union and Common Operations} *)

type t = [load | connect | call | retrieve]
type call_or_retrieve = [call | retrieve]
type load_or_connect = [load | connect]

val uri : [< t] -> Uri.t

val pp_hum : Format.formatter -> [< t] -> unit

val to_string_hum : [< t] -> string
