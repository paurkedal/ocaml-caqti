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

type driver_msg += Driver_msg : string -> driver_msg


(** {2 Error Records}

    {b Note.} Please consider the fields internal for now, they may still be
    revised or hidden. *)

type load_error = private {
  uri: Uri.t;
  msg: string;
}
type connect_error = private {
  uri: Uri.t;
  msg: driver_msg;
}
type local_error = private {
  uri: Uri.t;
  query: string;
  msg: string;
}
type remote_error = private {
  uri: Uri.t;
  query: string;
  msg: driver_msg;
}
type local_coding_error = private {
  uri: Uri.t;
  field_type: Caqti_type.Field.ex;
  msg: string option;
}
type remote_coding_error = private {
  uri: Uri.t;
  field_type: Caqti_type.Field.ex;
  msg: driver_msg;
}


(** {2 Errors during Driver Loading} *)

type load =
  [ `Load_rejected of load_error
  | `Load_failed of load_error ]

val load_rejected : uri: Uri.t -> string -> [> `Load_rejected of load_error]
(** [load_rejected ~uri msg] indicates that the driver to load could not be
    identified from [uri]. *)

val load_failed : uri: Uri.t -> string -> [> `Load_failed of load_error]
(** [load_failed ~uri msg] indicates that a driver for [uri] could not be
    loaded. *)


(** {2 Errors during Connect} *)

type connect =
  [ `Connect_failed of connect_error ]

val connect_failed : uri: Uri.t -> driver_msg ->
  [> `Connect_failed of connect_error]
(** [connect_failed ~uri msg] indicates that the driver failed to establish a
    connection to the database. *)


(** {2 Errors during Call} *)

type call =
  [ `Encode_missing of local_coding_error
  | `Encode_rejected of local_coding_error
  | `Encode_failed of remote_coding_error
  | `Request_rejected of remote_error
  | `Request_failed of remote_error ]

val encode_missing : uri: Uri.t -> field_type: 'a Caqti_type.field -> unit ->
  [> `Encode_missing of local_coding_error]
(** [encode_missing ~uri ~field_type ()] indicates that the driver does not
    support [field_type] and no fallback encoding is available for the type. *)

val encode_rejected : uri: Uri.t -> field_type: 'a Caqti_type.field -> string ->
  [> `Encode_rejected of local_coding_error]
(** [encode_rejected ~uri ~field_type msg] indicates that encoding a value to
    [field_type] failed, e.g. due to being out of range. *)

val encode_failed : uri: Uri.t -> field_type: 'a Caqti_type.field ->
  driver_msg ->
  [> `Encode_failed of remote_coding_error]
(** [encode_failed ~uri ~field_type msg] indicates that a parameter of type
    [field_type] was not accepted by the database driver. *)

val request_rejected : uri: Uri.t -> query: string -> driver_msg ->
  [> `Request_rejected of remote_error]
(** [request_rejected ~uri ~query msg] indicates that [query] was not accepted
    by the database or driver. *)

val request_failed : uri: Uri.t -> query: string -> driver_msg ->
  [> `Request_failed of remote_error]
(** [request_failed ~uri ~query msg] indicates that the request to could not be
    transmitted to the database, that the database was not ready to process the
    request, or that something went wrong while the request was being
    processed. *)


(** {2 Errors during Result Retrieval} *)

type retrieve =
  [ `Decode_missing of local_coding_error
  | `Decode_rejected of local_coding_error
  | `Response_failed of remote_error
  | `Response_rejected of local_error ]

val decode_missing : uri: Uri.t -> field_type: 'a Caqti_type.field -> unit ->
  [> `Decode_missing of local_coding_error]
(** [decode_missing ~uri ~field_type ()] indicates that the driver does not
    support [field_type] for decoding result rows. *)

val decode_rejected : uri: Uri.t -> field_type: 'a Caqti_type.field -> string ->
  [> `Decode_rejected of local_coding_error]
(** [decode_rejected ~uri ~field_type msg] indicates that the driver could not
    decode a field of type [field_type] from the returned row, e.g. due to an
    invalid value or limited range of the target type. *)

val response_failed : uri: Uri.t -> query: string -> driver_msg ->
  [> `Response_failed of remote_error]
(** [response_failed ~uri ~query msg] indicates that something when wrong while
    fetching a delayed part of the response. *)

val response_rejected : uri: Uri.t -> query: string -> string ->
  [> `Response_rejected of local_error]
(** [response_rejected ~uri ~query msg] indicates that the response from the
    database was rejected due to requirements posed by client code. *)


(** {2 Union and Common Operations} *)

type t = [load | connect | call | retrieve]
type call_or_retrieve = [call | retrieve]
type load_or_connect = [load | connect]

val uri : [< t] -> Uri.t

val pp_hum : Format.formatter -> [< t] -> unit

val to_string_hum : [< t] -> string
