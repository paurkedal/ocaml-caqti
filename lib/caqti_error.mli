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

type msg = ..
(** In this type, drivers can stash information about an errors in ther own
    format, which can later be used for pretty-printing and or future
    operations. Drivers must {!define_msg} on each constructor added to this
    type. *)

val define_msg :
  pp: (Format.formatter -> msg -> unit) ->
  extension_constructor -> unit
(** Mandatory registration of pretty-printer for a driver-supplied error
    descriptor.  *)

val pp_msg : Format.formatter -> msg -> unit

type msg += Msg : string -> msg


(** {2 Error Records}

    {b Note.} Please consider the fields internal for now, they may still be
    revised or hidden. *)

type load_error = private {
  uri: Uri.t;
  msg: msg;
}
type connection_error = private {
  uri: Uri.t;
  msg: msg;
}
type query_error = private {
  uri: Uri.t;
  query: string;
  msg: msg;
}
type coding_error = private {
  uri: Uri.t;
  typ: Caqti_type.ex;
  msg: msg;
}


(** {2 Errors during Driver Loading} *)

type load =
  [ `Load_rejected of load_error
  | `Load_failed of load_error ]

val load_rejected : uri: Uri.t -> msg -> [> `Load_rejected of load_error]
(** [load_rejected ~uri msg] indicates that the driver to load could not be
    identified from [uri]. *)

val load_failed : uri: Uri.t -> msg -> [> `Load_failed of load_error]
(** [load_failed ~uri msg] indicates that a driver for [uri] could not be
    loaded. *)


(** {2 Errors during Connect} *)

type connect =
  [ `Connect_rejected of connection_error
  | `Connect_failed of connection_error ]

val connect_rejected : uri: Uri.t -> msg ->
  [> `Connect_rejected of connection_error]
(** [connect_rejected ~uri msg] indicates that the driver rejected the URI. *)

val connect_failed : uri: Uri.t -> msg ->
  [> `Connect_failed of connection_error]
(** [connect_failed ~uri msg] indicates that the driver failed to establish a
    connection to the database. *)

type disconnect =
  [ `Disconnect_rejected of connection_error ]

val disconnect_rejected : uri: Uri.t -> msg ->
  [> `Disconnect_rejected of connection_error ]
(** [disconnect_failed ~uri msg] indicates that the connection could not be
    released, typically because it's busy. *)


(** {2 Errors during Call} *)

type call =
  [ `Encode_rejected of coding_error
  | `Encode_failed of coding_error
  | `Request_rejected of query_error
  | `Request_failed of query_error
  | `Response_rejected of query_error ]

val encode_missing : uri: Uri.t -> field_type: 'a Caqti_type.field -> unit ->
  [> `Encode_rejected of coding_error]
(** [encode_missing ~uri ~field_type ()] indicates that the driver does not
    support [field_type] and no fallback encoding is available for the type. *)

val encode_rejected : uri: Uri.t -> typ: 'a Caqti_type.t -> msg ->
  [> `Encode_rejected of coding_error]
(** [encode_rejected ~uri ~typ msg] indicates that encoding a value to [typ]
    failed, e.g. due to being out of range. *)

val encode_failed : uri: Uri.t -> typ: 'a Caqti_type.t -> msg ->
  [> `Encode_failed of coding_error]
(** [encode_failed ~uri ~typ msg] indicates that a parameter of type [typ] was
    not accepted by the database driver. *)

val request_rejected : uri: Uri.t -> query: string -> msg ->
  [> `Request_rejected of query_error]
(** [request_rejected ~uri ~query msg] indicates that [query] was not accepted
    by the database or driver. *)

val request_failed : uri: Uri.t -> query: string -> msg ->
  [> `Request_failed of query_error]
(** [request_failed ~uri ~query msg] indicates that the request to could not be
    transmitted to the database, that the database was not ready to process the
    request, or that something went wrong while the request was being
    processed. *)


(** {2 Errors during Result Retrieval} *)

type retrieve =
  [ `Decode_rejected of coding_error
  | `Response_failed of query_error
  | `Response_rejected of query_error ]

val decode_missing : uri: Uri.t -> field_type: 'a Caqti_type.field -> unit ->
  [> `Decode_rejected of coding_error]
(** [decode_missing ~uri ~field_type ()] indicates that the driver does not
    support [field_type] for decoding result rows. *)

val decode_rejected : uri: Uri.t -> typ: 'a Caqti_type.t -> msg ->
  [> `Decode_rejected of coding_error]
(** [decode_rejected ~uri ~typ msg] indicates that the driver could not decode a
    field of type [typ] from the returned row, e.g. due to an invalid value or
    limited range of the target type. *)

val response_failed : uri: Uri.t -> query: string -> msg ->
  [> `Response_failed of query_error]
(** [response_failed ~uri ~query msg] indicates that something when wrong while
    fetching a delayed part of the response. *)

val response_rejected : uri: Uri.t -> query: string -> msg ->
  [> `Response_rejected of query_error]
(** [response_rejected ~uri ~query msg] indicates that the response from the
    database was rejected due to requirements posed by client code. *)


(** {2 Union and Common Operations} *)

type t = [load | connect | disconnect | call | retrieve]
type load_or_connect = [load | connect]
type call_or_retrieve = [call | retrieve]
type transact = [call | retrieve] (* TODO: Should be a subset. *)

val uri : [< t] -> Uri.t

val pp_hum : Format.formatter -> [< t] -> unit

val to_string_hum : [< t] -> string
