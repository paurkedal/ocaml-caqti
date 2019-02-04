(* Copyright (C) 2017--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Error descriptors. *)


(** {2 Messages} *)

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
(** [pp_msg ppf msg] formats [msg] on [ppf]. *)

type msg += Msg : string -> msg
(** The shape of locally generated messages and messages from drivers without
    dedicated error type. *)

(**/**)
val pp_uri : Format.formatter -> Uri.t -> unit
(** Pretty printer of URIs which omits the password, used by drivers when
    logging. *)
(**/**)

(** {2 Messages with Metadata}

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
  typ: Caqti_type.any;
  msg: msg;
}


(** {2 Documented Constructors} *)


(** {3 Errors during Driver Loading} *)

val load_rejected : uri: Uri.t -> msg -> [> `Load_rejected of load_error]
(** [load_rejected ~uri msg] indicates that the driver to load could not be
    identified from [uri]. *)

val load_failed : uri: Uri.t -> msg -> [> `Load_failed of load_error]
(** [load_failed ~uri msg] indicates that a driver for [uri] could not be
    loaded. *)


(** {3 Errors during Connect} *)

val connect_rejected : uri: Uri.t -> msg ->
  [> `Connect_rejected of connection_error]
(** [connect_rejected ~uri msg] indicates that the driver rejected the URI. *)

val connect_failed : uri: Uri.t -> msg ->
  [> `Connect_failed of connection_error]
(** [connect_failed ~uri msg] indicates that the driver failed to establish a
    connection to the database. *)


(** {3 Errors during Call} *)

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


(** {3 Errors during Result Retrieval} *)

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


(** {2 Specific Error Types} *)

type call =
  [ `Encode_rejected of coding_error
  | `Encode_failed of coding_error
  | `Request_rejected of query_error
  | `Request_failed of query_error
  | `Response_rejected of query_error ]

type retrieve =
  [ `Decode_rejected of coding_error
  | `Response_failed of query_error
  | `Response_rejected of query_error ]

type call_or_retrieve = [call | retrieve]

type transact = [call | retrieve] (* TODO: Should be a subset. *)

type load =
  [ `Load_rejected of load_error
  | `Load_failed of load_error ]

type connect =
  [ `Connect_rejected of connection_error
  | `Connect_failed of connection_error
  | `Post_connect of call_or_retrieve ]

type load_or_connect = [load | connect]


(** {2 Generic Error Type and Functions} *)

type t = [load | connect | call | retrieve]
(** The full union of errors used by Caqti. *)

val uri : [< t] -> Uri.t
(** [uri error] is the URI of the connection used where [error] occurred. *)

val pp : Format.formatter -> [< t] -> unit
(** [pp ppf error] prints an explanation of [error] on [ppf]. *)

val show : [< t] -> string
(** [show error] is an explanation of [error]. *)

exception Exn of t
(** [Exn error] can be used when an exception is preferred over explicit error
    handling.  The core Caqti API never raises exceptions which originate from
    runtime errors. *)
