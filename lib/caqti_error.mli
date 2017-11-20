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

val load_rejected :
  uri: Uri.t -> string ->
  [> `Load_rejected of load_msg]
(** [load_rejected ~uri msg] indicates that the driver to load could not be
    identified from [uri]. *)

val load_failed :
  uri: Uri.t -> string ->
  [> `Load_failed of load_msg]
(** [load_failed ~uri msg] indicates that a driver for [uri] could not be
    loaded. *)

(** {2 Errors during Connect} *)

type connect_msg = private {
  uri: Uri.t;
  msg: driver_msg;
}
type connect =
  [ `Connect_failed of connect_msg ]

val connect_failed :
  uri: Uri.t -> driver_msg ->
  [> `Connect_failed of connect_msg]
(** [connect_failed ~uri msg] indicates that the driver failed to establish a
    connection to the database. *)


(** {2 Errors during Request Processing} *)

type request_msg = private {
  uri: Uri.t;
  query_string: string;
  msg: driver_msg;
}
type request =
  [ `Request_rejected of request_msg
  | `Request_failed of request_msg ]

val request_rejected :
  uri: Uri.t ->
  query_string: string -> driver_msg ->
  [> `Request_rejected of request_msg]
(** [request_rejected ~uri ~query_string msg] indicates that [query_string]
    was not accepted by the database or driver. *)

val request_failed :
  uri: Uri.t ->
  query_string: string -> driver_msg ->
  [> `Request_failed of request_msg]
(** [request_failed ~uri ~query_string msg] indicates that the request to
    could not be transmitted to the database, that the database was not ready to
    process the request, or that something went wrong while the requset was
    being processed. *)


(** {2 Errors during Response Processing} *)

type response_msg = private {
  uri: Uri.t;
  query_string: string;
  msg: string;
}
type response =
  [ `Response_rejected of response_msg ]

val response_rejected :
  uri: Uri.t ->
  query_string: string -> string ->
  [> `Response_rejected of response_msg]
(** [response_rejected ~uri ~query_string msg] indicates that the response from
    the database was rejected due to requirements posed by client code. *)


(** {2 Union and Common Operations} *)

type t = [load | connect | request | response]
type request_or_response = [request | response]
type load_or_connect = [load | connect]

val uri : [< t] -> Uri.t

val pp_hum : Format.formatter -> [< t] -> unit

val to_string_hum : [< t] -> string
