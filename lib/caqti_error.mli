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

(** Error descriptors. *)

type driver_detail = ..

val pp_driver_detail : Format.formatter -> driver_detail -> unit

val register_driver_detail :
  pp: (Format.formatter -> driver_detail -> unit) ->
  extension_constructor -> unit

type connect_error = private {
  uri : Uri.t;
  driver_detail : driver_detail;
}

val connect_failed :
  uri: Uri.t -> driver_detail -> [> `Connect_failed of connect_error]

type request_error = private {
  uri : Uri.t;
  query_string : string;
  driver_detail : driver_detail;
}

val request_rejected :
  uri: Uri.t -> query_string: string -> driver_detail ->
  [> `Request_rejected of request_error]

val request_failed :
  uri: Uri.t -> query_string: string -> driver_detail ->
  [> `Request_failed of request_error]

type response_error = private {
  uri : Uri.t;
  query_string : string;
  detail : string;
}

val response_rejected :
  uri: Uri.t -> query_string: string -> string ->
  [> `Response_rejected of response_error]

type t = [
 | `Connect_failed of connect_error
 | `Request_rejected of request_error
 | `Request_failed of request_error
 | `Response_rejected of response_error
]

val uri : [< t] -> Uri.t

val pp_hum : Format.formatter -> [< t] -> unit
