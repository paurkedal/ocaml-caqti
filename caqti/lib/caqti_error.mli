(* Copyright (C) 2017--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Error descriptors. *)


(** {2 Error Causes}

    The {!type:cause} type is an incomplete enumeration of consolidated causes
    of errors between different database systems.  The selection includes the
    causes which are believed to be useful to handle and excludes causes which
    are specific to the implementation of a certain database system.

    The causes are classified into subtypes to help matching them collectively.
    Each subtype has a fall-back case which is used if the database system does
    not clearly report one of the specific cases.  An condition which is
    reported as the fall-back case may in a future version be reported as a
    specific case, possibly adding a new case to the subtype.  Therefore, for
    backwards compatibility you should match the full subtype rather than the
    fall-back, e.g.
    {[
      (match Caqti_error.cause error with
       | `Unique_violation -> handle_unique_violation ()
       | #integrity_constraint_violation -> handle_other_constraint_violation ()
       | _ -> handle_other_error ())
    ]}
    This ensures that your code stil compiles when a new case is added to
    {!integrity_constraint_violation}, and that the error condition receiving
    mapped to the new case is still handled by the subtype pattern when you link
    to the new version of Caqti.

    Currently we do not have access to the extended error codes from SQLite3,
    meaning that all integrity constraint violation conditions will be reported
    as [`Integrity_constraint_violation__don't_match].

    Since the consolitation of each error condition requires some investitation
    and testing, the selection made is very conservative.  If you need to handle
    an error which is currently unlisted, please open an issue or create a pull
    request.  A pull request should, if possible, include a extension of
    [test_error_cause.ml] to demonstrate how the error is triggered by the
    database systems. *)

type integrity_constraint_violation = [
  | `Restrict_violation
      (** This is meant to indicate that a deletion or update would cause a
          foreign key violation, although this may be reported as a
          [`Foreign_key_violation]. *)
  | `Not_null_violation
      (** An insertion or update attempts to assign a [NULL] value to column
          having a [NOT NULL] constraint. *)
  | `Foreign_key_violation
      (** An modification would cause a column to reference a non-existing key.
          This cause may also be reported for cases which should have been
          covered by [`Restrict_violation]. *)
  | `Unique_violation
      (** An insertion or update would duplicate a key as declared by a [UNIQUE]
          or [PRIMARY KEY] constraint. *)
  | `Check_violation
      (** A requested change would violate a [CHECK] constraint. *)
  | `Exclusion_violation
      (** A requested insertion or update would cause an overlap of rows
          according to an [EXCLUDE] constraint. *)
  | `Integrity_constraint_violation__don't_match
      (** An yet unclassified cause; match the full subtype instead. *)
]
(** A subtype of {!type:cause} informing about violation of SQL constraints. *)

type insufficient_resources = [
  | `Disk_full
      (** The server is out of disk space. *)
  | `Out_of_memory
      (** The server is out of memory *)
  | `Too_many_connections
      (** The server does not accept establishing more connections. *)
  | `Configuration_limit_exceeded
      (** Some unspecific server limit is exceeded. *)
  | `Insufficient_resources__don't_match
      (** An yet unclassified cause; match the full subtype instead. *)
]
(** A subtype of {!type:cause} informing about insufficient resources on the
    server side. *)

type cause = [
  | integrity_constraint_violation
  | insufficient_resources
  | `Unspecified__don't_match
]
(** The selection of causes of errors which have been mapped. *)

val show_cause : [< cause] -> string


(** {2 Messages} *)

type msg = ..
(** In this type, drivers can stash information about any errors in their own
    format, which can later be used for pretty-printing and or future
    operations. Drivers must {!define_msg} on each constructor added to this
    type. *)

val define_msg :
  pp: (Format.formatter -> msg -> unit) ->
  ?cause: (msg -> cause) ->
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
(** [load_rejected ~uri msg] indicates that a driver could not be identified
    from [uri]. *)

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

val encode_missing : uri: Uri.t -> field_type: 'a Caqti_type.Field.t -> unit ->
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
    not accepted by the database client library. *)

val request_failed : uri: Uri.t -> query: string -> msg ->
  [> `Request_failed of query_error]
(** [request_failed ~uri ~query msg] indicates that the request could not be
    transmitted to the database, that the database was not ready to process the
    request, or that something went wrong while processing the request. *)


(** {3 Errors during Result Retrieval} *)

val decode_missing : uri: Uri.t -> field_type: 'a Caqti_type.Field.t -> unit ->
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
  | `Request_failed of query_error
  | `Response_rejected of query_error ]

type retrieve =
  [ `Decode_rejected of coding_error
  | `Request_failed of query_error
  | `Response_failed of query_error
  | `Response_rejected of query_error ]
(** Errors which may occur during retrival of result rows.  This includes
    [`Request_failed] since the request is fused with retrieval for the pgx
    driver. *)

type call_or_retrieve = [call | retrieve]

type transact = [call | retrieve] (* TODO: Should be a subset. *)

type load =
  [ `Load_rejected of load_error
  | `Load_failed of load_error ]

type preconnect =
  [ `Connect_rejected of connection_error ]

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

val cause :
  [< `Request_failed of query_error | `Response_failed of query_error] -> cause
(** A matchable representation of the cause of the error, if available. *)

type counit = |
(** An uninhabited type used by {!uncongested}. *)

val uncongested :
  ('a, [< t | `Congested of counit]) result ->
  ('a, [> t]) result
(** [uncongested r] eliminates an unused [`Congested] case from the error. *)

exception Exn of t
(** [Exn error] can be used when an exception is preferred over explicit error
    handling.  The core Caqti API never raises exceptions which originate from
    runtime errors. *)
