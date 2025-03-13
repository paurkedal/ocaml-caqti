(* Copyright (C) 2017--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Request template.

    A request template combines a function to generate an SQL query template
    with type descriptors use to encode parameters and decode result rows.
    The function will receive information about the SQL dialect when called by
    the chosen database driver library.

    Requests are passed to {!Caqti_connection_sig.S.call} or one of its shortcut
    methods provided by a database connection handle, and will be turned into a
    prepared query, cached by the connection handle, if prepared queries are
    supported by the driver and unless explicitly disabled, see {!create} for
    details on the latter. *)

(** {2 Primitive Constructor and Accessors} *)

type prepare_policy =
  | Direct
    (** The query string is sent to the database on each request, or if the
        driver only supports prepared queries, the preprepared query will be
        released after each use.
        Most importantly nothing is retained by the driver related to request
        template created with this policy, so this is a safe option for
        dynamically generated request templates.
        This option is only a suitable choice when it is known in advance that
        the request will be executed at most once, or very rarely, such as
        schema updates. *)
  | Dynamic
    (** The query string is prepared once per connection and scheduled for
        release after the request object has been garbage collected. *)
  | Static
    (** The query string is prepared once per connection on not released before
        the connection is closed.
        This policy will cause a resource leak on long-lived connections if the
        template is dynamically generated.
        As the name suggest, this policy should only be used when the request
        template has static lifetime. *)
(** The prepare policy decides whether Caqti drivers use prepared queries and,
    if so, the expected lifetime of the template. *)

type ('a, 'b, +'m) t constraint 'm = [< `Zero | `One | `Many]
(** A request specification embedding a query generator, parameter encoder, and
    row decoder.
    - ['a] is the type of the expected parameter bundle.
    - ['b] is the type of a returned row.
    - ['m] is the possible multiplicities of returned rows. *)

val create :
  prepare_policy -> ('a, 'b, 'm) Request_type.t -> (Dialect.t -> Query.t) ->
  ('a, 'b, 'm) t
(** [create prepare_policy (arg_type, row_type, row_mult) f] is a request
    template

      - whose query will be prepared (or not) according to [prepare_policy],
      - which takes parameters of type [arg_type],
      - which returns rows of type [row_type] with multiplicity [row_mult], and
      - which submits a query string rendered from the {!Query.t} returned by
        [f di], where [di] is the {!Dialect.t} supplied by the driver library of
        the connection.

    The driver is responsible for turning parameter references into a form
    accepted by the database system, while other dialectical differences must be
    handled by [f]. *)

val prepare_policy : (_, _, _) t -> prepare_policy
(** [prepare_policy req] is the prepare policy of [req]. *)

val param_type : ('a, _, _) t -> 'a Row_type.t
(** [param_type req] is the type of parameter bundles expected by [req]. *)

val row_type : (_, 'b, _) t -> 'b Row_type.t
(** [row_type req] is the type of rows returned by [req]. *)

val row_mult : (_, _, 'm) t -> 'm Row_mult.t
(** [row_mult req] indicates how many rows [req] may return.  This is asserted
    when constructing the query. *)

val query : ('a, 'b, 'm) t -> Dialect.t -> Query.t
(** [query req] is the function which generates the query of this request
    possibly tailored for the given driver. *)


(** {2 Formatting} *)

val make_pp :
  ?dialect: Dialect.t ->
  ?subst: Query.subst ->
  unit -> Format.formatter -> ('a, 'b, 'm) t -> unit
(** [make_pp ?subst ?dialect ()] is a pretty-printer for a request, which
    expands the query using [subst] and [dialect].

    @param subst
      Used to partially expand the query string. Defaults to the empty
      substitution.

    @param dialect
      The driver info to pass to the call-back which returns the query.
      Defaults to {!Dialect.Unknown}. *)

val pp : Format.formatter -> ('a, 'b, 'm) t -> unit
(** [pp ppf req] prints [req] on [ppf] in a form suitable for human
    inspection. *)

val make_pp_with_param :
  ?dialect: Dialect.t ->
  ?subst: Query.subst ->
  unit -> Format.formatter -> ('a, 'b, 'm) t * 'a -> unit
(** [make_pp_with_param ?subst ?dialect ()] is a pretty-printer for a
    request and parameter pair.  See {!make_pp} for the optional arguments.
    This functions is meant for debugging; the output is neither guaranteed to
    be consistent across releases nor to contain a complete record of the data.
    Lost database records cannot be reconstructed from the logs.

    Due to concerns about exposure of sensitive data in debug logs, this
    function only prints the parameter values if [CAQTI_DEBUG_PARAM] is set to
    [true].  If you enable it for applications which do not consistenly annotate
    sensitive parameters with {!Row_type.redacted}, make sure your debug logs
    are well-secured. *)

(**/**)
[@@@alert "-caqti_private"]

val query_id : ('a, 'b, 'm) t -> int option
[@@alert caqti_private]

type liveness_witness
[@@alert caqti_private]

val liveness_witness : (_, _, _) t -> liveness_witness
[@@alert caqti_private]
