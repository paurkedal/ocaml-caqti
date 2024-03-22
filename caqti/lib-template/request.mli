(* Copyright (C) 2017--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Request specification.

    A Caqti request is a function to generate a query string from information
    about the driver, along with type descriptors to encode parameters and
    decode rows returned from the same query.  Requests are passed to
    {!Caqti_connection_sig.S.call} or one of its shortcut methods provided by a
    database connection handle.

    The request often represent a prepared query, in which case it is static and
    can be defined directly in a module scope.  However, an optional [oneshot]
    parameter may be passed to indicate a dynamically generated query. *)

(** {2 Primitive Constructor and Accessors} *)

type ('a, 'b, +'m) t constraint 'm = [< `Zero | `One | `Many]
(** A request specification embedding a query generator, parameter encoder, and
    row decoder.
    - ['a] is the type of the expected parameter bundle.
    - ['b] is the type of a returned row.
    - ['m] is the possible multiplicities of returned rows. *)

val create :
  ?oneshot: bool ->
  'a Row_type.t -> 'b Row_type.t -> 'm Row_mult.t ->
  (Dialect.t -> Query.t) -> ('a, 'b, 'm) t
(** [create arg_type row_type row_mult f] is a request which takes parameters of
    type [arg_type], returns rows of type [row_type] with multiplicity
    [row_mult], and which sends query strings generated from the query [f di],
    where [di] is the {!Dialect.t} of the target driver.  The driver is
    responsible for turning parameter references into a form accepted by the
    database, while other differences must be handled by [f].

    @param oneshot
      Disables caching of a prepared statements on connections for this query.

        - If false (the default), the statement is prepared and a handle is
          permanently attached to the connection object right before the first
          time it is executed.

        - If true, everything allocated in order to execute the statement is
          released after use.

      In other words, the default is suitable for queries which are bound to
      static modules.  Conversely, you should pass [~oneshot:true] if the query
      is dynamically generated, whether it is within a function or a dynamic
      module, since there will otherwise be a memory leak associated with
      long-lived connections.  You might as well also pass [~oneshot:true] if
      you know that the query will only executed at most once (or a very few
      times) on each connection. *)

val param_type : ('a, _, _) t -> 'a Row_type.t
(** [param_type req] is the type of parameter bundles expected by [req]. *)

val row_type : (_, 'b, _) t -> 'b Row_type.t
(** [row_type req] is the type of rows returned by [req]. *)

val row_mult : (_, _, 'm) t -> 'm Row_mult.t
(** [row_mult req] indicates how many rows [req] may return.  This is asserted
    when constructing the query. *)

val query_id : ('a, 'b, 'm) t -> int option
(** If [req] is a prepared query, then [query_id req] is [Some id] for some [id]
    which uniquely identifies [req], otherwise it is [None]. *)

val query : ('a, 'b, 'm) t -> Dialect.t -> Query.t
(** [query req] is the function which generates the query of this request
    possibly tailored for the given driver. *)


(** They are implemented in terms of {!Request.create} and
    {!Query.of_string_exn}, meaning that the query string arguments accepts
    {{!query_template} The Syntax of Query Templates}.

    The [?oneshot] argument defaults to [false], so when not constructing
    one-shot queries, the full application [(pt -->! rt) f] can be written
    [pt -->! rt @@ f], which motivates the {!(@:-)} and {!(@@:-)} shortcuts. *)
module Infix : sig
  (** {3:indep Constructors for Driver-Independent Requests} *)

  val ( ->. ) :
    'a Row_type.t -> unit Row_type.t ->
    ?oneshot: bool -> string -> ('a, unit, [`Zero]) t
  (** [(pt ->. Row_type.unit) ?oneshot s] is the request which sends the query
      string [s], encodes parameters according to [pt], and expects no result
      rows. See {!Caqti_template.Request.create} for the meaning of [oneshot].
      *)

  val ( ->! ) :
    'a Row_type.t -> 'b Row_type.t ->
    ?oneshot: bool -> string -> ('a, 'b, [`One]) t
  (** [(pt ->! rt) ?oneshot s] is the request which sends the query string [s],
      encodes parameters according to [pt], and decodes a single result row
      according to [rt]. See {!Caqti_template.Request.create} for the meaning of
      [oneshot]. *)

  val ( ->? ) :
    'a Row_type.t -> 'b Row_type.t ->
    ?oneshot: bool -> string -> ('a, 'b, [`Zero | `One]) t
  (** [(pt ->? rt) ?oneshot s] is the request which sends the query string [s],
      encodes parameters according to [pt], and decodes zero or one result row
      according to [rt]. See {!Caqti_template.Request.create} for the meaning of
      [oneshot]. *)

  val ( ->* ) :
    'a Row_type.t -> 'b Row_type.t ->
    ?oneshot: bool -> string -> ('a, 'b, [`Zero | `One | `Many]) t
  (** [(pt ->* rt) ?oneshot s] is the request which sends the query string [s],
      encodes parameters according to [pt], and decodes any number of result
      rows according to [rt]. See {!Caqti_template.Request.create} for the
      meaning of [oneshot]. *)

  (** {3 Constructors for Driver-Dependent Requests}

      The below arrow operators takes a function instead of a string as their
      third argument.  The function receives information about the current
      driver and returns a {!Query.t}.  This is the most general way of
      providing the query string.

      As an alternative to using plain application (or [@@]) for the third
      positional argument, additional application operators are provided for
      convenience. *)

  val ( -->. ) :
    'a Row_type.t -> unit Row_type.t ->
    ?oneshot: bool -> (Dialect.t -> Query.t) ->
    ('a, unit, [`Zero]) t
  (** [(pt -->. Row_type.unit) ?oneshot f] is the request which sends the query
      string returned by [f], encodes parameters according to [pt], and expects
      no result rows. See {!Caqti_template.Request.create} for the meaning of
      [oneshot]. *)

  val ( -->! ) :
    'a Row_type.t -> 'b Row_type.t ->
    ?oneshot: bool -> (Dialect.t -> Query.t) ->
    ('a, 'b, [`One]) t
  (** [(pt -->! rt) ?oneshot f] is the request which sends the query string
      returned by [f], encodes parameters according to [pt], and decodes a
      single result row according to [rt]. See {!Caqti_template.Request.create}
      for the meaning of [oneshot]. *)

  val ( -->? ) :
    'a Row_type.t -> 'b Row_type.t ->
    ?oneshot: bool -> (Dialect.t -> Query.t) ->
    ('a, 'b, [`Zero | `One]) t
  (** [(pt -->? rt) ?oneshot f] is the request which sends the query string
      returned by [f], encodes parameters according to [pt], and decodes zero or
      one result row according to [rt]. See {!Caqti_template.Request.create} for
      the meaning of [oneshot]. *)

  val ( -->* ) :
    'a Row_type.t -> 'b Row_type.t ->
    ?oneshot: bool -> (Dialect.t -> Query.t) ->
    ('a, 'b, [`Zero | `One | `Many]) t
  (** [(pt -->* rt) ?oneshot f] is the request which sends the query string
      returned by [f], encodes parameters according to [pt], and decodes any
      number of result rows according to [rt]. See
      {!Caqti_template.Request.create} for the meaning of [oneshot]. *)

  val ( @@:- ) :
    ((Dialect.t -> Query.t) -> ('a, 'b, 'm) t) ->
    (Dialect.t -> string) -> ('a, 'b, 'm) t
  (** Applies a dialect-dependent query string which is parsed with
      {!Caqti_template.Query.of_string_exn}. *)
end

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
