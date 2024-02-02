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

(** {2 Primitives} *)

type ('a, 'b, +'m) t constraint 'm = [< `Zero | `One | `Many]
(** A request specification embedding a query generator, parameter encoder, and
    row decoder.
    - ['a] is the type of the expected parameter bundle.
    - ['b] is the type of a returned row.
    - ['m] is the possible multiplicities of returned rows. *)

val create :
  ?oneshot: bool ->
  'a Row_type.t -> 'b Row_type.t -> 'm Row_mult.t ->
  (Driver_info.t -> Query.t) -> ('a, 'b, 'm) t
(** [create arg_type row_type row_mult f] is a request which takes parameters of
    type [arg_type], returns rows of type [row_type] with multiplicity
    [row_mult], and which sends query strings generated from the query [f di],
    where [di] is the {!Driver_info.t} of the target driver.  The driver is
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

val query : ('a, 'b, 'm) t -> Driver_info.t -> Query.t
(** [query req] is the function which generates the query of this request
    possibly tailored for the given driver. *)


(** {2 Printing} *)

val make_pp :
  ?env: (Driver_info.t -> string -> Query.t) ->
  ?driver_info: Driver_info.t ->
  unit -> Format.formatter -> ('a, 'b, 'm) t -> unit
(** [make_pp ?env ?driver_info ()] is a pretty-printer for a request, which
    expands the query using [env] and [driver_info].

    @param env
      Used to partially expand the query string.  Defaults to the empty
      environment.

    @param driver_info
      The driver info to pass to the call-back which returns the query.
      Defaults to {!Driver_info.dummy}. *)

val pp : Format.formatter -> ('a, 'b, 'm) t -> unit
(** [pp ppf req] prints [req] on [ppf] in a form suitable for human
    inspection. *)

val make_pp_with_param :
  ?env: (Driver_info.t -> string -> Query.t) ->
  ?driver_info: Driver_info.t ->
  unit -> Format.formatter -> ('a, 'b, 'm) t * 'a -> unit
(** [make_pp_with_param ?env ?driver_info ()] is a pretty-printer for a
    request and parameter pair.  See {!make_pp} for the optional arguments.
    This functions is meant for debugging; the output is neither guaranteed to
    be consistent across releases nor to contain a complete record of the data.
    Lost database records cannot be reconstructed from the logs.

    Due to concerns about exposure of sensitive data in debug logs, this
    function only prints the parameter values if [CAQTI_DEBUG_PARAM] is set to
    [true].  If you enable it for applications which do not consistenly annotate
    sensitive parameters with {!Row_type.redacted}, make sure your debug logs
    are well-secured. *)

(** {2 How to Dynamically Assemble Queries and Parameters}

    This can now be done more simply using {!Query.V}.

    In some cases, queries are constructed dynamically, e.g. when translating an
    expression for searching a database into SQL.  In such cases the number of
    parameters and their types will typically vary, as well.  A helper like the
    following can be used to existentially pack the parameter types along with
    the corresponding parameter values to allow collecing them incrementally:
    {[
      module Dynparam = struct
        open Caqti_template
        type t = Pack : 'a Row_type.t * 'a -> t
        let empty = Pack (Row_type.unit, ())
        let add t x (Pack (t', x')) = Pack (Row_type.t2 t' t, (x', x))
      end
    ]}
    Now, given a [param : Dynparam.t] and a corresponding query string [qs], one
    can construct a request and execute it:
    {[
      let Dynparam.Pack (pt, pv) = param in
      let req = Caqti_template.Std.(pt -->. unit) ~oneshot:true pt qs in
      C.exec req pv
    ]}
    Note that dynamically constructed requests should have [~oneshot:true]
    unless they are memoized.  Also note that it is natural to use {!create} for
    dynamically constructed queries, since it accepts the easily composible
    {!Query.t} type instead of plain strings.

    This scheme can be specialized for particular use cases, including
    generation of fragments of the [query], which reduces the risk of wrongly
    matching up parameters with their uses in the query string.
    *)
