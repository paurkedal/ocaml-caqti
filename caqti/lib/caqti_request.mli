(* Copyright (C) 2017--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(**/**)
[@@@warning "-3"]
type query = Caqti_query.t =
  | L of string [@deprecated "Moved to Caqti_query"]
  | Q of string [@deprecated "Moved to Caqti_query"]
  | P of int [@deprecated "Moved to Caqti_query"]
  | E of string [@deprecated "Moved to Caqti_query"]
  | S of query list [@deprecated "Moved to Caqti_query"]
[@@deprecated "Moved to Caqti_query.t"]
[@@@warning "+3"]
(**/**)

type ('a, 'b, +'m) t constraint 'm = [< `Zero | `One | `Many]
(** A request specification embedding a query generator, parameter encoder, and
    row decoder.
    - ['a] is the type of the expected parameter bundle.
    - ['b] is the type of a returned row.
    - ['m] is the possible multiplicities of returned rows. *)

val create :
  ?oneshot: bool ->
  'a Caqti_type.t -> 'b Caqti_type.t -> 'm Caqti_mult.t ->
  (Caqti_driver_info.t -> Caqti_query.t) -> ('a, 'b, 'm) t
(** [create arg_type row_type row_mult f] is a request which takes parameters of
    type [arg_type], returns rows of type [row_type] with multiplicity
    [row_mult], and which sends query strings generated from the query [f di],
    where [di] is the {!Caqti_driver_info.t} of the target driver.  The driver
    is responsible for turning parameter references into a form accepted by the
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

val param_type : ('a, _, _) t -> 'a Caqti_type.t
(** [param_type req] is the type of parameter bundles expected by [req]. *)

val row_type : (_, 'b, _) t -> 'b Caqti_type.t
(** [row_type req] is the type of rows returned by [req]. *)

val row_mult : (_, _, 'm) t -> 'm Caqti_mult.t
(** [row_mult req] indicates how many rows [req] may return.  This is asserted
    when constructing the query. *)

val query_id : ('a, 'b, 'm) t -> int option
(** If [req] is a prepared query, then [query_id req] is [Some id] for some [id]
    which uniquely identifies [req], otherwise it is [None]. *)

val query : ('a, 'b, 'm) t -> Caqti_driver_info.t -> Caqti_query.t
(** [query req] is the function which generates the query of this request
    possibly tailored for the given driver. *)


(** {2 Convenience Interface} *)

module Infix : sig
  (** The following operators provides a more visually appealing way of
      expressing requests.  They are implemented in terms of {!create} and
      {!Caqti_query.of_string_exn}, meaning the the query string arguments
      accepts {{!query_template} The Syntax of Query Templates}.

      The [?oneshot] argument defaults to [false], so when not constructing
      one-shot queries, the full application [(pt -->! rt) f] can be written
      [pt -->! rt @@ f], which motivates the {!(@:-)} and {!(@@:-)} shortcuts.

      In the simplest case you can use this module directly together with
      {!Caqti_type}:
      {[
        let bounds_upto_req =
          let open Caqti_type.Std in
          let open Caqti_request.Infix in
          tup2 int32 float -->! option (tup2 float float) @:-
          "SELECT min(y), max(y) FROM samples WHERE series_id = ? AND x < ?"
      ]}
      For more complex applications it may be convenient to provide a custom
      module, to avoid the double open and to customize the operators, e.g.
      {[
        module Caqtireq = struct
          include Caqti_type.Std
          include Caqti_type_calendar (* if needed, link caqti-type-calendar *)
          include Caqti_request.Infix

          (* Any additional types. *)
          let password = redacted string
          let uri =
            custom ~encode:(fun x -> Ok (Uri.to_string x))
                   ~decode:(fun s -> Ok (Uri.of_string s)) string

          (* Optionally define a custom environment providing two schema names.
           * The references should only be assigned at startup. *)
          let myapp_schema = ref "myapp"
          let mylib_schema = ref "mylib"
          let env = function
           | "" -> Caqti_query.L !myapp_schema
           | "mylib" -> Caqti_query.L !mylib_schema
           | _ -> raise Not_found

          (* Since we have a custom environment, override the definitions of the
           * following operators to perform the substitution. *)
          let (@:-) t qs =
            let q = Caqti_query.expand env (Caqti_query.of_string_exn qs) in
            t (fun _ -> q)
          let (@@:-) t qsf =
            t (fun driver_info ->
              let qs = qsf (Caqti_driver_info.dialect_tag driver_info) in
              Caqti_query.expand env (Caqti_query.of_string_exn qs))
        end
      ]}
      If you don't like using global references, or you need to work with
      different enviroments for different connections, you should instead pass
      the environment function when connecting to the database.  We can now
      simplify and schema-qualify the previous request,
      {[
        let bounds_upto_req =
          let open Caqtireq in
          tup2 int32 float -->! option (tup2 float float) @:-
          "SELECT min(y), max(y) FROM $.samples WHERE series_id = ? AND x < ?"
      ]}
      {!section:indep} also provides alternative arrow operators for this common
      case, which allows using short-from local open,
      {[
        let bounds_upto_req =
          Caqtireq.(tup2 int32 float ->! option (tup2 float float))
          "SELECT min(y), max(y) FROM $.samples WHERE series_id = ? AND x < ?"
      ]}
   *)

  (** {2:indep Constructors for Driver-Independent Requests} *)

  val ( ->. ) :
    'a Caqti_type.t -> unit Caqti_type.t ->
    ?oneshot: bool -> string -> ('a, unit, [`Zero]) t
  (** [(pt ->. Caqti_type.unit) ?oneshot s] is the request which sends the
      query string [s], encodes parameters according to [pt], and expects no
      result rows. See {!create} for the meaning of [oneshot]. *)

  val ( ->! ) :
    'a Caqti_type.t -> 'b Caqti_type.t ->
    ?oneshot: bool -> string -> ('a, 'b, [`One]) t
  (** [(pt ->! rt) ?oneshot s] is the request which sends the query string [s],
      encodes parameters according to [pt], and decodes a single result row
      according to [rt]. See {!create} for the meaning of [oneshot]. *)

  val ( ->? ) :
    'a Caqti_type.t -> 'b Caqti_type.t ->
    ?oneshot: bool -> string -> ('a, 'b, [`Zero | `One]) t
  (** [(pt ->? rt) ?oneshot s] is the request which sends the query string [s],
      encodes parameters according to [pt], and decodes zero or one result row
      according to [rt]. See {!create} for the meaning of [oneshot]. *)

  val ( ->* ) :
    'a Caqti_type.t -> 'b Caqti_type.t ->
    ?oneshot: bool -> string -> ('a, 'b, [`Zero | `One | `Many]) t
  (** [(pt ->* rt) ?oneshot s] is the request which sends the query string [s],
      encodes parameters according to [pt], and decodes any number of result
      rows according to [rt]. See {!create} for the meaning of [oneshot]. *)

  (** {2 Constructors for Driver-Dependent Requests}

      The below arrow operators takes a function instead of a string as their
      third argument.  The function receives information about the current
      driver and returns a {!Caqti_query.t}.  This is the most general way of
      providing the query string.

      As an alternative to using plain application (or [@@]) for the third
      positional argument, additional application operators are provided for
      convenience. *)

  val ( -->. ) :
    'a Caqti_type.t -> unit Caqti_type.t ->
    ?oneshot: bool -> (Caqti_driver_info.t -> Caqti_query.t) ->
    ('a, unit, [`Zero]) t
  (** [(pt -->. Caqti_type.unit) ?oneshot f] is the request which sends the
      query string returned by [f], encodes parameters according to [pt], and
      expects no result rows. See {!create} for the meaning of [oneshot]. *)

  val ( -->! ) :
    'a Caqti_type.t -> 'b Caqti_type.t ->
    ?oneshot: bool -> (Caqti_driver_info.t -> Caqti_query.t) ->
    ('a, 'b, [`One]) t
  (** [(pt -->! rt) ?oneshot f] is the request which sends the query string
      returned by [f], encodes parameters according to [pt], and decodes a
      single result row according to [rt]. See {!create} for the meaning of
      [oneshot]. *)

  val ( -->? ) :
    'a Caqti_type.t -> 'b Caqti_type.t ->
    ?oneshot: bool -> (Caqti_driver_info.t -> Caqti_query.t) ->
    ('a, 'b, [`Zero | `One]) t
  (** [(pt -->? rt) ?oneshot f] is the request which sends the query string
      returned by [f], encodes parameters according to [pt], and decodes zero or
      one result row according to [rt]. See {!create} for the meaning of
      [oneshot]. *)

  val ( -->* ) :
    'a Caqti_type.t -> 'b Caqti_type.t ->
    ?oneshot: bool -> (Caqti_driver_info.t -> Caqti_query.t) ->
    ('a, 'b, [`Zero | `One | `Many]) t
  (** [(pt -->* rt) ?oneshot f] is the request which sends the query string
      returned by [f], encodes parameters according to [pt], and decodes any
      number of result rows according to [rt]. See {!create} for the meaning of
      [oneshot]. *)

  (**/**)
  val ( --> ) :
    'a Caqti_type.t -> 'b Caqti_type.t ->
    ?oneshot: bool -> (Caqti_driver_info.t -> Caqti_query.t) ->
    ('a, 'b, [`One]) t
  [@@deprecated "Renamed to (-->!) for consistency with (->!)."]
  (**/**)

  val ( @:- ) :
    ((Caqti_driver_info.t -> Caqti_query.t) -> ('a, 'b, 'm) t) ->
    string -> ('a, 'b, 'm) t
  (** Applies a dialect-independent query string which is parsed with
      {!Caqti_query.of_string_exn}.  Composition with arrow operators from this
      section, gives the corresponding operators from {!section:indep}. *)

  val ( @@:- ) :
    ((Caqti_driver_info.t -> Caqti_query.t) -> ('a, 'b, 'm) t) ->
    (Caqti_driver_info.dialect_tag -> string) -> ('a, 'b, 'm) t
  (** Applies a dialect-dependent query string which is parsed with
      {!Caqti_query.of_string_exn}. *)

end


(**/**)
val create_p :
  ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
  ?oneshot: bool ->
  'a Caqti_type.t -> 'b Caqti_type.t -> 'm Caqti_mult.t ->
  (Caqti_driver_info.t -> string) -> ('a, 'b, 'm) t
[@@deprecated "Use create and Caqti_query.of_string_exn or the Infix module."]
val exec :
  ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
  ?oneshot: bool ->
  'a Caqti_type.t ->
  string -> ('a, unit, [> `Zero]) t
[@@deprecated "Replaced by the Infix module."]
val find :
  ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
  ?oneshot: bool ->
  'a Caqti_type.t -> 'b Caqti_type.t ->
  string -> ('a, 'b, [> `One]) t
[@@deprecated "Replaced by the Infix module."]
val find_opt :
  ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
  ?oneshot: bool ->
  'a Caqti_type.t -> 'b Caqti_type.t ->
  string -> ('a, 'b, [> `Zero | `One]) t
[@@deprecated "Replaced by the Infix module."]
val collect :
  ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
  ?oneshot: bool ->
  'a Caqti_type.t -> 'b Caqti_type.t ->
  string -> ('a, 'b, [> `Zero | `One | `Many]) t
[@@deprecated "Replaced by the Infix module."]
(**/**)


(** {2 Printing} *)

val make_pp :
  ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
  ?driver_info: Caqti_driver_info.t ->
  unit -> Format.formatter -> ('a, 'b, 'm) t -> unit
(** [make_pp ?env ?driver_info ()] is a pretty-printer for a request, which
    expands the query using [env] and [driver_info].

    @param env
      Used to partially expand the query string.  Defaults to the empty
      environment.

    @param driver_info
      The driver info to pass to the call-back which returns the query.
      Defaults to {!Caqti_driver_info.dummy}. *)

val pp : Format.formatter -> ('a, 'b, 'm) t -> unit
(** [pp ppf req] prints [req] on [ppf] in a form suitable for human
    inspection. *)

val make_pp_with_param :
  ?env: (Caqti_driver_info.t -> string -> Caqti_query.t) ->
  ?driver_info: Caqti_driver_info.t ->
  unit -> Format.formatter -> ('a, 'b, 'm) t * 'a -> unit
(** [make_pp_with_param ?env ?driver_info ()] is a pretty-printer for a
    request and parameter pair.  See {!make_pp} for the optional arguments.
    This functions is meant for debugging; the output is neither guaranteed to
    be consistent across releases nor to contain a complete record of the data.
    Lost database records cannot be reconstructed from the logs.

    Due to concerns about exposure of sensitive data in debug logs, this
    function only prints the parameter values if [CAQTI_DEBUG_PARAM] is set to
    [true].  If you enable it for applications which do not consistenly annotate
    sensitive parameters with {!Caqti_type.redacted}, make sure your debug logs
    are well-secured. *)

val pp_with_param :
  ?driver_info: Caqti_driver_info.t ->
  Format.formatter -> ('a, 'b, 'm) t * 'a -> unit
[@@deprecated "Use make_pp_with_param."]

(** {2 How to Dynamically Assemble Queries and Parameters}

    In some cases, queries are constructed dynamically, e.g. when translating an
    expression for searching a database into SQL.  In such cases the number of
    parameters and their types will typically vary, as well.  A helper like the
    following can be used to existentially pack the parameter types along with
    the corresponding parameter values to allow collecing them incrementally:
    {[
      module Dynparam = struct
        type t = Pack : 'a Caqti_type.t * 'a -> t
        let empty = Pack (Caqti_type.unit, ())
        let add t x (Pack (t', x')) = Pack (Caqti_type.tup2 t' t, (x', x))
      end
    ]}
    Now, given a [param : Dynparam.t] and a corresponding query string [qs], one
    can construct a request and execute it:
    {[
      let Dynparam.Pack (pt, pv) = param in
      let req = Caqti_request.exec ~oneshot:true pt qs in
      C.exec req pv
    ]}
    Note that dynamically constructed requests should have [~oneshot:true]
    unless they are memoized.  Also note that it is natural to use {!create} for
    dynamically constructed queries, since it accepts the easily composible
    {!Caqti_query.t} type instead of plain strings.

    This scheme can be specialized for particular use cases, including
    generation of fragments of the [query], which reduces the risk of wrongly
    matching up parameters with their uses in the query string.
    *)
