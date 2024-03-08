(* Copyright (C) 2023--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Format-based query construction. *)

type 'a t = Format.formatter -> 'a -> unit
(** The type of a function which formats values of type ['a] or fragments based
    on an input of type ['a]. *)

val qprintf : ('a, Format.formatter, unit, Query.t) format4 -> 'a
(** {!qprintf} allows building Caqti queries using a printf-style interface.

    When using {!qprintf}, you can use the {!query}, {!quote}, {!env} and
    {!param} printers from this module to generate the corresponding query
    fragments.

    In addition, you can use the "Q" and "E" string tags to delimit portions of
    the formatting string that should be interpreted as quotes and environment
    variables, respectively. The "Q" and "E" tags can not be nested: within the
    tags, {!qprintf} behaves no differently than {!Format.asprintf} and will
    generate a string, not a query (only when the tag is closed does the string
    get converted into a query).

    The two following calls to {!qprintf}:
    {[
      qprintf "FUNC(@{<Q>Quoted value with %d format(s)})" 1
    ]}
    and
    {[
      qprintf "FUNC(%a)" quote (Format.asprintf "Quoted value with %d format(s)" 1)
    ]}
    are functionally equivalent. Both compute
    {[
      S [L "FUNC("; Q "Quoted value with 1 format(s)"; L ")"]
    ]}
    but the first one is nicer to work with.

    @raise Failure if the "Q" and "E" tags are nested.
*)

val kqprintf : (Query.t -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
(** {!kqprintf} is the continuation-passing version of {!qprintf} (like
    {!Format.kasprintf} for {!Format.asprintf}).

    You usually want [qprintf] instead. *)

val param : int t
(** {!param} is a formatter that includes the corresponding parameter in a
    query built by {!qprintf}.

    Note that to include a parameter in a query, {!param} *must* be used: using
    literal ["$"] or ["?"] will be sent as-is to the SQL driver and will not be
    processed by Caqti.
*)

val env : string t
(** {!env} is a formatter that includes the corresponding environment variable
    in a query built by {!qprintf}.

    Note that to include an environment variable in a query, {!env} *must* be
    used: using literal ["$(...)"] will be sent as-is to the SQL driver and
    will not be processed by Caqti. *)

val quote : string t
(** {!quote} is a formatter that includes a TEXT literal in a query built by
    {!qprintf}. *)

val query : Query.t t
(** {!query} can be used with {!qprintf} to embed a query that was already
    parsed in the format string. Direct use of {!query} should be rare, and
    {!param}, {!env}, or {!quote} should be used instead when possible.

    Using {!query} with any other formatter will ignore the query and instead
    print a dummy value (currently ["... SQL FRAGMENT ..."]) instead. *)

(** {2 Value Formatters}

    The following formatters emit values of basic field types by passing them as
    parameters.  This is done by emitting a {!Caqti_template.Query.V} node with
    a appropriate field type. *)

val bool : bool t
val int : int t
val float : float t
val string : string t
val octets : string t
val pdate : Ptime.t t
val ptime : Ptime.t t
val ptime_span : Ptime.span t
