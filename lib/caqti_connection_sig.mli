(* Copyright (C) 2017--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signature of connection handles.

    You can obtain a connection handle from {!Caqti_lwt} or {!Caqti_async},
    which both implement the {!Caqti_connect_sig} interface.

    While {!Caqti_request.t} objects hold SQL code to be sent to the database,
    connection handles defined here provide the means to execute them with
    actual parameters on an RDBMS.  So, there is a separation between
    preparation and execution.  This is motivated by the common support for
    prepared queries in database client libraries, and by the desire to keep the
    possibly deeply nested data-processing code uncluttered by strings of SQL
    code.  For this separation to be reasonably safe, the request declares the
    types of parameters and result, and these type declarations are placed right
    next to the SQL code, so that we can rely on OCaml's powers of refactoring
    large code bases safely.

    The result type of {!Caqti_request.t} only describes how to decode {e
    individual rows}, leaving the decision of how to process multiple rows to
    the execution interface.  That is, the request does not detail how to fold
    over rows, and for this reason, the separation into request constructors and
    executors entails a collection of pairwise compatible convenience functions
    across multiplicities between the APIs.  E.g. when there are like-named
    {!Caqti_request.find_opt} and {!S.find_opt} functions, the first declares
    the row multiplicity, while the latter implements a way to fold rows into an
    OCaml datatype for that multiplicity.

    Though a request object stipulates the expected multiplicity, there is
    still choice left for how to fold rows.  E.g., a request constructed by
    {!Caqti_request.collect}, or in fact any request, can be processed with
    {!S.fold}, {!S.fold_s}, {!S.iter_s}, or {!S.collect_list}.  The two
    functions {!Caqti_request.create} and {!S.fold} can construct and process
    any supported SQL query. *)

(** Essential connection signature implemented by drivers. *)
module type Base = sig
  type 'a future


  (** {2 Query} *)

  module Response : Caqti_response_sig.S with type 'a future := 'a future

  val call :
    f: (('b, 'm) Response.t -> ('c, 'e) result future) ->
    ('a, 'b, 'm) Caqti_request.t -> 'a ->
    ('c, [> Caqti_error.call] as 'e) result future
  (** [call ~f request params] performs [request] with parameters [params]
      invoking [f] to process the result.  The argument of [f] is only valid
      during the call to [f], and must to be returned or operated on by other
      threads. *)


  (** {2 Transactions} *)

  val start : unit -> (unit, [> Caqti_error.transact]) result future
  (** Starts a transaction if supported by the underlying database, otherwise
      does nothing. *)

  val commit : unit -> (unit, [> Caqti_error.transact]) result future
  (** Commits the current transaction if supported by the underlying database,
      otherwise does nothing. *)

  val rollback : unit -> (unit, [> Caqti_error.transact]) result future
  (** Rolls back a transaction if supported by the underlying database,
      otherwise does nothing. *)


  (** {2 Disconnection and Reuse} *)

  val disconnect : unit -> unit future
  (** Calling [disconnect ()] closes the connection to the database and frees
      up related resources. *)

  val validate : unit -> bool future
  (** For internal use by {!Caqti_pool}.  Tries to ensure the validity of the
      connection and must return [false] if unsuccessful. *)

  val check : (bool -> unit) -> unit
  (** For internal use by {!Caqti_pool}.  Called after a connection has been
      used.  [check f] must call [f ()] exactly once with an argument
      indicating whether to keep the connection in the pool or discard it. *)

end

(** Full connection signature available to users. *)
module type S = sig
  include Base

  val driver_info : Caqti_driver_info.t
  (** Information about the driver providing this connection module. *)


  (** {2 Retrieval Convenience}

      These are shortcuts for {!call} combined with retrieval functions from
      {!Caqti_response_sig.S} of the same name. *)

  val exec :
    ('a, unit, [< `Zero]) Caqti_request.t -> 'a ->
    (unit, [> Caqti_error.call_or_retrieve] as 'e) result future
  (** Combining {!call} with {!Response.exec}, this sends a request to the
      database and checks that no rows are returned. *)

  val find :
    ('a, 'b, [< `One]) Caqti_request.t -> 'a ->
    ('b, [> Caqti_error.call_or_retrieve] as 'e) result future
  (** Combining {!call} with {!Response.find}, this sends a request to the
      database, checks that a single row is returned, and extracts it. *)

  val find_opt :
    ('a, 'b, [< `Zero | `One]) Caqti_request.t -> 'a ->
    ('b option, [> Caqti_error.call_or_retrieve] as 'e) result future
  (** Combining {!call} with {!Response.find_opt}, this sends a request to the
      database, checks that at most one row is returned, and extracts it if
      present. *)

  val fold :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t ->
    ('b -> 'c -> 'c) ->
    'a -> 'c -> ('c, [> Caqti_error.call_or_retrieve] as 'e) result future
  (** Combining {!call} with {!Response.fold}, this sends a request to the
      database and folds over the result rows. *)

  val fold_s :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t ->
    ('b -> 'c -> ('c, 'e) result future) ->
    'a -> 'c -> ('c, [> Caqti_error.call_or_retrieve] as 'e) result future
  (** Combining {!call} with {!Response.fold_s}, this sends a request to the
      database and folds concurrently over the result rows.

      Please be aware of possible deadlocks when using resources from the
      callback.  In particular, if the same connection pool is invoked as the
      one used to obtain the current connection, it will deadlock if the pool
      has just run out of connections.  An alternative is to collect the rows
      first e.g. with {!fold} and do the nested queries after exiting. *)

  val iter_s :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t ->
    ('b -> (unit, 'e) result future) ->
    'a -> (unit, [> Caqti_error.call_or_retrieve] as 'e) result future
  (** Combining {!call} with {!Response.iter_s}, this sends a request to the
      database and iterates concurrently over the result rows.  Please see the
      warning in {!fold_s} about resource usage in the callback. *)

  val collect_list :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t -> 'a ->
    ('b list, [> Caqti_error.call_or_retrieve] as 'e) result future
  (** [collect_list request param] performs a {!call} on [request], extracting
      the result as a list. *)

  val rev_collect_list :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t -> 'a ->
    ('b list, [> Caqti_error.call_or_retrieve] as 'e) result future
  (** [rev_collect_list request param] performs a {!call} on [request],
      extracting the result as a reversed list.  This is more efficient than
      {!find_list} and fits well with a subsequent {!List.rev_map}, though it
      may not matter much in practise. *)
end
