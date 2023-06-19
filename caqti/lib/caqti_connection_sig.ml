(* Copyright (C) 2017--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signature of connection handles.

    The main signature {!S} of this module represents a database connection
    handle.  This is obtained by {{!Caqti_connect_sig} connection functions}
    implemented in the subpackages [caqti-async], [caqti-eio], [caqti-lwt], and
    [caqti-mirage].

    While values of {!Caqti_request.t} hold SQL code to be sent to the database,
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
    the execution interface.  Therefore, for each request constructor from
    {!Caqti_request.Infix}, there are one or more matching retrieval functions
    in the present signature. *)

type driver_connection = ..
(** This type is only to be extended by drivers. *)

(** Essential connection signature implemented by drivers. *)
module type Base = sig
  type +'a fiber
  type (+'a, +'err) stream

  (** {2 Query} *)

  module Response : Caqti_response_sig.S
    with type 'a fiber := 'a fiber
     and type ('a, 'err) stream := ('a, 'err) stream

  val call :
    f: (('b, 'm) Response.t -> ('c, 'e) result fiber) ->
    ('a, 'b, 'm) Caqti_request.t -> 'a ->
    ('c, [> Caqti_error.call] as 'e) result fiber
  (** [call ~f request params] executes [request] with parameters [params]
      invoking [f] to process the result; except the driver may postpone the
      request until [f] attempts to retrieve the result.

      One of the {{!Response.result_retrieval} result retrieval}
      functions must be called exactly once before [f] returns a non-error
      result.  If a result retrieval function is not called, it is unspecified
      whether the database query has been issued.

      The argument of [f] is only valid during the call to [f], and must not be
      returned or operated on by other threads. *)

  val set_statement_timeout :
    float option -> (unit, [> Caqti_error.call]) result fiber
  (** Set or clear the timeout after which a running SQL statement will be
      terminated if supported by the driver.
      This is currently supported for MariaDB (using [max_statement_time]) and
      PostgreSQL (using [statement_timeout]) and has no effect for SQLite3. *)


  (** {2 Transactions} *)

  val start : unit -> (unit, [> Caqti_error.transact]) result fiber
  (** Starts a transaction if supported by the underlying database, otherwise
      does nothing. *)

  val commit : unit -> (unit, [> Caqti_error.transact]) result fiber
  (** Commits the current transaction if supported by the underlying database,
      otherwise does nothing. *)

  val rollback : unit -> (unit, [> Caqti_error.transact]) result fiber
  (** Rolls back a transaction if supported by the underlying database,
      otherwise does nothing. *)


  (** {2 Disconnection and Reuse} *)

  val deallocate :
    ('a, 'b, 'm) Caqti_request.t -> (unit, [> Caqti_error.call]) result fiber
  (** [deallocate req] deallocates the prepared query for [req] if it was
      allocated. The request must not be oneshot. *)

  val disconnect : unit -> unit fiber
  (** Calling [disconnect ()] closes the connection to the database and frees
      up related resources. *)

  val validate : unit -> bool fiber
  (** For internal use by pool implementations.  Tries to ensure the validity of
      the connection and must return [false] if unsuccessful. *)

  val check : (bool -> unit) -> unit
  (** For internal use by pool implementations.  Called after a connection has
      been used.  [check f] must call [f ()] exactly once with an argument
      indicating whether to keep the connection in the pool or discard it. *)

end

module type Convenience = sig
  type +'a fiber

  (** {2 Retrieval Convenience}

      Each of these shortcuts combine [call] with the correspondingly named
      retrieval function from {!Caqti_response_sig.S}. *)

  val exec :
    ('a, unit, [< `Zero]) Caqti_request.t -> 'a ->
    (unit, [> Caqti_error.call_or_retrieve] as 'e) result fiber
  (** [exec req x] performs [req] with parameters [x] and checks that no rows
      are returned.
      See also {!Caqti_response_sig.S.exec}. *)

  val exec_with_affected_count :
    ('a, unit, [< `Zero]) Caqti_request.t -> 'a ->
    (int, [> Caqti_error.call_or_retrieve | `Unsupported] as 'e) result fiber
  (** [exec_with_affected_count req x] performs [req] with parameters [x],
      checks that no rows are returned, and returns the number of affected rows.

      See also {!Caqti_response_sig.S.exec} and
      {!Caqti_response_sig.S.affected_count}. *)

  val find :
    ('a, 'b, [< `One]) Caqti_request.t -> 'a ->
    ('b, [> Caqti_error.call_or_retrieve] as 'e) result fiber
  (** [find req x] performs [req] with parameters [x], checks that a single row
      is retured, and returns it.

      See also {!Caqti_response_sig.S.find}. *)

  val find_opt :
    ('a, 'b, [< `Zero | `One]) Caqti_request.t -> 'a ->
    ('b option, [> Caqti_error.call_or_retrieve] as 'e) result fiber
  (** [find_opt req x] performs [req] with parameters [x] and returns either
      [None] if no rows are returned or [Some y] if a single now [y] is returned
      and fails otherwise.

      See also {!Caqti_response_sig.S.find_opt}. *)

  val fold :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t ->
    ('b -> 'c -> 'c) ->
    'a -> 'c -> ('c, [> Caqti_error.call_or_retrieve] as 'e) result fiber
  (** [fold req f x acc] performs [req] with parameters [x] and passes [acc]
      through the composition of [f y] across the result rows [y] in the order
      of retrieval.

      See also {!Caqti_response_sig.S.fold}. *)

  val fold_s :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t ->
    ('b -> 'c -> ('c, 'e) result fiber) ->
    'a -> 'c -> ('c, [> Caqti_error.call_or_retrieve] as 'e) result fiber
  (** [fold_s f x acc] performs [req] with parameters [x] and passes [acc]
      through the monadic composition of [f y] across the returned rows [y] in
      the order of retrieval.

      Please be aware of possible deadlocks when using resources from the
      callback.  In particular, if the same connection pool is invoked as the
      one used to obtain the current connection, it will deadlock if the pool
      has just run out of connections.  An alternative is to collect the rows
      first e.g. with {!fold} and do the nested queries after exiting.

      See also {!Caqti_response_sig.S.fold_s}. *)

  val iter_s :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t ->
    ('b -> (unit, 'e) result fiber) ->
    'a -> (unit, [> Caqti_error.call_or_retrieve] as 'e) result fiber
  (** [iter_s f x] performs [req] with parameters [x] and sequences calls to [f
      y] for each result row [y] in the order of retrieval.

      Please see the warning in {!fold_s} about resource usage in the callback.

      See also {!Caqti_response_sig.S.iter_s}. *)

  val collect_list :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t -> 'a ->
    ('b list, [> Caqti_error.call_or_retrieve] as 'e) result fiber
  (** [collect_list request x] performs a [req] with parameters [x] and returns
      a list of rows in order of retrieval.  The accumulation is tail recursive
      but slightly less efficient than {!rev_collect_list}. *)

  val rev_collect_list :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t -> 'a ->
    ('b list, [> Caqti_error.call_or_retrieve] as 'e) result fiber
  (** [rev_collect_list request x] performs [request] with parameters [x] and
      returns a list of rows in the reverse order of retrieval.  The
      accumulation is tail recursive and slighly more efficient than
      {!collect_list}. *)


  (** {2 Transactions} *)

  val with_transaction :
    (unit -> ('a, 'e) result fiber) ->
    ('a, [> Caqti_error.transact] as 'e) result fiber
  (** [with_transaction f] wraps [f] in a transaction which is committed iff [f]
      returns [Ok _]. *)
end

module type Populate = sig
  type +'a fiber
  type (+'a, +'err) stream

  (** {2 Insertion} *)

  val populate :
    table: string ->
    columns: string list ->
    'a Caqti_type.t -> ('a, 'err) stream ->
    (unit, [> Caqti_error.call_or_retrieve | `Congested of 'err]) result fiber
  (** [populate table columns row_type seq] inputs the contents of [seq] into
      the database in whatever manner is most efficient as decided by the
      driver. *)
end

(** Full connection signature available to users. *)
module type S = sig

  val driver_info : Caqti_driver_info.t
  (** Information about the driver providing this connection module. *)

  val driver_connection : driver_connection option
  (** The underlying connection object of the driver if available.  The open
      variant constructor is defined in the driver library.  This is currently
      only implemented for caqti-driver-sqlite3 for the purpose of defining
      custom functions. *)

  include Base
  include Convenience with type 'a fiber := 'a fiber
  include Populate
     with type 'a fiber := 'a fiber
      and type ('a, 'err) stream := ('a, 'err) stream
end
