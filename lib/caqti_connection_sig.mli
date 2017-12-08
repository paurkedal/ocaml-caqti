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

(** Signature of connection handles. *)

module type S = sig
  type 'a io


  (** {2 Query} *)

  module Response : Caqti_response_sig.S with type 'a io := 'a io

  val call :
    f: (('b, 'm) Response.t -> ('c, 'e) result io) ->
    ('a, 'b, 'm) Caqti_request.t -> 'a ->
    ('c, [> Caqti_error.call] as 'e) result io
  (** [call ~f request params] performs [request] with parameters [params]
      invoking [f] to process the result. *)


  (** {2 Retrieval Convenience}

      These are shortcuts for {!call} combined with retrieval functions from
      {!Caqti_response_sig.S}. *)

  val exec :
    ('a, unit, [< `Zero]) Caqti_request.t -> 'a ->
    (unit, [> Caqti_error.call_or_retrieve] as 'e) result io

  val find :
    ('a, 'b, [< `One]) Caqti_request.t -> 'a ->
    ('b, [> Caqti_error.call_or_retrieve] as 'e) result io

  val find_opt :
    ('a, 'b, [< `Zero | `One]) Caqti_request.t -> 'a ->
    ('b option, [> Caqti_error.call_or_retrieve] as 'e) result io

  val fold :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t ->
    ('b -> 'c -> 'c) ->
    'a -> 'c -> ('c, [> Caqti_error.call_or_retrieve] as 'e) result io

  val fold_s :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t ->
    ('b -> 'c -> ('c, 'e) result io) ->
    'a -> 'c -> ('c, [> Caqti_error.call_or_retrieve] as 'e) result io

  val iter_s :
    ('a, 'b, [< `Zero | `One | `Many]) Caqti_request.t ->
    ('b -> (unit, 'e) result io) ->
    'a -> (unit, [> Caqti_error.call_or_retrieve] as 'e) result io


  (** {2 Transactions} *)

  val start : unit -> (unit, [> Caqti_error.transact]) result io
  (** Starts a transaction if supported by the underlying database, otherwise
      does nothing. *)

  val commit : unit -> (unit, [> Caqti_error.transact]) result io
  (** Commits the current transaction if supported by the underlying database,
      otherwise does nothing. *)

  val rollback : unit -> (unit, [> Caqti_error.transact]) result io
  (** Rolls back a transaction if supported by the underlying database,
      otherwise does nothing. *)


  (** {2 Disconnection and Reuse} *)

  val disconnect : unit -> (unit, [> Caqti_error.disconnect]) result io
  (** Calling [disconnect ()] closes the connection to the database and frees
      up related resources. *)

  val validate : unit -> bool io
  (** For internal use by {!Caqti_pool}.  Tries to ensure the validity of the
      connection and must return [false] if unsuccessful. *)

  val check : (bool -> unit) -> unit
  (** For internal use by {!Caqti_pool}.  Called after a connection has been
      used.  [check f] must call [f ()] exactly once with an argument
      indicating whether to keep the connection in the pool or discard it. *)

end
