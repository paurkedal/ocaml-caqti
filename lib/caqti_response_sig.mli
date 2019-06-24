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

(** Signature of a response from a database. *)

module type S = sig
  type +'b future

  type (+'a, +'err) stream

  type ('b, +'m) t
  (** The type describing the response and containing returned data from a
      request execution.
      - ['b] is the type of a single row
      - ['m] is the possible multiplicities of rows *)

  (** {2 Result inspection} *)

  val returned_count :
    ('b, 'm) t -> (int, [> Caqti_error.retrieve | `Unsupported]) result future
  (** [returned_count resp] is the number of rows returned by [resp].  This
      function may not be available for all databases. *)

  val affected_count :
    ('b, 'm) t -> (int, [> Caqti_error.retrieve | `Unsupported]) result future
  (** [affected_count resp] is the number of rows affected by the updated the
      produced [resp].  This function may not be available for all databases. *)

  (** {2 Result retrieval} *)

  val exec :
    (unit, [< `Zero]) t -> (unit, [> Caqti_error.retrieve]) result future
  (** [exec resp] checks that [resp] succeeded with no result rows. *)

  val find :
    ('b, [< `One]) t -> ('b, [> Caqti_error.retrieve]) result future
  (** [find resp] checks that [resp] succeeded with a single row, and returns
      the decoded row. *)

  val find_opt :
    ('b, [< `Zero | `One]) t ->
    ('b option, [> Caqti_error.retrieve]) result future
  (** [find_opt resp] checks that [resp] succeeded with at most one row, and
      returns the row if any. *)

  val fold :
    ('b -> 'c -> 'c) ->
    ('b, 'm) t -> 'c -> ('c, [> Caqti_error.retrieve]) result future
  (** [fold f resp] folds [f] over the decoded rows returned in [resp]. *)

  val fold_s :
    ('b -> 'c -> ('c, 'e) result future) ->
    ('b, 'm) t -> 'c -> ('c, [> Caqti_error.retrieve] as 'e) result future
  (** [fold_s f resp] folds [f] over the decoded rows returned by [resp] within
      the IO and result monad.

      {b Note.} Do not make nested queries in the callback to this function.  If
      you use the same connection, it may lead to data corruption.  If you pull
      a different connection from the same pool, it may deadlock if the pool
      runs out of connections.  Also, some drivers may not support simpltaneous
      connections. *)

  val iter_s :
    ('b -> (unit, 'e) result future) ->
    ('b, 'm) t -> (unit, [> Caqti_error.retrieve] as 'e) result future
  (** [iter_s f resp] iterates [f] over the decoded rows returned by [resp]
      within the IO and result monad.

      {b Note.} Do not make nested queries in the callback to this function.
      Cf. {!fold_s}. *)

  val to_stream : ('b, 'm) t -> ('b, [> Caqti_error.retrieve] as 'err) stream
  (** [to_stream resp] returns a stream whose elements are the decoded rows
      returned by [resp]. *)

  (** {2 Insertion functions} *)

  val from_stream :
    (Caqti_request.counit, [`Many_in]) t ->
    ('row_type, unit) stream ->
    (unit, [> Caqti_error.driver] as 'err) result future
  (** [from_stream resp stream] inserts the contents of [stream] into the database. *)
end
