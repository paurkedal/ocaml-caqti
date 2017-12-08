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

(** Signature of a response from a database. *)

module type S = sig
  type 'b io

  type ('b, +'m) t
  (** The type describing the response and containing returned data from a
      request execution.
      - ['b] is the type of a single row
      - ['m] is the possible multiplicities of rows *)

  (** {2 Result inspection} *)

  val returned_count :
    ('b, 'm) t -> (int, [> Caqti_error.retrieve | `Unsupported]) result io
  (** [returned_count resp] is the number of rows returned by [resp].  This
      function may not be available for all databases. *)

  val affected_count :
    ('b, 'm) t -> (int, [> Caqti_error.retrieve | `Unsupported]) result io
  (** [affected_count resp] is the number of rows affected by the updated the
      produced [resp].  This function may not be available for all databases. *)

  (** {2 Result retrieval} *)

  val exec :
    (unit, [< `Zero]) t -> (unit, [> Caqti_error.retrieve]) result io
  (** [exec resp] checks that [resp] succeeded with no result rows. *)

  val find :
    ('b, [< `One]) t -> ('b, [> Caqti_error.retrieve]) result io
  (** [find resp] checks that [resp] succeeded with a single row, and returns
      the decoded row. *)

  val find_opt :
    ('b, [< `Zero | `One]) t -> ('b option, [> Caqti_error.retrieve]) result io
  (** [find_opt resp] checks that [resp] succeeded with at most one row, and
      returns the row if any. *)

  val fold :
    ('b -> 'c -> 'c) ->
    ('b, 'm) t -> 'c -> ('c, [> Caqti_error.retrieve]) result io
  (** [fold f resp] folds [f] over the decoded rows returned in [resp]. *)

  val fold_s :
    ('b -> 'c -> ('c, 'e) result io) ->
    ('b, 'm) t -> 'c -> ('c, [> Caqti_error.retrieve] as 'e) result io
  (** [fold_s f resp] folds [f] over the decoded rows returned by [resp] within
      the IO and result monad. *)

  val iter_s :
    ('b -> (unit, 'e) result io) ->
    ('b, 'm) t -> (unit, [> Caqti_error.retrieve] as 'e) result io
  (** [iter_s f resp] iterates [f] over the decoded rows returned by [resp]
      within the IO and result monad. *)
end
