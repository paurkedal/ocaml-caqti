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

(** (v2) Request specification. *)

(** {2 Primitives} *)

type template =
  | L of string  (** Literal code. May contain incomplete fragments. *)
  | P of int     (** [P i] refers to parameter number [i], counting from 0. *)
  | S of template list (** [S frags] is the concatenation of [frags]. *)
(** A representation of a query string to send to a database, abstracting over
    parameter references and providing nested concatenation to simplify
    generation.  For databases which only support linear parameters (typically
    denoted "[?]"), the driver will reshuffle, elide, and duplicate parameters
    as needed.  *)

type ('a, 'b, +'m) t constraint 'm = [< `Zero | `One | `Many]

val create :
  ?oneshot: bool ->
  'a Caqti_type.t -> 'b Caqti_type.t -> 'm Caqti_mult.t ->
  (Caqti_driver_info.t -> template) -> ('a, 'b, 'm) t

val params_type : ('a, _, _) t -> 'a Caqti_type.t
val row_type : (_, 'b, _) t -> 'b Caqti_type.t
val row_mult : (_, _, 'm) t -> 'm Caqti_mult.t

val query_id : ('a, 'b, 'm) t -> int option
val query_template : ('a, 'b, 'm) t -> Caqti_driver_info.t -> template

(** {2 Convenience}

    In the following functions, queries are specified as plain strings using
    linear parameters in the style of Sqlite3 and MySQL.  That is, occurrences
    of "[?]" are bound to successive parameters independent of the parameter
    style used by the database.  For PostgreSQL "[$1]", "[$2]", ... are
    substituted for "[?]". *)

val create_p :
  ?oneshot: bool ->
  'a Caqti_type.t -> 'b Caqti_type.t -> 'm Caqti_mult.t ->
  (Caqti_driver_info.t -> string) -> ('a, 'b, 'm) t
(** [create_p arg_type row_type row_mult f] is a request which takes parameters
    of type [arg_type], returns rows of type [row_type] with multiplicity
    [row_mult], and which sends a query string based on a preliminary form given
    by [f di], where [di] is the {!Caqti_driver_info.t} for the target database.
    The preliminary query string may contain occurrences of "[?]", which are
    replaced by successive parameter references.  This is implemented as [create
    arg_type row_type (parse % f)] where [parse] parses a string into a
    template.

    @param oneshot
      For queries generated on-demand or which are otherwise executed only once,
      pass [true] do make the query non-prepared.  By default queries are
      prepared, which is suitable for requests defined at the module level. *)

val exec :
  ?oneshot: bool -> 'a Caqti_type.t -> 'b Caqti_type.t ->
  string -> ('a, 'b, [> `Zero]) t
(** [exec_p arg_type row_type s] is a shortcut for [create_p arg_type row_type
    Caqti_mult.zero (fun _ -> s)]. *)

val find :
  ?oneshot: bool -> 'a Caqti_type.t -> 'b Caqti_type.t ->
  string -> ('a, 'b, [> `One]) t
(** [find_p arg_type row_type s] is a shortcut for [create_p arg_type row_type
    Caqti_mult.one (fun _ -> s)]. *)

val find_opt :
  ?oneshot: bool -> 'a Caqti_type.t -> 'b Caqti_type.t ->
  string -> ('a, 'b, [> `Zero | `One]) t
(** [find_opt_p arg_type row_type s] is a shortcut for [create_p arg_type
    row_type Caqti_mult.zero_or_one (fun _ -> s)]. *)

val collect :
  ?oneshot: bool -> 'a Caqti_type.t -> 'b Caqti_type.t ->
  string -> ('a, 'b, [> `Zero | `One | `Many]) t
(** [collect_p arg_type row_type s] is a shortcut for [create_p arg_type
    row_type Caqti_mult.many (fun _ -> s)]. *)
