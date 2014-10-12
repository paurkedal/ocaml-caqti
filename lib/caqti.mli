(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

(** Exceptions and connect functor. *)

open Caqti_metadata
open Caqti_query
open Caqti_sigs

exception Connect_failed of Uri.t * string
(** The exception raised when a backend fails to connect to a resource.  Due
    to pooled connections, this may be raised by describe and query functions
    as well as the connect function. *)

exception Prepare_failed of Uri.t * query_info * string
(** The exception raised when query preparation fails. *)

exception Execute_failed of Uri.t * query_info * string
(** The exception raised when query execution failes. *)

exception Miscommunication of Uri.t * query_info * string
(** This exception may be raised by a backend when something unexpected
    happen during the communication process.  It signifies a programming error
    or at least an incompatibility somewhere:
    - A query returns a different number of tuples than expected.
    - A query returns tuples of a different type then expected.
    - A query returns a different kind of result than expected.
    - A networked backend does not understand the result from the server,
      e.g. due to a different protocol version. *)

val register_scheme : string -> (module CAQTUS_FUNCTOR) -> unit
(** [register_scheme scheme m] installs [m] as a handler for the URI scheme
    [scheme].  This call must be done by a backend installed with findlib name
    caqtus-{i scheme} as part of its initialization. *)

module Make (System : SYSTEM) : CAQTI with module System = System
