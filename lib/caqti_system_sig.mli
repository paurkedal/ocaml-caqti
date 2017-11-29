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

(** (internal) System Interface

    The system utilities used by drivers.  This signature will likely be
    extended or changed due requirements of new backends. *)

open Caqti_query

module type S = sig

  type +'a io
  val (>>=) : 'a io -> ('a -> 'b io) -> 'b io
  val (>|=) : 'a io -> ('a -> 'b) -> 'b io
  val return : 'a -> 'a io
  val join : unit io list -> unit io

  module Mvar : sig
    type 'a t
    val create : unit -> 'a t
    val store : 'a -> 'a t -> unit
    val fetch : 'a t -> 'a io
  end

  module Unix : sig
    type file_descr
    val wrap_fd : (file_descr -> 'a io) -> Unix.file_descr -> 'a io
    val poll : ?read: bool -> ?write: bool -> ?timeout: float ->
               file_descr -> (bool * bool * bool) io
  end

  module Log : sig
    val error_f : ('a, unit, string, unit io) format4 -> 'a
    val warning_f : ('a, unit, string, unit io) format4 -> 'a
    val info_f : ('a, unit, string, unit io) format4 -> 'a
    val debug_f : ('a, unit, string, unit io) format4 -> 'a
    val debug_query_enabled : unit -> bool
    val debug_query : query_info -> string list -> unit io
    val debug_tuple_enabled : unit -> bool
    val debug_tuple : string list -> unit io
  end

  module Preemptive : sig
    val detach : ('a -> 'b) -> 'a -> 'b io
    val run_in_main : (unit -> 'a io) -> 'a
  end

end
