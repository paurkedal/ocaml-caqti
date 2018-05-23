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

(** {b Internal:} System Interface

    The system utilities used by drivers.  This signature will likely be
    extended or changed due requirements of new backends and architectures. *)

module type S = sig

  type +'a future

  val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
  val (>|=) : 'a future -> ('a -> 'b) -> 'b future
  val return : 'a -> 'a future
  val join : unit future list -> unit future

  module Mvar : sig
    type 'a t
    val create : unit -> 'a t
    val store : 'a -> 'a t -> unit
    val fetch : 'a t -> 'a future
  end

  module Unix : sig
    type file_descr
    val wrap_fd : (file_descr -> 'a future) -> Unix.file_descr -> 'a future
    val poll :
      ?read: bool -> ?write: bool -> ?timeout: float ->
      file_descr -> (bool * bool * bool) future
  end

  module Log : sig
    type 'a log = ('a, unit future) Logs.msgf -> unit future
    val err : ?src: Logs.src -> 'a log
    val warn : ?src: Logs.src -> 'a log
    val info : ?src: Logs.src -> 'a log
    val debug : ?src: Logs.src -> 'a log
  end

  module Preemptive : sig
    val detach : ('a -> 'b) -> 'a -> 'b future
    val run_in_main : (unit -> 'a future) -> 'a
  end

end
