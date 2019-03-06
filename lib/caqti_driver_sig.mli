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

(** {b Internal:} Signature for driver implementation.

    This interface is unstable and may change between minor versions.  If you
    are developing an external driver, please open an issue to sort out
    requirements and to announce you need for a stable driver API. *)

module type System_common = sig

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

  module Log : sig
    type 'a log = ('a, unit future) Logs.msgf -> unit future
    val err : ?src: Logs.src -> 'a log
    val warn : ?src: Logs.src -> 'a log
    val info : ?src: Logs.src -> 'a log
    val debug : ?src: Logs.src -> 'a log
  end
end

module type System_unix = sig
  include System_common

  module Unix : sig
    type file_descr
    val wrap_fd : (file_descr -> 'a future) -> Unix.file_descr -> 'a future
    val poll :
      ?read: bool -> ?write: bool -> ?timeout: float ->
      file_descr -> (bool * bool * bool) future
  end

  module Preemptive : sig
    val detach : ('a -> 'b) -> 'a -> 'b future
    val run_in_main : (unit -> 'a future) -> 'a
  end

end

module type S = sig
  type +'a future

  module type CONNECTION =
    Caqti_connection_sig.Base with type 'a future := 'a future

  val driver_info : Caqti_driver_info.t

  val connect :
    Uri.t -> ((module CONNECTION), [> Caqti_error.connect]) result future
end

module type Of_system_unix =
  functor (System : System_unix) ->
  S with type 'a future := 'a System.future
