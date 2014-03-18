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

open Caqti_query

module type CONNECTION = sig

  type 'a io
  type param
  type tuple

  val drain : unit -> unit io

  val exec : query -> param array -> unit io
  val find : query -> (tuple -> 'a) -> param array -> 'a option io
  val fold : query -> (tuple -> 'a -> 'a) -> param array -> 'a -> 'a io
  val fold_s : query -> (tuple -> 'a -> 'a io) -> param array -> 'a -> 'a io
  val iter_p : query -> (tuple -> unit io) -> param array -> unit io
  val iter_s : query -> (tuple -> unit io) -> param array -> unit io

  module Param : sig
    val null : param
    val option : ('a -> param) -> 'a option -> param
    val bool : bool -> param
    val int : int -> param
    val int64 : int64 -> param
    val float : float -> param
    val string : string -> param
    val date : CalendarLib.Date.t -> param
    val utc : CalendarLib.Calendar.t -> param
  end

  module Tuple : sig
    val length : tuple -> int
    val raw : int -> tuple -> string
    val is_null : int -> tuple -> bool
    val option : (int -> tuple -> 'a) -> int -> tuple -> 'a option
    val bool : int -> tuple -> bool
    val int : int -> tuple -> int
    val int64 : int -> tuple -> int64
    val float : int -> tuple -> float
    val string : int -> tuple -> string
    val date : int -> tuple -> CalendarLib.Date.t
    val utc : int -> tuple -> CalendarLib.Calendar.t
  end

end

module type CONNECT = sig
  type 'a io
  module type CONNECTION = CONNECTION with type 'a io = 'a io
  val connect : ?max_pool_size: int -> Uri.t -> (module CONNECTION) io
end

module type SYSTEM = sig

  type 'a io
  val (>>=) : 'a io -> ('a -> 'b io) -> 'b io
  val return : 'a -> 'a io
  val fail : exn -> 'a io

  module Unix : sig
    type file_descr
    val of_unix_file_descr : Unix.file_descr -> file_descr
    val wait_read : file_descr -> unit io
  end

  module Pool : sig
    type 'a t
    val create : ?max_size: int -> ?max_priority: int ->
		 ?validate: ('a -> bool io) -> (unit -> 'a io) -> 'a t
    val use : ?priority: int -> ('a -> 'b io) -> 'a t -> 'b io
    val drain : 'a t -> unit io
  end

  module Log : sig
    val error_f : query -> ('a, unit, string, unit io) format4 -> 'a
    val warning_f : query -> ('a, unit, string, unit io) format4 -> 'a
    val info_f : query -> ('a, unit, string, unit io) format4 -> 'a
    val debug_f : query -> ('a, unit, string, unit io) format4 -> 'a
  end
end

module type CONNECT_FUNCTOR = sig
  module Make (System : SYSTEM) : CONNECT with type 'a io = 'a System.io
end
