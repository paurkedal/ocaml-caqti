(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
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

module type S = sig
  include Caqti_private.System_sig.S

  module Sequencer : sig
    type 'a t
    val create : 'a -> 'a t
    val enqueue : 'a t -> ('a -> 'b future) -> 'b future
  end

  module Networking : sig

    type in_channel
    type out_channel

    type sockaddr = Unix of string | Inet of string * int

    val open_connection : sockaddr -> (in_channel * out_channel) future

    (* TODO: STARTTLS *)

    val output_char : out_channel -> char -> unit future
    val output_string : out_channel -> string -> unit future
    val flush : out_channel -> unit future
    val input_char : in_channel -> char future
    val really_input : in_channel -> Bytes.t -> int -> int -> unit future
    val close_in : in_channel -> unit future
  end
end
