(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Connecting with Lwt.

    This module contains the signature and connect function specialized for use
    with Lwt. *)

include Caqti_connect_sig.S with type 'a future := 'a Lwt.t
                            and type 'a stream := 'a Lwt_stream.t

val or_fail : ('a, [< Caqti_error.t]) result -> 'a Lwt.t
(** Converts an error to an Lwt future failed with a {!Caqti_error.Exn}
    exception holding the error. *)
