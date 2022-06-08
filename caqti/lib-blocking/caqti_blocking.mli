(* Copyright (C) 2018--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Blocking API based on the Unix module.

    This module implements a blocking API.  It is not designed for preemptive
    threading.  That is, connections and connection pools must be created and
    used within a single thread, and any limitation on multithreading from the
    driver or client library applies.

    You can use a connection pool to cache a single DB connection, additional
    connections will not be allocated, since usage is serial. *)

include Caqti_connect_sig.S with type 'a future := 'a

val or_fail : ('a, [< Caqti_error.t]) result -> 'a
(** Takes [Ok x] to [x] and raises {!Caqti_error.Exn}[ err] on [Error err]. *)
