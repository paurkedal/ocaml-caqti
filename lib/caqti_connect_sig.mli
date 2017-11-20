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

(** Signature for establishing database connections. *)

module type S = sig
  type 'a io

  module Pool : Caqti_pool_sig.V2 with type 'a io := 'a io

  module type CONNECTION = Caqti_connection_sig.S with type 'a io := 'a io
  type connection = (module CONNECTION)

  val connect : Uri.t ->
    (connection, [> Caqti_error.load_or_connect]) result io

  val connect_pool : ?max_size: int -> Uri.t ->
    ((connection, [> Caqti_error.connect]) Pool.t, [> Caqti_error.load]) result
end
