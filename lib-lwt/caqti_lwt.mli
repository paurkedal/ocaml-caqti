(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

module V1 : Caqti_sigs.CAQTI with type 'a io := 'a Lwt.t
module V2 : Caqti_connect_sig.S with type 'a io := 'a Lwt.t

(**/**)

[@@@ocaml.warning "-3"]
module Pool : Caqti_pool_sig.V1 with type 'a io := 'a Lwt.t
  [@@ocaml.deprecated "Moved to V1."]
module type CONNECTION = Caqti_sigs.CONNECTION with type 'a io = 'a Lwt.t
  [@@ocaml.deprecated "Moved to V1."]
val connect : Uri.t -> (module CONNECTION) Lwt.t
  [@@ocaml.deprecated "Moved to V1."]
val connect_pool : ?max_size: int -> Uri.t -> (module CONNECTION) Pool.t
  [@@ocaml.deprecated "Moved to V1."]
