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

(** (v2) Connector for Async. *)

open Async

module V2 : Caqti_connect_sig.S with type 'a io := 'a Deferred.t
(** You have to use this submodule in this release due to backwards
    compatibility for v1 API. *)

(**/**)

[@@@ocaml.warning "-3"]
module Pool : Caqti1_pool_sig.S with type 'a io := 'a Deferred.Or_error.t
[@@ocaml.deprecated "Moved to Caqti1_async."]
module type CONNECTION = Caqti_sigs.CONNECTION
  with type 'a io = 'a Deferred.Or_error.t
[@@ocaml.deprecated "Moved to Caqti1_async."]
val connect : Uri.t -> (module CONNECTION) Deferred.Or_error.t
[@@ocaml.deprecated "Moved to Caqti1_async."]
val connect_pool : ?max_size: int -> Uri.t -> (module CONNECTION) Pool.t
[@@ocaml.deprecated "Moved to Caqti1_async."]
