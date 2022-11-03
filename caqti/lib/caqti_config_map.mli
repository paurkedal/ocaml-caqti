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

(** Per-Connection Configuration *)

open Sexplib0

module Conv : sig
  type 'a t

  val create_with_args :
    of_sexps: (Sexp.t list -> 'a) ->
    sexps_of: ('a -> Sexp.t list) ->
    string -> 'a t

  val create :
    of_sexp: (Sexp.t -> 'a) ->
    sexp_of: ('a -> Sexp.t) ->
    string -> 'a t

  val create_atomic :
    of_string: (string -> 'a) ->
    to_string: ('a -> string) ->
    string -> 'a t

  val bool : bool t
  val int : int t
  val float : float t
  val string : string t
  val enum : string -> (string * 'a) list -> 'a t
  val minor_version : (int * int) t
end

module Key : sig
  type (+'a, 'b) t
  type +'a any = Any : ('a, 'b) t -> 'a any

  val name : ('a, 'b) t -> string
  val conv : ('a, 'b) t -> 'b Conv.t
  val create : 'b Conv.t -> string -> ('a, 'b) t
end

module Key_set : sig
  type +'a t

  val empty : 'a t
  val add : 'a Key.any -> 'a t -> 'a t
  val find : string -> 'a t -> 'a Key.any option
  val fold : ('a Key.any -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

type 'a t

type any = Any : 'a t -> any

type any_specific =
  | Any_specific : [`Generic | `Specific of 'a] t -> any_specific

module Driver : sig
  type 'a id = ..

  val register :
    name: string ->
    keys: [> `Specific of 'a] Key_set.t ->
    add_uri:
      (Uri.t ->
       [`Generic | `Specific of 'a] t ->
       ([`Generic | `Specific of 'a] t, [< Caqti_error.preconnect]) result) ->
    'a id -> unit
end

val empty : [`Generic] t

val add : ('a, 'b) Key.t -> 'b -> 'a t -> 'a t

val add_default : ('a, 'b) Key.t -> 'b -> 'a t -> 'a t

val add_driver : 'a Driver.id -> [`Generic] t -> [`Generic | `Specific of 'a] t

val add_uri :
  Uri.t -> [`Generic] t ->
  (any_specific, [> Caqti_error.load | Caqti_error.preconnect]) result

val as_default : [`Generic | `Specific of 'a] t -> [`Generic] t

val remove : ('a, 'b) Key.t -> 'a t -> 'a t

val update : ('a, 'b) Key.t -> ('b option -> 'b option) -> 'a t -> 'a t

val find : ('a, 'b) Key.t -> 'a t -> 'b option

val find_driver : [`Generic | `Specific of 'a] t -> 'a Driver.id

type +'a binding = B : ('a, 'b) Key.t * 'b -> 'a binding

val fold : ('a binding -> 'c -> 'c) -> 'a t -> 'c -> 'c

val right_union : 'a t -> 'b t -> 'b t

val any_of_sexp : Sexp.t -> any

val sexp_of_t : 'a t -> Sexp.t
