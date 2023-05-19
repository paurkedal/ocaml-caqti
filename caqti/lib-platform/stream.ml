(* Copyright (C) 2018--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_stream_sig

module type FIBER = sig
  type +'a t

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  end

  val return : 'a -> 'a t
end

module Make(Fiber : FIBER) : S with type 'a fiber := 'a Fiber.t = struct
  open Fiber.Infix

  let (>>=?) res_future f =
    res_future >>= function
    | Ok a -> f a
    | Error _ as r -> Fiber.return r

  let (>|=?) res_future f =
    res_future >>= function
    | Ok a -> Fiber.return @@ Ok (f a)
    | Error _ as r -> Fiber.return r

  type ('a, 'err) t = unit -> ('a, 'err) node Fiber.t
  and ('a, 'err) node =
    | Nil
    | Error of 'err
    | Cons of 'a * ('a, 'err) t

  let rec fold ~f t state =
    t () >>= function
    | Nil -> Fiber.return (Ok state)
    | Error err -> Fiber.return (Error err : ('a, 'err) result)
    | Cons (a, t') -> fold ~f t' (f a state)

  let rec fold_s ~f t state =
    t () >>= function
    | Nil -> Fiber.return (Ok state)
    | Error err -> Fiber.return (Error (`Congested err) : ('a, 'err) result)
    | Cons (a, t') -> f a state >>=? fold_s ~f t'

  let rec iter_s ~f t =
    t () >>= function
    | Nil -> Fiber.return (Ok ())
    | Error err -> Fiber.return (Error (`Congested err) : ('a, 'err) result)
    | Cons (a, t') -> f a >>=? fun () -> iter_s ~f t'

  let to_rev_list t = fold ~f:List.cons t []

  let to_list t = to_rev_list t >|=? List.rev

  let rec of_list l =
    fun () -> match l with
    | [] -> Fiber.return Nil
    | hd::tl -> Fiber.return (Cons (hd, (of_list tl)))

  let rec map_result ~f xs () =
    xs () >|= function
     | Cons (x, xs') ->
        (match f x with
         | Result.Ok y -> Cons (y, map_result ~f xs')
         | Result.Error e -> Error e)
     | Nil | Error _ as r -> r
end
