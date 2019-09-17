(* Copyright (C) 2018--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

module type S = sig
  type +'a future

  type ('a, 'err) t = unit -> ('a, 'err) node future
  and ('a, 'err) node =
    | Nil
    | Error of 'err
    | Cons of 'a * ('a, 'err) t

  val fold :
    f: ('a -> 'state -> 'state) ->
    ('a, 'err) t ->
    'state ->
    ('state, 'err) result future

  val fold_s :
    f: ('a -> 'state -> ('state, 'err) result future) ->
    ('a, 'clog) t ->
    'state ->
    ('state, [> `Congested of 'clog ] as 'err) result future

  val iter_s :
    f: ('a -> (unit, 'err) result future) ->
    ('a, 'clog) t ->
    (unit, [> `Congested of 'clog ] as 'err) result future

  val to_rev_list : ('a, 'err) t -> ('a list, 'err) result future

  val to_list : ('a, 'err) t -> ('a list, 'err) result future

  val of_list : 'a list -> ('a, 'err) t
end

module type FUTURE = sig
  type +'a future

  val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
  val (>|=) : 'a future -> ('a -> 'b) -> 'b future
  val return : 'a -> 'a future
end

module Make(X : FUTURE) : S with type 'a future := 'a X.future = struct
  open X

  let (>>=?) res_future f =
    res_future >>= function
    | Ok a -> f a
    | Error _ as r -> return r

  let (>|=?) res_future f =
    res_future >>= function
    | Ok a -> return @@ Ok (f a)
    | Error _ as r -> return r

  type ('a, 'err) t = unit -> ('a, 'err) node future
  and ('a, 'err) node =
    | Nil
    | Error of 'err
    | Cons of 'a * ('a, 'err) t

  let rec fold ~f t state =
    t () >>= function
    | Nil -> return (Ok state)
    | Error err -> return (Error err : ('a, 'err) result)
    | Cons (a, t') -> fold ~f t' (f a state)

  let rec fold_s ~f t state =
    t () >>= function
    | Nil -> return (Ok state)
    | Error err -> return (Error (`Congested err) : ('a, 'err) result)
    | Cons (a, t') -> f a state >>=? fold_s ~f t'

  let rec iter_s ~f t =
    t () >>= function
    | Nil -> return (Ok ())
    | Error err -> return (Error (`Congested err) : ('a, 'err) result)
    | Cons (a, t') -> f a >>=? fun () -> iter_s ~f t'

  let to_rev_list t = fold ~f:List.cons t []

  let to_list t = to_rev_list t >|=? List.rev

  let rec of_list l =
    fun () -> match l with
    | [] -> return Nil
    | hd::tl -> return (Cons (hd, (of_list tl)))
end
