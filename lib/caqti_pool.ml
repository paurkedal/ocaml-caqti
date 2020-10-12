(* Copyright (C) 2014--2020  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_compat [@@warning "-33"]

let default_max_size =
  try int_of_string (Sys.getenv "CAQTI_POOL_MAX_SIZE") with Not_found -> 8

module Make (System : Caqti_driver_sig.System_common) = struct
  open System

  let (>>=?) m mf = m >>= (function Ok x -> mf x | Error e -> return (Error e))

  module Task = struct
    type t = {priority: float; mvar: unit Mvar.t}
    let wake {mvar; _} = Mvar.store () mvar
    let compare {priority = pA; _} {priority = pB; _} = Float.compare pB pA
  end

  module Taskq = Caqti_heap.Make (Task)

  type ('a, +'e) t = {
    create: unit -> ('a, 'e) result future;
    free: 'a -> unit future;
    check: 'a -> (bool -> unit) -> unit;
    validate: 'a -> bool future;
    max_size: int;
    mutable cur_size: int;
    pool: 'a Queue.t;
    mutable waiting: Taskq.t;
  }

  let create
        ?(max_size = default_max_size)
        ?(check = fun _ f -> f true)
        ?(validate = fun _ -> return true)
        create free =
    { create; free; check; validate; max_size;
      cur_size = 0; pool = Queue.create (); waiting = Taskq.empty }

  let size {cur_size; _} = cur_size

  let wait ~priority p =
    let mvar = Mvar.create () in
    p.waiting <- Taskq.push Task.({priority; mvar}) p.waiting;
    Mvar.fetch mvar

  let rec acquire ~priority p =
    if Queue.is_empty p.pool then begin
      if p.cur_size < p.max_size then
        begin
          p.cur_size <- p.cur_size + 1;
          p.create () >|=
          (function
           | Ok e -> Ok e
           | Error err -> p.cur_size <- p.cur_size - 1; Error err)
        end
      else
        (wait ~priority p >>= fun () -> acquire ~priority p)
    end else begin
      let e = Queue.take p.pool in
      p.validate e >>= fun ok ->
      if ok then
        return (Ok e)
      else
        p.create () >|=
        (function
         | Ok e -> Ok e
         | Error err -> p.cur_size <- p.cur_size - 1; Error err)
    end

  let release p e =
    p.check e @@ fun ok ->
    if ok then Queue.add e p.pool
          else p.cur_size <- p.cur_size - 1;
    if not (Taskq.is_empty p.waiting) then
      begin
        let task, taskq = Taskq.pop_e p.waiting in
        p.waiting <- taskq;
        Task.wake task
      end

  let use ?(priority = 0.0) f p =
    acquire ~priority p >>=? fun e ->
    try
      f e >>=
      (function
       | Ok y -> release p e; return (Ok y)
       | Error err -> release p e; return (Error err))
    with exn ->
      release p e; raise exn

  let dispose p e = p.free e >|= fun () -> p.cur_size <- p.cur_size - 1

  let rec drain p =
    if p.cur_size = 0 then return () else
    (if Queue.is_empty p.pool
     then wait ~priority:0.0 p
     else dispose p (Queue.take p.pool)) >>= fun () ->
    drain p

end
