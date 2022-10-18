(* Copyright (C) 2014--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

let default_max_size =
  try int_of_string (Sys.getenv "CAQTI_POOL_MAX_SIZE") with Not_found -> 8

let default_log_src = Logs.Src.create "Caqti_pool"

module Option = struct
  let for_all f = function None -> true | Some x -> f x
end

module Make (System : System_sig.S) = struct
  open System

  let (>>=?) m mf = m >>= (function Ok x -> mf x | Error e -> return (Error e))

  module Task = struct
    type t = {priority: float; mvar: unit Mvar.t}
    let wake {mvar; _} = Mvar.store () mvar
    let compare {priority = pA; _} {priority = pB; _} = Float.compare pB pA
  end

  module Taskq = Heap.Make (Task)

  type 'a entry = {
    resource: 'a;
    mutable used_count: int;
  }

  type ('a, +'e) t = {
    create: unit -> ('a, 'e) result future;
    free: 'a -> unit future;
    check: 'a -> (bool -> unit) -> unit;
    validate: 'a -> bool future;
    log_src: Logs.Src.t;
    max_idle_size: int;
    max_size: int;
    max_use_count: int option;
    mutable cur_size: int;
    pool: 'a entry Queue.t;
    mutable waiting: Taskq.t;
  }

  let create
        ?(max_size = default_max_size)
        ?(max_idle_size = max_size)
        ?(max_use_count = None)
        ?(check = fun _ f -> f true)
        ?(validate = fun _ -> return true)
        ?(log_src = default_log_src)
        create free =
    assert (max_size > 0);
    assert (max_size >= max_idle_size);
    assert (Option.for_all (fun n -> n > 0) max_use_count);
    { create; free; check; validate; log_src;
      max_idle_size; max_size; max_use_count;
      cur_size = 0; pool = Queue.create (); waiting = Taskq.empty }

  let size {cur_size; _} = cur_size

  let wait ~priority p =
    let mvar = Mvar.create () in
    p.waiting <- Taskq.push Task.({priority; mvar}) p.waiting;
    Mvar.fetch mvar

  let schedule p =
    if not (Taskq.is_empty p.waiting) then begin
      let task, taskq = Taskq.pop_e p.waiting in
      p.waiting <- taskq;
      Task.wake task
    end

  let realloc p =
    p.create () >|=
    (function
     | Ok e -> Ok {resource = e; used_count = 0}
     | Error err ->
        p.cur_size <- p.cur_size - 1;
        schedule p;
        Error err)

  let rec acquire ~priority p =
    if Queue.is_empty p.pool then begin
      if p.cur_size < p.max_size then
        begin
          p.cur_size <- p.cur_size + 1;
          realloc p
        end
      else
        wait ~priority p >>= fun () ->
        acquire ~priority p
    end else begin
      let e = Queue.take p.pool in
      p.validate e.resource >>= fun ok ->
      if ok then
        return (Ok e)
      else begin
        Log.warn ~src:p.log_src (fun f ->
          f "Dropped pooled connection due to invalidation.") >>= fun () ->
        realloc p
      end
    end

  let can_reuse p e =
       p.cur_size <= p.max_idle_size
    && Option.for_all (fun n -> e.used_count < n) p.max_use_count

  let release p e =
    if not (can_reuse p e) then begin
      p.cur_size <- p.cur_size - 1;
      p.free e.resource >|= fun () ->
      schedule p
    end else begin
      p.check e.resource begin fun ok ->
        if ok then
          Queue.add e p.pool
        else begin
          Logs.warn ~src:p.log_src (fun f ->
            f "Will not repool connection due to invalidation.");
          p.cur_size <- p.cur_size - 1
        end;
        schedule p
      end;
      return ()
    end

  let use ?(priority = 0.0) f p =
    acquire ~priority p >>=? fun e ->
    finally
      (fun () -> f e.resource)
      (fun () -> e.used_count <- e.used_count + 1; release p e)

  let dispose p e = p.free e >|= fun () -> p.cur_size <- p.cur_size - 1

  let rec drain p =
    if p.cur_size = 0 then return () else
    (if Queue.is_empty p.pool
     then wait ~priority:0.0 p
     else dispose p (Queue.take p.pool).resource) >>= fun () ->
    drain p

end
