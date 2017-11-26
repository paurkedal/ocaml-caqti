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

let default_max_size =
  try int_of_string (Sys.getenv "CAQTI_POOL_MAX_SIZE") with Not_found -> 8

module Make_v1 (System : Caqti_system_sig.V1) = struct
  open System

  module Task = struct
    type t = {priority : float; mvar : unit Mvar.t}
    let wake {mvar; _} = Mvar.store () mvar
    let compare {priority = pA; _} {priority = pB; _} = Pervasives.compare pB pA
  end

  module Taskq = Caqti_heap.Make (Task)

  type 'a t = {
    p_create : unit -> 'a io;
    p_free : 'a -> unit io;
    p_check : 'a -> (bool -> unit) -> unit;
    p_validate : 'a -> bool io;
    p_max_size : int;
    mutable p_cur_size : int;
    p_pool : 'a Queue.t;
    mutable p_waiting : Taskq.t;
  }

  let create
        ?(max_size = default_max_size)
        ?(check = fun _ f -> f true)
        ?(validate = fun _ -> return true)
        create free =
    { p_create = create; p_free = free;
      p_check = check; p_validate = validate;
      p_max_size = max_size;
      p_cur_size = 0; p_pool = Queue.create (); p_waiting = Taskq.empty }

  let size {p_cur_size; _} = p_cur_size

  let wait ~priority p =
    let mvar = Mvar.create () in
    p.p_waiting <- Taskq.push Task.({priority; mvar}) p.p_waiting;
    Mvar.fetch mvar

  let rec acquire ~priority p =
    if Queue.is_empty p.p_pool then begin
      if p.p_cur_size < p.p_max_size
      then (p.p_create () >>= fun e ->
            p.p_cur_size <- p.p_cur_size + 1; return e)
      else (wait ~priority p >>= fun () -> acquire ~priority p)
    end else begin
      let e = Queue.take p.p_pool in
      catch (fun () -> p.p_validate e)
            (fun xc -> Queue.add e p.p_pool; fail xc) >>= fun ok ->
      if ok then return e else
      catch p.p_create (fun xc -> p.p_cur_size <- p.p_cur_size - 1; fail xc)
    end

  let release p e =
    p.p_check e @@ fun ok ->
    if ok then Queue.add e p.p_pool
          else p.p_cur_size <- p.p_cur_size - 1;
    if not (Taskq.is_empty p.p_waiting) then
      let task, taskq = Taskq.pop_e p.p_waiting in
      p.p_waiting <- taskq;
      Task.wake task

  let use ?(priority = 0.0) f p =
    acquire ~priority p >>= fun e ->
    catch (fun () -> f e >>= fun r -> release p e; return r)
          (fun xc -> release p e; fail xc)

  let dispose p e =
    p.p_free e >>= fun () -> p.p_cur_size <- p.p_cur_size - 1; return ()

  let rec drain p =
    if p.p_cur_size = 0 then return () else
    ( if Queue.is_empty p.p_pool
      then wait ~priority:0.0 p
      else dispose p (Queue.take p.p_pool) ) >>= fun () ->
    drain p

end
module Make = Make_v1

module Make_v2 (System : Caqti_system_sig.V2) = struct
  open System

  let (>>=?) m mf = m >>= (function Ok x -> mf x | Error e -> return (Error e))

  module Task = struct
    type t = {priority : float; mvar : unit Mvar.t}
    let wake {mvar; _} = Mvar.store () mvar
    let compare {priority = pA; _} {priority = pB; _} = Pervasives.compare pB pA
  end

  module Taskq = Caqti_heap.Make (Task)

  type ('a, +'e) t = {
    p_create : unit -> ('a, 'e) result io;
    p_free : 'a -> bool io;
    p_check : 'a -> (bool -> unit) -> unit;
    p_validate : 'a -> bool io;
    p_max_size : int;
    mutable p_cur_size : int;
    p_pool : 'a Queue.t;
    mutable p_waiting : Taskq.t;
  }

  let create
        ?(max_size = default_max_size)
        ?(check = fun _ f -> f true)
        ?(validate = fun _ -> return true)
        create free =
    { p_create = create; p_free = free;
      p_check = check; p_validate = validate;
      p_max_size = max_size;
      p_cur_size = 0; p_pool = Queue.create (); p_waiting = Taskq.empty }

  let size {p_cur_size; _} = p_cur_size

  let wait ~priority p =
    let mvar = Mvar.create () in
    p.p_waiting <- Taskq.push Task.({priority; mvar}) p.p_waiting;
    Mvar.fetch mvar

  let rec acquire ~priority p =
    if Queue.is_empty p.p_pool then begin
      if p.p_cur_size < p.p_max_size
      then (p.p_create () >>= fun e ->
            p.p_cur_size <- p.p_cur_size + 1; return e)
      else (wait ~priority p >>= fun () -> acquire ~priority p)
    end else begin
      let e = Queue.take p.p_pool in
      p.p_validate e >>= fun ok ->
      if ok then
        return (Ok e)
      else
        p.p_create () >|=
        (function
         | Ok e -> Ok e
         | Error err -> p.p_cur_size <- p.p_cur_size - 1; Error err)
    end

  let release p e =
    p.p_check e @@ fun ok ->
    if ok then Queue.add e p.p_pool
          else p.p_cur_size <- p.p_cur_size - 1;
    if not (Taskq.is_empty p.p_waiting) then
      let task, taskq = Taskq.pop_e p.p_waiting in
      p.p_waiting <- taskq;
      Task.wake task

  let use ?(priority = 0.0) f p =
    acquire ~priority p >>=? fun e ->
    f e >>=
    (function
     | Ok y -> release p e; return (Ok y)
     | Error err -> release p e; return (Error err))

(* TODO: Revise draining. *)
(*
  let dispose p e =
    p.p_free e >>= fun () -> p.p_cur_size <- p.p_cur_size - 1; return ()

  let rec drain p =
    if p.p_cur_size = 0 then return () else
    ( if Queue.is_empty p.p_pool
      then wait ~priority:0.0 p
      else dispose p (Queue.take p.p_pool) ) >>= fun () ->
    drain p
*)

end
