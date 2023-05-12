(* Copyright (C) 2014--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

let default_log_src = Logs.Src.create "Caqti_platform.Pool"

module Option' = struct
  let for_all f = function None -> true | Some x -> f x
end

module type ALARM = sig
  type switch
  type connect_env

  type t
  val schedule : sw: switch -> connect_env: connect_env -> Mtime.t -> (unit -> unit) -> t
  val unschedule : t -> unit
end

module type S = sig
  type switch
  type connect_env

  include Caqti_pool_sig.S

  val create :
    ?max_size: int ->
    ?max_idle_size: int ->
    ?max_idle_age: Mtime.Span.t ->
    ?max_use_count: int option ->
    ?check: ('a -> (bool -> unit) -> unit) ->
    ?validate: ('a -> bool future) ->
    ?log_src: Logs.Src.t ->
    sw: switch ->
    connect_env: connect_env ->
    (unit -> ('a, 'e) result future) -> ('a -> unit future) ->
    ('a, 'e) t
end

module Make
  (System : System_sig.CORE)
  (Alarm : ALARM
    with type connect_env := System.connect_env
     and type switch := System.Switch.t) =
struct
  open System

  let (>>=?) m mf = m >>= (function Ok x -> mf x | Error e -> return (Error e))

  module Task = struct
    type t = {priority: float; semaphore: Semaphore.t}
    let wake {semaphore; _} = Semaphore.release semaphore
    let compare {priority = pA; _} {priority = pB; _} = Float.compare pB pA
  end

  module Taskq = Heap.Make (Task)

  type 'a entry = {
    resource: 'a;
    mutable used_count: int;
    mutable used_latest: Mtime.t;
  }

  type ('a, +'e) t = {
    connect_env: connect_env;
    switch: Switch.t;
    create: unit -> ('a, 'e) result future;
    free: 'a -> unit future;
    check: 'a -> (bool -> unit) -> unit;
    validate: 'a -> bool future;
    log_src: Logs.Src.t;
    max_idle_size: int;
    max_idle_age: Mtime.Span.t option;
    max_size: int;
    max_use_count: int option;
    mutable cur_size: int;
    queue: 'a entry Queue.t;
    mutable waiting: Taskq.t;
    mutable alarm: Alarm.t option;
  }

  let create
        ?(max_size = default_max_size)
        ?(max_idle_size = max_size)
        ?max_idle_age
        ?(max_use_count = None)
        ?(check = fun _ f -> f true)
        ?(validate = fun _ -> return true)
        ?(log_src = default_log_src)
        ~sw
        ~connect_env
        create free =
    assert (max_size > 0);
    assert (max_size >= max_idle_size);
    assert (Option'.for_all (fun n -> n > 0) max_use_count);
    {
      connect_env; switch = sw;
      create; free; check; validate; log_src;
      max_idle_size; max_size; max_use_count; max_idle_age;
      cur_size = 0;
      queue = Queue.create ();
      waiting = Taskq.empty;
      alarm = None;
    }

  let size {cur_size; _} = cur_size

  let wait ~priority pool =
    let semaphore = Semaphore.create () in
    pool.waiting <- Taskq.push Task.({priority; semaphore}) pool.waiting;
    Semaphore.acquire semaphore

  let schedule pool =
    if not (Taskq.is_empty pool.waiting) then begin
      let task, taskq = Taskq.pop_e pool.waiting in
      pool.waiting <- taskq;
      Task.wake task
    end

  let realloc pool =
    pool.create () >|=
    (function
     | Ok resource ->
        Ok {resource; used_count = 0; used_latest = Mtime_clock.now ()}
     | Error err ->
        pool.cur_size <- pool.cur_size - 1;
        schedule pool;
        Error err)

  let rec acquire ~priority pool =
    if Queue.is_empty pool.queue then begin
      if pool.cur_size < pool.max_size then
        begin
          pool.cur_size <- pool.cur_size + 1;
          realloc pool
        end
      else
        wait ~priority pool >>= fun () ->
        acquire ~priority pool
    end else begin
      let entry = Queue.take pool.queue in
      pool.validate entry.resource >>= fun ok ->
      if ok then
        return (Ok entry)
      else begin
        Log.warn ~src:pool.log_src (fun f ->
          f "Dropped pooled connection due to invalidation.") >>= fun () ->
        realloc pool
      end
    end

  let can_reuse pool entry =
       pool.cur_size <= pool.max_idle_size
    && Option'.for_all (fun n -> entry.used_count < n) pool.max_use_count

  let rec dispose_expiring pool =
    (match pool.max_idle_age, pool.alarm with
     | None, None -> ()
     | Some _, Some _ -> ()
     | None, Some alarm ->
        Alarm.unschedule alarm;
        pool.alarm <- None
     | Some max_idle_age, None ->
        let now = Mtime_clock.now () in
        let rec loop () =
          (match Queue.peek_opt pool.queue with
           | None -> ()
           | Some entry ->
              (match Mtime.add_span entry.used_latest max_idle_age with
               | None ->
                  Logs.warn ~src:pool.log_src (fun f -> f
                    "Cannot schedule pool expiration check due to \
                     Mtime overflow.")
               | Some expiry ->
                  if Mtime.compare now expiry >= 0 then
                    begin
                      let entry = Queue.take pool.queue in
                      pool.cur_size <- pool.cur_size - 1;
                      async ~sw:pool.switch
                        (fun () -> pool.free entry.resource);
                      loop ()
                    end
                  else
                    pool.alarm <- Option.some @@
                      Alarm.schedule
                        ~sw:pool.switch ~connect_env:pool.connect_env expiry
                        begin fun () ->
                          pool.alarm <- None;
                          dispose_expiring pool
                        end))
        in
        loop ())

  let release pool entry =
    if not (can_reuse pool entry) then begin
      pool.cur_size <- pool.cur_size - 1;
      pool.free entry.resource >|= fun () ->
      schedule pool
    end else begin
      pool.check entry.resource begin fun ok ->
        if ok then
          begin
            entry.used_latest <- Mtime_clock.now ();
            Queue.add entry pool.queue;
            dispose_expiring pool
          end
        else
          begin
            Logs.warn ~src:pool.log_src (fun f ->
              f "Will not repool connection due to invalidation.");
            pool.cur_size <- pool.cur_size - 1
          end;
        schedule pool
      end;
      return ()
    end

  let use ?(priority = 0.0) f pool =
    acquire ~priority pool >>=? fun entry ->
    finally
      (fun () -> f entry.resource)
      (fun () -> entry.used_count <- entry.used_count + 1; release pool entry)

  let rec drain pool =
    if pool.cur_size = 0 then
      begin
        pool.alarm |> Option.iter begin fun alarm ->
          Alarm.unschedule alarm;
          pool.alarm <- None
        end;
      return ()
      end
    else
      (match Queue.take_opt pool.queue with
       | None -> wait ~priority:0.0 pool
       | Some entry ->
          pool.cur_size <- pool.cur_size - 1;
          pool.free entry.resource) >>= fun () ->
      drain pool

end

module No_alarm = struct
  type t = unit
  let schedule ~sw:_ ~connect_env:_ _ _ = ()
  let unschedule _ = ()
end

module Make_without_alarm (System : System_sig.CORE) = Make (System) (No_alarm)
