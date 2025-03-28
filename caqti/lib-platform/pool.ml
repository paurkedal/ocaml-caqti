(* Copyright (C) 2014--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

module Config = Caqti_pool_config

let default_max_size =
  try int_of_string (Sys.getenv "CAQTI_POOL_MAX_SIZE") with Not_found -> 8

let default_log_src = Logs.Src.create "Caqti_platform.Pool"

module type ALARM = sig
  type switch
  type stdenv

  type t

  val schedule :
    sw: switch ->
    stdenv: stdenv ->
    Mtime.t -> (unit -> unit) -> t

  val unschedule : t -> unit
end

module type S = sig
  type switch
  type stdenv

  include Caqti_pool_sig.S

  val create :
    ?config: Caqti_pool_config.t ->
    ?check: ('a -> (bool -> unit) -> unit) ->
    ?validate: ('a -> bool fiber) ->
    ?log_src: Logs.Src.t ->
    sw: switch ->
    stdenv: stdenv ->
    (unit -> ('a, 'e) result fiber) -> ('a -> unit fiber) ->
    ('a, 'e) t
end

module Make
  (System : System_sig.CORE)
  (Alarm : ALARM
    with type stdenv := System.stdenv
     and type switch := System.Switch.t) =
struct
  open System
  open System.Fiber.Infix

  type reaper_state =
    | Idle
    | Working
    | Waiting of Alarm.t

  let (>>=?) m f =
    m >>= function Ok x -> f x | Error e -> Fiber.return (Error e)

  module Task = struct
    type t = {priority: float; condition: Condition.t}
    let wake {condition; _} = Condition.signal condition
    let compare {priority = pA; _} {priority = pB; _} = Float.compare pB pA
  end

  module Taskq = Heap.Make (Task)

  type 'a entry = {
    resource: 'a;
    mutable used_count: int;
    mutable used_latest: Mtime.t;
  }

  type ('a, +'e) t = {
    stdenv: stdenv;
    switch: Switch.t;
    create: unit -> ('a, 'e) result Fiber.t;
    free: 'a -> unit Fiber.t;
    check: 'a -> (bool -> unit) -> unit;
    validate: 'a -> bool Fiber.t;
    log_src: Logs.Src.t;
    max_idle_size: int;
    max_idle_age: Mtime.Span.t option;
    max_size: int;
    max_use_count: int option;

    (* Mutable *)
    mutex: Mutex.t;
    mutable cur_size: int;
    queue: 'a entry Queue.t;
    mutable waiting: Taskq.t;
    mutable reaper_state: reaper_state;
  }

(*
  let configure c pool =
    Option.iter (fun x -> pool.max_size <- x) Config.(get max_size c);
    Option.iter (fun x -> pool.max_idle_size <- x) Config.(get max_idle_size c);
    Option.iter (fun x -> pool.max_idle_age <- x) Config.(get max_idle_age c);
    Option.iter (fun x -> pool.max_use_count <- x) Config.(get max_use_count c)
*)

  let create
        ?(config = Caqti_pool_config.default)
        ?(check = fun _ f -> f true)
        ?(validate = fun _ -> Fiber.return true)
        ?(log_src = default_log_src)
        ~sw
        ~stdenv
        create free =
    let max_size =
      Config.(get max_size) config |> Option.value ~default:default_max_size in
    let max_idle_size =
      Config.(get max_idle_size) config |> Option.value ~default:max_size in
    let max_idle_age =
      Config.(get max_idle_age) config |> Option.value ~default:None in
    let max_use_count =
      Config.(get max_use_count) config |> Option.value ~default:(Some 100) in
    assert (max_size > 0);
    assert (max_size >= max_idle_size);
    assert (Option.fold ~none:true ~some:(fun n -> n > 0) max_use_count);
    {
      stdenv; switch = sw;
      create; free; check; validate; log_src;
      max_idle_size; max_size; max_use_count; max_idle_age;
      cur_size = 0;
      queue = Queue.create ();
      waiting = Taskq.empty;
      reaper_state = Idle;
      mutex = Mutex.create ();
    }

  let size pool = pool.cur_size (* TODO: atomic *)

  let wait_lck ~priority pool =
    let condition = Condition.create () in
    pool.waiting <- Taskq.push Task.({priority; condition}) pool.waiting;
    Condition.wait condition pool.mutex

  let schedule_lck pool =
    if not (Taskq.is_empty pool.waiting) then begin
      let task, taskq = Taskq.pop_e pool.waiting in
      pool.waiting <- taskq;
      Task.wake task
    end

  let realloc pool =
    let on_error () =
      Mutex.lock pool.mutex >|= fun () ->
      pool.cur_size <- pool.cur_size - 1;
      schedule_lck pool;
      Mutex.unlock pool.mutex
    in
    Fiber.cleanup
      (fun () ->
        pool.create () >>=
        (function
         | Ok resource ->
            Fiber.return @@
              Ok {resource; used_count = 0; used_latest = Mtime_clock.now ()}
         | Error err ->
            on_error () >|= fun () ->
            Error err))
      on_error

  let rec acquire ~priority pool =
    Mutex.lock pool.mutex >>= fun () ->
    acquire_and_unlock ~priority pool
  and acquire_and_unlock ~priority pool =
    if Queue.is_empty pool.queue then begin
      if pool.cur_size < pool.max_size then
        begin
          pool.cur_size <- pool.cur_size + 1;
          Mutex.unlock pool.mutex;
          realloc pool
        end
      else
        begin
          wait_lck ~priority pool >>= fun () ->
          acquire_and_unlock ~priority pool
        end
    end else begin
      let entry = Queue.take pool.queue in
      Mutex.unlock pool.mutex;
      pool.validate entry.resource >>= fun ok ->
      if ok then
        Fiber.return (Ok entry)
      else
        begin
          Log.warn ~src:pool.log_src (fun f ->
            f "Dropped pooled connection due to invalidation.") >>= fun () ->
          realloc pool
        end
    end

  let can_reuse_lck pool entry =
    pool.cur_size <= pool.max_idle_size
     && Option.fold ~none:true ~some:(fun n -> entry.used_count < n)
          pool.max_use_count

  let dispose_expiring_lck pool =
    let rec process_queue_lck () =
      (* We hold the lock and only release it while freeing resources, which
       * means that some other operations may have been processed right before
       * each recursive call. *)
      (match Queue.peek_opt pool.queue, pool.max_idle_age with
       | None, _ | _, None -> Fiber.return ()
       | Some entry, Some max_idle_age ->
          let now = Mtime_clock.now () in
          (match Mtime.add_span entry.used_latest max_idle_age with
           | None ->
              Logs.warn ~src:pool.log_src (fun f -> f
                "Cannot schedule pool expiration check due to \
                 Mtime overflow.");
              pool.reaper_state <- Idle;
              Fiber.return ()
           | Some expiry when Mtime.compare now expiry < 0 ->
              let alarm =
                Alarm.schedule
                  ~sw:pool.switch ~stdenv:pool.stdenv
                  expiry on_alarm
              in
              pool.reaper_state <- Waiting alarm;
              Fiber.return ()
           | Some _ ->
              let entry = Queue.take pool.queue in
              pool.cur_size <- pool.cur_size - 1;
              Mutex.unlock pool.mutex;
              pool.free entry.resource >>= fun () ->
              Mutex.lock pool.mutex >>= fun () ->
              assert (pool.reaper_state = Working);
              process_queue_lck ()))
    and on_alarm () =
      async ~sw:pool.switch begin fun () ->
        Mutex.lock pool.mutex >>= fun () ->
        pool.reaper_state <- Working;
        process_queue_lck () >|= fun () ->
        Mutex.unlock pool.mutex
      end
    in
    (match pool.reaper_state, pool.max_idle_age with
     | Idle, None | Working, _ | Waiting _, Some _ ->
        Fiber.return ()
     | Idle, Some _ ->
        pool.reaper_state <- Working;
        process_queue_lck ()
     | Waiting alarm, None ->
        (* Not reachable unless live reconfiguration is implemented. *)
        Alarm.unschedule alarm;
        pool.reaper_state <- Idle;
        Fiber.return ())

  let release pool entry =
    Mutex.lock pool.mutex >>= fun () ->
    entry.used_count <- entry.used_count + 1;
    if not (can_reuse_lck pool entry) then
      begin
        pool.cur_size <- pool.cur_size - 1;
        Mutex.unlock pool.mutex;
        pool.free entry.resource >>= fun () ->
        Mutex.lock pool.mutex >|= fun () ->
        schedule_lck pool;
        Mutex.unlock pool.mutex
      end
    else
      begin
        Mutex.unlock pool.mutex;
        (* TODO: Consider changing the signature of check to return a bool
         * Fiber.t to avoid the async call. *)
        pool.check entry.resource begin fun ok ->
          async ~sw:pool.switch @@ fun () ->
          Mutex.lock pool.mutex >>= fun () ->
          begin
            if ok then
              begin
                entry.used_latest <- Mtime_clock.now ();
                Queue.add entry pool.queue;
                dispose_expiring_lck pool
              end
            else
              begin
                Logs.warn ~src:pool.log_src (fun f ->
                  f "Will not repool connection due to invalidation.");
                pool.cur_size <- pool.cur_size - 1;
                Fiber.return ()
              end
          end >|= fun () ->
          schedule_lck pool;
          Mutex.unlock pool.mutex
        end;
        Fiber.return ()
      end

  let use ?(priority = 0.0) f pool =
    acquire ~priority pool >>=? fun entry ->
    Fiber.finally
      (fun () -> f entry.resource)
      (fun () -> release pool entry)

  let rec drain pool =
    Mutex.lock pool.mutex >>= fun () ->
    drain_and_unlock pool
  and drain_and_unlock pool =
    if pool.cur_size = 0 then
      begin
        (match pool.reaper_state with
         | Idle | Working -> ()
         | Waiting alarm ->
            Alarm.unschedule alarm;
            pool.reaper_state <- Idle);
        Mutex.unlock pool.mutex;
        Fiber.return ()
      end
    else
      (match Queue.take_opt pool.queue with
       | None ->
          wait_lck ~priority:0.0 pool >>= fun () ->
          drain_and_unlock pool
       | Some entry ->
          pool.cur_size <- pool.cur_size - 1;
          Mutex.unlock pool.mutex;
          pool.free entry.resource >>= fun () ->
          drain pool)

end

module No_alarm = struct
  type t = unit
  let schedule ~sw:_ ~stdenv:_ _ _ = ()
  let unschedule _ = ()
end

module Make_without_alarm (System : System_sig.CORE) = Make (System) (No_alarm)
