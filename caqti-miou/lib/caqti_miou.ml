(* Copyright (C) 2023--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_platform

external reraise : exn -> 'a = "%reraise"

module Fiber = struct
  type 'a t = 'a

  module Infix = struct
    let ( >>= ) x f = f x
    let ( >|= ) x f = f x
  end

  let return x = x
  let catch f g = try f () with exn -> g exn
  let finally f finally = Fun.protect ~finally f
  let cleanup f g = try f () with exn -> g (); raise exn
end

module Stream = Caqti_platform.Stream.Make (Fiber)

module Log = struct
  type 'a log = 'a Logs.log

  let err ?(src = Logging.default_log_src) = Logs.err ~src
  let warn ?(src = Logging.default_log_src) = Logs.warn ~src
  let info ?(src = Logging.default_log_src) = Logs.info ~src
  let debug ?(src = Logging.default_log_src) = Logs.debug ~src
end

module Sequencer = struct
  type 'a t = 'a * Miou.Mutex.t

  let create v = (v, Miou.Mutex.create ())
  let enqueue (v, m) f = Miou.Mutex.protect m (fun () -> f v)
end

type switch =
  { stop : bool Atomic.t
  ; mutex : Miou.Mutex.t
  ; condition : Miou.Condition.t
  ; hooks : (unit -> unit) Miou.Sequence.t
  ; jobs : [ `Job of (unit -> unit) | `Check ] Miou.Queue.t }


module Switch = struct
  type hook = Miou.Mutex.t * (unit -> unit) Miou.Sequence.node
  type t = switch

  exception Off

  let create () =
    { stop= Atomic.make true
    ; mutex= Miou.Mutex.create ()
    ; condition= Miou.Condition.create ()
    ; hooks= Miou.Sequence.create ()
    ; jobs= Miou.Queue.create () }

  let eternal = create ()

  let release t =
    Miou.Mutex.protect t.mutex @@ fun () ->
    List.iter (fun fn -> fn ()) (Miou.Sequence.to_list t.hooks);
    Miou.Sequence.drop t.hooks

  let rec terminate orphans =
    match Miou.care orphans with
    | None -> ()
    | Some None -> Miou.yield (); terminate orphans
    | Some (Some prm) -> (
        match Miou.await prm with
        | Ok () -> terminate orphans
        | Error exn ->
            Log.err (fun m -> m "Got an unexpected error: %S" (Printexc.to_string exn));
            terminate orphans)

  let rec clean orphans =
    match Miou.care orphans with
    | None | Some None -> Miou.yield ()
    | Some (Some prm) -> (
        match Miou.await prm with
        | Ok () -> clean orphans
        | Error exn ->
            Log.err (fun m -> m "Got an unexpected error: %S" (Printexc.to_string exn));
            clean orphans)

  let rec worker ~orphans t =
    clean orphans;
    let jobs = Miou.Mutex.protect t.mutex @@ fun () ->
      if Miou.Queue.is_empty t.jobs && Atomic.get t.stop = false
      then Miou.Condition.wait t.condition t.mutex;
      Miou.Queue.(to_list (transfer t.jobs)) in
    let prgm = function
      | `Job fn -> ignore (Miou.async ~orphans fn)
      | `Check -> clean orphans in
    List.iter prgm jobs;
    if Atomic.get t.stop = false
    then worker ~orphans t
    else terminate orphans

  let worker t () =
    let orphans = Miou.orphans () in
    worker ~orphans t

  let stop ~daemon t =
    Miou.Mutex.protect t.mutex begin fun () ->
      Atomic.set t.stop true;
      Miou.Condition.signal t.condition
    end;
    match Miou.await daemon with
    | Ok () -> ()
    | Error exn ->
        Log.err (fun m -> m "our worker finished with: %S" (Printexc.to_string exn));
        reraise exn

  let enqueue t fn =
    Miou.Mutex.protect t.mutex @@ fun () ->
    Miou.Queue.enqueue t.jobs (`Job fn);
    Miou.Condition.signal t.condition

  let call_if_available fn =
    if Miou.Domain.available () > 0
    then Miou.call fn
    else Miou.async fn

  let run fn =
    let t = { stop= Atomic.make false
            ; mutex= Miou.Mutex.create ()
            ; condition= Miou.Condition.create ()
            ; hooks= Miou.Sequence.create ()
            ; jobs= Miou.Queue.create () } in
    let daemon = call_if_available (worker t) in
    match fn t with
    | value -> stop ~daemon t; release t; value
    | exception exn ->
        Log.debug (fun m -> m "our function finished with: %S" (Printexc.to_string exn));
        stop ~daemon t; release t; reraise exn

  let on_release_cancellable t fn =
    Miou.Mutex.protect t.mutex @@ fun () ->
    let hook = Miou.Sequence.(add Left) t.hooks fn in
    (t.mutex, hook)

  let remove_hook (mutex, hook) =
    Miou.Mutex.protect mutex @@ fun () ->
    Miou.Sequence.remove hook

  let check t =
    if Atomic.get t.stop then raise Off
    else Miou.Mutex.protect t.mutex @@ fun () ->
      Miou.Queue.enqueue t.jobs `Check;
      Miou.Condition.signal t.condition
end

module System_core = struct
  module Fiber = Fiber
  module Stream = Stream
  module Switch = Switch
  module Mutex = Miou.Mutex
  module Condition = Miou.Condition
  module Log = Log
  module Sequencer = Sequencer

  let async ~sw fn = Switch.enqueue sw fn
end

module type CONNECTION = Caqti_connection_sig.S
  with type 'a fiber := 'a
    and type ('a, 'e) stream := ('a, 'e) Stream.t

type connection = (module CONNECTION)

let or_fail = function
  | Ok x -> x
  | Error (#Caqti_error.t as err) -> raise (Caqti_error.Exn err)
