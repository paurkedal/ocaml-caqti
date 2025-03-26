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

module Pool = Caqti_miou_unix.System.Pool

module Resource = struct
  type t = {
    id: int;
    use_count: int Atomic.t;
  }

  let alive = Hashtbl.create 17
  let alive_mutex = Mutex.create ()

  let with_alive_locked f =
    Mutex.lock alive_mutex;
    Fun.protect ~finally:(fun () -> Mutex.unlock alive_mutex) f

  let latest_id = Atomic.make 0

  let create () =
    let id = Atomic.fetch_and_add latest_id 1 in
    with_alive_locked (fun () -> Hashtbl.add alive id ());
    Ok {id; use_count = Atomic.make 0}

  let create_or_fail () =
    if Random.int 4 = 0 then Error () else
    create ()

  let free resource =
    with_alive_locked begin fun () ->
      assert (Hashtbl.mem alive resource.id);
      Hashtbl.remove alive resource.id
    end
end

exception Timeout

let with_timeout ts fn =
  let prm0 = Miou.async fn in
  let prm1 = Miou.async @@ fun () -> Miou_unix.sleep ts; raise Timeout in
  match Miou.await_first [ prm0; prm1 ] with
  | Ok value -> value
  | Error Timeout -> raise Timeout
  | Error exn ->
      Logs.err (fun m -> m "Unexpected exception: %S" (Printexc.to_string exn));
      raise exn

let test_n n =
  Caqti_miou.Switch.run @@ fun sw ->
  let max_idle_size = Random.int 11 in
  let max_size = max 1 (max_idle_size + Random.int 5) in
  let max_use_count =
    (match Random.bool () with
     | false -> None
     | true -> Some (1 + Random.int 8))
  in
  let pool =
    let config =
      Caqti_pool_config.create ~max_idle_size ~max_size ~max_use_count ()
    in
    Pool.create ~config ~sw ~stdenv:()
      Resource.create_or_fail Resource.free
  in
  let wakers = Array.make n None in
  let wait_count = ref 0 in
  let wait_count_cond = Miou.Condition.create () in
  let wait_count_mutex = Miou.Mutex.create () in
  let wake j c =
    ignore (Miou.Computation.try_return c ());
    wakers.(j) <- None in
  for _ = 0 to 3 * n - 1 do
    let j = Random.int n in
    (* NOTE(dinosaure): This assertion is not necessarily true when another
       domain may be in charge of the asynchronous functions and these are not
       actually launched immediately. *)
    (* assert (Pool.size pool = Hashtbl.length Resource.alive); *)
    (match wakers.(j) with
     | None ->
        let c = Miou.Computation.create () in
        Miou.Mutex.protect wait_count_mutex (fun () -> incr wait_count);
        let task (resource : Resource.t) =
          begin match max_use_count with
          | None -> ()
          | Some n -> assert (Atomic.get resource.use_count < n) end;
          Atomic.incr resource.use_count;
          ignore (Miou.Computation.await c);
          Miou.Mutex.protect wait_count_mutex begin fun () ->
            decr wait_count;
            Miou.Condition.signal wait_count_cond
          end;
          Ok ()
        in
        Caqti_miou.System_core.async ~sw begin fun () ->
          begin match Pool.use task pool with
          | Ok () -> ()
          | Error () ->
             ignore (Miou.Computation.await c);
             Miou.Mutex.protect wait_count_mutex begin fun () ->
               decr wait_count;
               Miou.Condition.signal wait_count_cond
             end;
          end
        end;
        wakers.(j) <- Some c
     | Some c -> wake j c)
  done;
  for j = 0 to n - 1 do
    (match wakers.(j) with
     | None -> Miou.yield ()
     | Some c -> wake j c)
  done;
  let rec wait_for_all () =
    Miou.Mutex.lock wait_count_mutex;
    while !wait_count > 0 do
      Miou.Condition.wait wait_count_cond wait_count_mutex;
    done;
    let n = !wait_count in
    Miou.Mutex.unlock wait_count_mutex;
    if n > 0 then wait_for_all ()
  in
  (* NOTE(dinosaure): see the note below, we need to give a better chance to
     wait all tasks. *)
  with_timeout 5.0 wait_for_all;
  (* FIXME(paurkedal): Re-enable after fixing #126
  assert (Pool.size pool <= max_idle_size); *)
  assert (!wait_count = 0);
  Pool.drain pool;
  assert (Pool.size pool = 0);
  assert (Hashtbl.length Resource.alive = 0)

let test () =
  Miou_unix.run @@ fun () ->
  test_n 0;
  test_n 1;
  (* NOTE(dinosaure): For a simple core, the miou pattern is not the most
     optimised: it's basically a scheduler within a scheduler. So the [Switch]
     ends up in the role of a small scheduler which will execute tasks with
     [async], whereas Miou is a scheduler. The aim is for the [Switch] to
     execute tasks in another domain. In this case, the test execution time is
     reasonable, but for a simple core, this repetition of task management slows
     down the process. We therefore limit the number of tasks to be performed
     for a single core.

     We could offer another [Switch] implementation depending on the number of
     domains available. However, this choice can only be made dynamically. It is
     more reasonable to consider that with miou, we have at least 2 cores
     available. *)
  let max =
    if Miou.Domain.available () > 0
    then 12 else 6 in
  let rec loop n_it =
    if n_it > 0 then begin
      test_n (Random.int (1 lsl max));
      loop (n_it - 1)
    end
  in
  loop 500

let create_gathering n =
  let count = Atomic.make n in
  let c = Miou.Computation.create () in
  fun () ->
    let v = Atomic.fetch_and_add count (-1) in
    Logs.debug (fun m -> m "count:%d" (v - 1));
    if v - 1 > 0 then Miou.Computation.await_exn c
    else begin
      assert (Miou.Computation.try_return c ());
      Logs.debug (fun m -> m "signal others")
    end

let test_age _ =
  Miou_unix.run @@ fun () ->
  Caqti_miou.Switch.run @@ fun sw ->
  let max_size = 8 in
  let max_idle_size = 4 in
  let max_idle_age = Some Mtime.Span.(100 * ms) in
  let pool =
    let config =
      Caqti_pool_config.create ~max_size ~max_idle_size ~max_idle_age ()
    in
    Pool.create ~config ~sw ~stdenv:() Resource.create Resource.free
  in
  let user_count = 8 in
  let join_gathering = create_gathering user_count in
  let f _i _resource =
    Ok (join_gathering ()) in
  let jobs = List.init user_count f in
  let launch job =
    Caqti_miou_unix.System.async ~sw @@ fun () ->
    ignore (Pool.use job pool) in
  List.iter launch jobs;
  (* NOTE(dinosaure): miou does not effectively launch jobs. So, [Pool.use]
     is not yet effectively executed. At this stage, [Pool.size] should be
     equal to [0] but, due to the fact the a domain is probably used to launch
     these jobs, it can be higher than [0]. *)
  (* Alcotest.(check int) "pool size before sleep" 4 (Pool.size pool); *)
  let rec wait_while_draining timeout =
    if Pool.size pool > 0 then begin
      Miou_unix.sleep 0.1; Miou.yield ();
      wait_while_draining (timeout -. 0.1)
    end
  in
  wait_while_draining 5.0;
  Alcotest.(check int) "pool size after sleep" 0 (Pool.size pool)

let test_cases = [
  Alcotest.test_case "basic usage" `Quick test;
  Alcotest.test_case "timed cleanup" `Quick test_age;
]
