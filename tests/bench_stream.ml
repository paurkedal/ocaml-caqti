(* Copyright (C) 2019  Petter A. Urkedal <paurkedal@gmail.com>
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

open Lwt.Infix

let data = List.init 100_000 begin fun i ->
  let x = float_of_int (i + 1) in
  1.0 /. (x *. x)
end

module Stream_blocking = Caqti_stream.Make (struct
  type 'a future = 'a
  let return x = x
  let (>>=) x f = f x
  let (>|=) x f = f x
end)

module Stream_lwt = Caqti_stream.Make (struct
  type 'a future = 'a Lwt.t
  let return = Lwt.return
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
end)

let rec seq_of_list xs () =
  (match xs with
   | [] -> Seq.Nil
   | x :: xs' -> Seq.Cons (x, seq_of_list xs'))

let test_list () =
  let _ : float = List.fold_left (+.) 0.0 data in
  ()

let test_seq =
  let seq = seq_of_list data in
  fun () ->
    let _ : float = Seq.fold_left (+.) 0.0 seq in
    ()

let test_stream_blocking =
  let stream = Stream_blocking.of_list data in
  fun () ->
    let (_ : float) =
      Stream_blocking.fold ~f:(+.) stream 0.0 |> function
       | Ok x -> x
       | Error (`Congested err) -> Caqti_common.absurd err
    in
    ()

let test_stream_lwt =
  let stream = Stream_lwt.of_list data in
  fun () -> Lwt_main.run begin
    let%lwt (_ : float) =
      Stream_lwt.fold ~f:(+.) stream 0.0 >|= function
       | Ok x -> x
       | Error (`Congested err) -> Caqti_common.absurd err
    in
    Lwt.return_unit
  end

(*
let test_lwt_stream =
  let stream = Lwt_stream.of_list data in (* OBS: Not re-iterable. *)
  fun () -> Lwt_main.run begin
    let%lwt (_ : float) = Lwt_stream.fold (+.) stream 0.0 in
    Lwt.return_unit
  end
*)

let () =
  let res = Benchmark.throughputN 30 [
    "List", test_list, ();
    "Seq", test_seq, ();
    "Stream_blocking", test_stream_blocking, ();
    "Stream_lwt", test_stream_lwt, ();
  ] in
  print_newline ();
  Benchmark.tabulate res
