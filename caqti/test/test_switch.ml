(* Copyright (C) 2023  Petter A. Urkedal <paurkedal@gmail.com>
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

module A = Alcotest.V1

module Switch = Caqti_platform.Switch.Make (struct
  type 'a future = 'a
  let return = Fun.id
  let (>>=) m f = f m
  let finally f h = try let y = f () in h (); y with exn -> h (); raise exn
end)

let test_switch sw =
  Switch.check sw;
  let state = ref 0 in
  let transit i j () = A.(check int) "transit" i !state; state := j in
  let _ = Switch.on_release_cancellable sw (transit 2 3) in
  let hook1 = Switch.on_release_cancellable sw (fun () -> A.fail "removed") in
  let _ = Switch.on_release_cancellable sw (transit 1 2) in
  let hook2 = Switch.on_release_cancellable sw (fun () -> A.fail "removed") in
  let _ = Switch.on_release_cancellable sw (transit 0 1) in
  Switch.remove_hook hook1;
  Switch.remove_hook hook2;
  Switch.check sw

let test_eternal () =
  let sw = Switch.eternal in
  let _ = Switch.on_release_cancellable sw (fun () -> A.fail "on eternal") in
  let hook = Switch.on_release_cancellable sw (fun () -> A.fail "on eternal") in
  Switch.remove_hook hook

let test_create () =
  let sw = Switch.create () in
  test_switch sw;
  Switch.release sw;
  A.check_raises "released switch" Switch.Off (fun () -> Switch.check sw)

let test_run () =
  let sw' = ref None in
  Switch.run begin fun sw ->
    sw' := Some sw;
    test_switch sw
  end;
  A.check_raises "outside run"
    Switch.Off (fun () -> Option.iter Switch.check !sw')

let test_cases = [
  A.test_case "eternal" `Quick test_eternal;
  A.test_case "create" `Quick test_create;
  A.test_case "run" `Quick test_run;
]
