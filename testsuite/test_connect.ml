(* Copyright (C) 2024  Petter A. Urkedal <paurkedal@gmail.com>
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

module Make (Ground : Testlib.Sig.Ground) = struct
  open Ground
  open Ground.Fiber.Infix

  let test_connect connect =
    let rec loop n =
      if n = 0 then Fiber.return () else
      connect () >>= or_fail >>= fun (module C : CONNECTION) ->
      C.disconnect () >>= fun () ->
      loop (n - 1)
    in
    loop 2049 (* assumes ulimit -n 2048 or smaller *)

  let test_cases = [
    "connect", `Slow, test_connect;
  ]
end
