(* Copyright (C) 2025  Petter A. Urkedal <paurkedal@gmail.com>
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

module Type = struct
  type (_, _) eq = Equal : ('a, 'a) eq (* OCaml 5.1 *)
end

module Atomic = struct
  type 'a t = 'a Stdlib.Atomic.t
  let make = Stdlib.Atomic.make
  let fetch_and_add = Stdlib.Atomic.fetch_and_add
end

module Mutex = struct
  let protect mutex f =
    Fun.protect
      ~finally:(fun () -> Stdlib.Mutex.unlock mutex)
      (fun () -> Stdlib.Mutex.lock mutex; f ())
end

let memo_if_safe ?hashed ?weight ~cap f =
  let mutex = Stdlib.Mutex.create () in
  let f' = Lru.memo ?hashed ?weight ~cap f in
  fun x -> Mutex.protect mutex (fun () -> f' x)
