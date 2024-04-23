(* Copyright (C) 2023--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

include Caqti_miou.System_core
include Caqti_miou
include Caqti_miou_unix

module Fiber = struct
  include Fiber

  let fail = raise

  module Infix = struct
    include Infix
    let (>>=?) = Result.bind
    let (>|=?) x f = Result.map f x
  end
end

module Alcotest_cli =
  Testlib.Make_alcotest_cli
    (Alcotest.Unix_platform)
    (Alcotest_engine.Monad.Identity)

module List_result_fiber = struct
  open Fiber.Infix

  let rec iter_s f = function
   | [] -> Fiber.return (Ok ())
   | x :: xs -> f x >>=? fun () -> iter_s f xs
end
