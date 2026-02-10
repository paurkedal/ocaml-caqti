(* Copyright (C) 2025--2026  Petter A. Urkedal <paurkedal@gmail.com>
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

module Make (Fiber : System_sig.FIBER) = struct
  include Fiber.Infix

  let ( let* ) = (>>=)
  let ( let+ ) = (>|=)

  let ( >>=? ) m f =
    m >>= function Ok x -> f x | Error _ as r -> Fiber.return r
  let ( >|=? ) m f =
    m >|= function Ok x -> Ok (f x) | Error _ as r -> r

  let ( let*? ) = ( >>=? )
  let ( let+? ) = ( >|=? )

  let assert_single_use ~what in_use f =
    if !in_use then
      failwith ("Invalid concurrent usage of " ^ what ^ " detected.");
    in_use := true;
    Fiber.cleanup
      (fun () -> f () >|= fun res -> in_use := false; res)
      (fun () -> in_use := false; Fiber.return ())
end
