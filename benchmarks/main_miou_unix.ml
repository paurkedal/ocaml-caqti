(* Copyright (C) 2022--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

include Benchmark_all.Make (struct
  let name = "miou-unix"

  module Fiber = struct
    type 'a t = 'a
    module Infix = struct
      let (>>=) x f = f x
      let (>|=) x f = f x
    end
  end

  type context = Caqti_miou.switch

  let run_fiber f = f ()

  let run_main f =
    Miou_unix.run @@ fun () ->
    Caqti_miou.Switch.run @@ fun sw ->
    let rng = Mirage_crypto_rng_miou_unix.(initialize (module Pfortuna)) in
    let finally () = Mirage_crypto_rng_miou_unix.kill rng in
    Fun.protect ~finally (fun () -> f sw)

  include Caqti_miou
  include Caqti_miou_unix

  let connect ?config sw uri = connect ?config ~sw uri
end)
