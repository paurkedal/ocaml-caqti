(* Copyright (C) 2022--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

include Benchmark_fetch_many.Make (struct
  let name = "blocking"
  module Fiber = struct
    type 'a t = 'a
    module Infix = struct
      let (>>=) x f = f x
      let (>|=) x f = f x
    end
  end
  type context = unit
  let run_fiber f = f ()
  let run_main f = f ()
  include Caqti_blocking
  let connect ?config () uri = connect ?config uri
end)
