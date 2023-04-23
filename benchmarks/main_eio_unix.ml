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

open Eio.Std

include Benchmark_fetch_many.Make (struct
  let name = "eio-unix"
  type 'a future = 'a
  type context = Eio.Stdenv.t * Switch.t
  let run_fiber f = f ()
  let run_main f =
    Eio_main.run (fun stdenv -> Switch.run (fun sw -> f (stdenv, sw)))
  let (>>=) x f = f x
  let (>|=) x f = f x
  include Caqti_eio
  include Caqti_eio_unix
  let connect (stdenv, sw) uri = connect stdenv ~sw uri
end)
