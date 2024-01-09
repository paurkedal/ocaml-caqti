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

open Eio.Std

include Benchmark_all.Make (struct
  let name = "eio-unix"

  module Fiber = struct
    type 'a t = 'a
    module Infix = struct
      let (>>=) x f = f x
      let (>|=) x f = f x
    end
  end

  type context = Caqti_eio.stdenv * Switch.t

  let run_fiber f = f ()

  let run_main f =
    Eio_main.run @@ fun stdenv ->
    Switch.run @@ fun sw ->
    Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) stdenv
      @@ fun () ->
    f ((stdenv :> Caqti_eio.stdenv), sw)

  include Caqti_eio
  include Caqti_eio_unix

  let connect ?config (stdenv, sw) uri = connect ?config ~sw ~stdenv uri
end)
