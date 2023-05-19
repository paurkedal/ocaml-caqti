(* Copyright (C) 2021--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

module type Alcotest_cli = sig
  include Alcotest_engine.V1.Cli.S

  val test_case :
    string -> Alcotest.speed_level -> ('a -> return) -> 'a test_case

  val test_case_sync :
    string -> Alcotest.speed_level -> ('a -> unit) -> 'a test_case

  val run_with_args_dependency :
    ?argv: string array -> string ->
    'a Cmdliner.Term.t -> ('a -> unit test list) -> return
end

module type Ground = sig
  module Fiber : sig
    type +'a t

    module Infix : sig
      val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
      val (>|=) : 'a t -> ('a -> 'b) -> 'b t

      val (>>=?) :
        ('a, 'e) result t -> ('a -> ('b, 'e) result t) -> ('b, 'e) result t

      val (>|=?) :
        ('a, 'e) result t -> ('a -> 'b) -> ('b, 'e) result t
    end

    val return : 'a -> 'a t

    val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t

    val fail : exn -> 'a t

  end

  val or_fail : ('a, [< Caqti_error.t]) result -> 'a Fiber.t

  module Stream : Caqti_stream_sig.S with type 'a fiber := 'a Fiber.t

  module Pool : Caqti_pool_sig.S with type 'a fiber := 'a Fiber.t

  module type CONNECTION = Caqti_connection_sig.S
    with type 'a fiber := 'a Fiber.t
     and type ('a, 'err) stream := ('a, 'err) Stream.t

  type connection = (module CONNECTION)

  module Alcotest_cli : Alcotest_cli with type return = unit Fiber.t

  module List_result_fiber : sig
    val iter_s :
      ('a -> (unit, 'e) result Fiber.t) -> 'a list -> (unit, 'e) result Fiber.t
  end

end
