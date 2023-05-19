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

module type FIBER = sig
  type 'a t

  val return : 'a -> 'a t
  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  end
  val finally : (unit -> 'a t) -> (unit -> unit t) -> 'a t
end

module type S = sig
  type 'a fiber
  type t
  type hook

  exception Off

  val eternal : t
  val create : unit -> t
  val release : t -> unit fiber
  val run : (t -> 'a fiber) -> 'a fiber
  val check : t -> unit
  val on_release_cancellable : t -> (unit -> unit fiber) -> hook
  val remove_hook : hook -> unit
end

module Make (Fiber : FIBER) = struct
  open Fiber.Infix

  type state =
    | On of (unit -> unit Fiber.t) Lwt_dllist.t
    | Off

  type t =
    | Eternal
    | Ephemeral of {mutable state: state}

  type hook = (unit -> unit Fiber.t) Lwt_dllist.node option

  exception Off

  let eternal = Eternal
  let create () = Ephemeral {state = On (Lwt_dllist.create ())}

  let release = function
   | Eternal -> failwith "Tried to release eternal switch."
   | Ephemeral {state = Off} -> Fiber.return ()
   | Ephemeral ({state = On tasks} as arg) ->
      let rec loop () =
        if Lwt_dllist.is_empty tasks then Fiber.return (arg.state <- Off) else
        Lwt_dllist.take_l tasks () >>= loop
      in
      loop ()

  let run f =
    let sw = create () in
    Fiber.finally (fun () -> f sw) (fun () -> release sw)

  let check = function
   | Ephemeral {state = On _} | Eternal -> ()
   | Ephemeral {state = Off} -> raise Off

  let on_release_cancellable sw f =
    (match sw with
     | Eternal -> None
     | Ephemeral {state = On tasks} -> Some (Lwt_dllist.add_l f tasks)
     | Ephemeral {state = Off} -> raise Off)

  let remove_hook hook = Option.iter Lwt_dllist.remove hook
end
