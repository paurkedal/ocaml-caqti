(* Copyright (C) 2019--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

module Make_helpers
  (System : System_sig.S) =
struct
  open System
  open System.Fiber.Infix

  let assert_single_use ~what in_use f =
    if !in_use then
      failwith ("Invalid concurrent usage of " ^ what ^ " detected.");
    in_use := true;
    Fiber.cleanup
      (fun () -> f () >|= fun res -> in_use := false; res)
      (fun () -> in_use := false; Fiber.return ())
end

module Make_convenience
  (System : System_sig.S)
  (C : Caqti_connection_sig.Base
        with type 'a fiber := 'a System.Fiber.t
         and type ('a, 'err) stream := ('a, 'err) System.Stream.t) =
struct
  open System
  open System.Fiber.Infix
  module Response = C.Response
  let (>>=?) m f = m >>= function Ok x -> f x | Error _ as r -> Fiber.return r
  let (>|=?) m f = m >|= function Ok r -> Ok (f r) | Error _ as r -> r

  let exec q p = C.call ~f:Response.exec q p
  let find q p = C.call ~f:Response.find q p
  let find_opt q p = C.call ~f:Response.find_opt q p
  let fold q f p acc = C.call ~f:(fun resp -> Response.fold f resp acc) q p
  let fold_s q f p acc = C.call ~f:(fun resp -> Response.fold_s f resp acc) q p
  let iter_s q f p = C.call ~f:(fun resp -> Response.iter_s f resp) q p
  let collect_list q p =
    let f resp = Response.fold List.cons resp [] >|= Result.map List.rev in
    C.call ~f q p
  let rev_collect_list q p =
    let f resp = Response.fold List.cons resp [] in
    C.call ~f q p

  let exec_with_affected_count q p =
    let f response =
      Response.exec response >>= fun execResult ->
      match execResult with
      | Ok () -> Response.affected_count response
      | Error x -> Fiber.return (Error x) in
    C.call ~f q p

  let with_transaction f =
    C.start () >>=? fun () ->
    Fiber.cleanup
      (fun () ->
        f () >>= (function
         | Ok y -> C.commit () >|=? fun () -> y
         | Error _ as r -> C.rollback () >|= fun _ -> r))
      (fun () -> C.rollback () >|= ignore)
end

module Make_populate
  (System : System_sig.S)
  (C : Caqti_connection_sig.Base
        with type 'a fiber := 'a System.Fiber.t
         and type ('a, 'e) stream := ('a, 'e) System.Stream.t) =
struct
  open System
  open System.Fiber.Infix
  let (>>=?) m f = m >>= function Ok x -> f x | Error _ as r -> Fiber.return r

  let populate ~table ~columns row_type =
    let request =
      let open Caqti_template.Create in
      dynamic_gen T.(row_type -->. unit) @@ Fun.const @@
      Q.concat [
        Q.lit "INSERT INTO "; Q.lit table; Q.lit "(";
        Q.concat ~sep:", " (List.map Q.lit columns);
        Q.lit ") VALUES (";
        Q.concat ~sep:", " (List.mapi (fun i _ -> Q.param i) columns);
        Q.lit ")";
      ]
    in
    fun data ->
      C.start () >>=? fun () ->
      Stream.iter_s ~f:(C.call ~f:C.Response.exec request) data >>= fun res ->
      C.deallocate request >>= fun _ ->
      (match res with
       | Ok () ->
          C.commit ()
       | Error (`Congested err) ->
          C.rollback () >>=? fun () ->
          Fiber.return (Error (`Congested err))
       | Error err ->
          Fiber.return (Error err))
end
