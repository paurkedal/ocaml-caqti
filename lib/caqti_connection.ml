(* Copyright (C) 2019  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Caqti_prereq
open Printf

module Make_helpers
  (System : Caqti_driver_sig.System_common) =
struct
  open System

  let assert_single_use in_use f =
    assert (not !in_use);
    in_use := true;
    f () >|= fun y ->
    assert !in_use;
    in_use := false;
    y
end

module Make_convenience
  (System : Caqti_driver_sig.System_common)
  (C : Caqti_connection_sig.Base
        with type 'a future := 'a System.future
         and type ('a, 'err) stream := ('a, 'err) System.Stream.t) =
struct
  open System
  module Response = C.Response

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
end

module Make_populate
  (System : Caqti_driver_sig.System_common)
  (C : Caqti_connection_sig.Base
        with type 'a future := 'a System.future
         and type ('a, 'e) stream := ('a, 'e) System.Stream.t) =
struct
  open System
  let (>>=?) m f = m >>= function Ok x -> f x | Error _ as r -> return r

  let populate ~table ~columns row_type data =

    let request =
      let columns_tuple = String.concat ", " columns in
      let q =
        let open Caqti_query in
        S[L(sprintf "INSERT INTO %s (%s) VALUES (" table columns_tuple);
          concat ", " (List.mapi (fun i _ -> P i) columns); L")"]
      in
      Caqti_request.create ~oneshot:true
        row_type Caqti_type.unit Caqti_mult.zero (fun _ -> q)
    in

    C.start () >>=? fun () ->
    Stream.iter_s ~f:(C.call ~f:C.Response.exec request) data >>= function
     | Ok () ->
        C.commit ()
     | Error (`Congested err) ->
        C.rollback () >>=? fun () ->
        return (Error (`Congested err))
     | Error err ->
        return (Error err)
end
