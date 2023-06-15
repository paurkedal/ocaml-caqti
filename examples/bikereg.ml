(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(* This is an example of how to use Caqti directly in application code.
 * This way of defining typed wrappers around queries should also work for
 * code-generators. *)

open Lwt.Infix

(* This is an example of a custom type, but it is experimental for now.  The
 * alternative is to use plain tuples, as the other queries below, and coverting
 * in client code. *)
module Bike = struct
  type t = {
    frameno: string;
    owner: string;
    stolen: Ptime.t option;
  }
end

(* Query Strings
 * =============
 *
 * Queries are normally defined in advance.  This allows Caqti to emit
 * prepared queries which are reused throughout the lifetime of the
 * connection. *)

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let bike =
    let open Bike in
    let intro frameno owner stolen = {frameno; owner; stolen} in
    product intro
      @@ proj string (fun bike -> bike.frameno)
      @@ proj string (fun bike -> bike.owner)
      @@ proj (option ptime) (fun bike -> bike.stolen)
      @@ proj_end

  let create_bikereg =
    unit ->. unit @@
    {eos|
      CREATE TEMPORARY TABLE bikereg (
        frameno text NOT NULL,
        owner text NOT NULL,
        stolen timestamp NULL
      )
    |eos}

  let reg_bike =
    t2 string string ->. unit @@
    "INSERT INTO bikereg (frameno, owner) VALUES (?, ?)"

  let report_stolen =
    string ->. unit @@
    "UPDATE bikereg SET stolen = current_timestamp WHERE frameno = ?"

  let select_stolen =
    unit ->* bike @@
    "SELECT * FROM bikereg WHERE NOT stolen IS NULL"

  let select_owner =
    string ->? string @@
    "SELECT owner FROM bikereg WHERE frameno = ?"
end

(* Wrappers around the Generic Execution Functions
 * ===============================================
 *
 * Here we combine the above queries with a suitable execution function, for
 * convenience and to enforce type safety.  We could have defined these in a
 * functor on CONNECTION and used the resulting module in place of Db. *)

(* Db.exec runs a statement which must not return any rows.  Errors are
 * reported as exceptions. *)
let create_bikereg (module Db : Caqti_lwt.CONNECTION) =
  Db.exec Q.create_bikereg ()
let reg_bike (module Db : Caqti_lwt.CONNECTION) frameno owner =
  Db.exec Q.reg_bike (frameno, owner)
let report_stolen (module Db : Caqti_lwt.CONNECTION) frameno =
  Db.exec Q.report_stolen frameno

(* Db.find runs a query which must return at most one row.  The result is a
 * option, since it's common to seach for entries which don't exist. *)
let find_bike_owner frameno (module Db : Caqti_lwt.CONNECTION) =
  Db.find_opt Q.select_owner frameno

(* Db.iter_s iterates sequentially over the set of result rows of a query. *)
let iter_s_stolen (module Db : Caqti_lwt.CONNECTION) f =
  Db.iter_s Q.select_stolen f ()

(* There is also a Db.iter_p for parallel processing, and Db.fold and
 * Db.fold_s for accumulating information from the result rows. *)


(* Test Code
 * ========= *)

let (>>=?) m f =
  m >>= (function | Ok x -> f x | Error err -> Lwt.return (Error err))

let test db =
  (* Examples of statement execution: Create and populate the register. *)
  create_bikereg db >>=? fun () ->
  reg_bike db "BIKE-0000" "Arthur Dent" >>=? fun () ->
  reg_bike db "BIKE-0001" "Ford Prefect" >>=? fun () ->
  reg_bike db "BIKE-0002" "Zaphod Beeblebrox" >>=? fun () ->
  reg_bike db "BIKE-0003" "Trillian" >>=? fun () ->
  reg_bike db "BIKE-0004" "Marvin" >>=? fun () ->
  report_stolen db "BIKE-0000" >>=? fun () ->
  report_stolen db "BIKE-0004" >>=? fun () ->

  (* Examples of single-row queries. *)
  let show_owner frameno =
    find_bike_owner frameno db >>=? fun owner_opt ->
    (match owner_opt with
     | Some owner -> Lwt_io.printf "%s is owned by %s.\n" frameno owner
     | None -> Lwt_io.printf "%s is not registered.\n" frameno)
    >>= Lwt.return_ok in
  show_owner "BIKE-0003" >>=? fun () ->
  show_owner "BIKE-0042" >>=? fun () ->

  (* An example multi-row query. *)
  Lwt_io.printf "Stolen:" >>= fun () ->
  iter_s_stolen db
    (fun bike ->
      let stolen =
        match bike.Bike.stolen with Some x -> x | None -> assert false in
      Lwt_io.printf "\t%s %s %s\n" bike.Bike.frameno
                    (Ptime.to_rfc3339 stolen) bike.Bike.owner >>= Lwt.return_ok)

let report_error = function
 | Ok () -> Lwt.return_unit
 | Error err ->
    Lwt_io.eprintl (Caqti_error.show err) >|= fun () -> exit 69

let main {Testlib.uris; tweaks_version} = Lwt_main.run begin
  uris |> Lwt_list.iter_s begin fun uri ->
    Caqti_lwt_unix.with_connection ?tweaks_version uri test >>= report_error
  end
end

let main_cmd =
  let open Cmdliner in
  let doc = "Caqti bikereg example." in
  (* If you wish to play with this outside the Caqti distribution, replace
     "Testlib.common_args" with the "uris" definition from that function. *)
  let term = Term.(const main $ Testlib.common_args) in
  let info = Cmd.info ~doc "bikereg" in
  Cmd.v info term

let () = exit (Cmdliner.Cmd.eval main_cmd)
