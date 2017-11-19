(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(* This is an example of how to use Caqti directly in application code.
 * This way of defining typed wrappers around queries should also work for
 * code-generators. *)

open Lwt.Infix

module Bike = struct
  type t = {
    frameno: string;
    owner: string;
    stolen: Ptime.t option;
  }
  let t =
    let encode {frameno; owner; stolen} = (frameno, owner, stolen) in
    let decode (frameno, owner, stolen) = {frameno; owner; stolen} in
    let rep = Caqti_type.(tup3 string string (option ptime)) in
    Caqti_type.Custom {rep; encode; decode}
end

(* Query Strings
 * =============
 *
 * Queries are normally registered in advance.  This allows Caqti to emit
 * prepared queries which are reused throughout the lifetime of the
 * connection.  The two most useful functions to do this are
 *
 *   - [prepare_sql] creates a query string which should work with all SQL
 *     databases of interest.
 *
 *   - [prepare_fun] creates a per-database query string.
 *
 * It's possible to avoid many dialect variation cases by using a function
 * which translates the placeholders for parameters, like [Q._q] defined in
 * [tests/test_sql_lwt.ml]. *)

module Q = struct

  let create_ign_p pt rt rm s = Caqti_request.create_p pt rt rm (fun _ -> s)

  let create_bikereg = create_ign_p
    Caqti_type.unit Caqti_type.unit Caqti_mult.zero
    {eos|
      CREATE TEMPORARY TABLE bikereg (
        frameno text NOT NULL,
        owner text NOT NULL,
        stolen timestamp
      )
    |eos}

  let reg_bike = create_ign_p
    Caqti_type.(tup2 string string) Caqti_type.unit Caqti_mult.zero
    "INSERT INTO bikereg (frameno, owner) VALUES (?, ?)"

  let report_stolen = create_ign_p
    Caqti_type.string Caqti_type.unit Caqti_mult.zero
    "UPDATE bikereg SET stolen = current_timestamp WHERE frameno = ?"

  let select_stolen = create_ign_p
    Caqti_type.unit Bike.t Caqti_mult.many
    "SELECT * FROM bikereg WHERE stolen IS NOT NULL"

  let select_frameno = create_ign_p
    Caqti_type.string Caqti_type.string Caqti_mult.zero_or_one
    "SELECT frameno FROM bikereg WHERE frameno = ?"
end

(* Wrappers around the Generic Execution Functions
 * ===============================================
 *
 * Here we combine the above queries with a suitable execution function, for
 * convenience and to enforce type safety.  We could have defined these in a
 * functor on CONNECTION and used the resulting module in place of Db. *)

(* Db.exec runs a statement which must not return any rows.  Errors are
 * reported as exceptions. *)
let create_bikereg (module Db : Caqti_lwt.CONNECTION_V2) =
  Db.exec Q.create_bikereg ()
let reg_bike (module Db : Caqti_lwt.CONNECTION_V2) frameno owner =
  Db.exec Q.reg_bike (frameno, owner)
let report_stolen (module Db : Caqti_lwt.CONNECTION_V2) frameno =
  Db.exec Q.report_stolen frameno

(* Db.find runs a query which must return at most one row.  The result is a
 * option, since it's common to seach for entries which don't exist. *)
let find_bike_owner frameno (module Db : Caqti_lwt.CONNECTION_V2) =
  Db.find_opt Q.select_frameno frameno

(* Db.iter_s iterates sequentially over the set of result rows of a query. *)
let iter_s_stolen (module Db : Caqti_lwt.CONNECTION_V2) f =
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
  reg_bike db "BIKE-0001" "Ford Perfect" >>=? fun () ->
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
 | Error err -> Lwt_io.eprintl (Caqti_error.to_string_hum err)

let () =
  let uri =
    Uri.of_string (try Sys.getenv "CAQTI_URI" with Not_found -> "sqlite3:") in
  Lwt_main.run (Caqti_lwt.connect_v2 uri >>=? test >>= report_error)
