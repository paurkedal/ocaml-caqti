(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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
open Caqti_query
open Caqti_sigs
open Printf
open Sqlite3

module Param = struct
  open Sqlite3.Data
  let null = NULL
  let option f = function None -> NULL | Some x -> f x
  let bool = function false -> INT Int64.zero | true -> INT Int64.one
  let int x = INT (Int64.of_int x)
  let int64 x = INT x
  let float x = FLOAT x
  let string x = TEXT x (* FIXME: Don't know if it's binary. *)
  let date x = assert false (* TODO *)
  let utc x = assert false (* TODO *)
end

let invalid_decode typename i stmt =
  let msg = sprintf "Expected %s in column %d of Sqlite result." typename i in
  raise (Invalid_argument msg)

module Tuple = struct
  open Sqlite3.Data
  let length stmt = Sqlite3.column_count stmt
  let raw i stmt = to_string (Sqlite3.column stmt i)
  let is_null i stmt =
    match Sqlite3.column stmt i with NONE | NULL -> true | _ -> false
  let option f i stmt =
    match Sqlite3.column stmt i with NONE | NULL -> None | _ -> Some (f i stmt)
  let bool i stmt =
    match Sqlite3.column stmt i with
    | INT x when x = Int64.zero -> false
    | INT x when x = Int64.one -> true
    | _ -> invalid_decode "bool" i stmt
  let int i stmt =
    match Sqlite3.column stmt i with
    | INT x -> Int64.to_int x
    | _ -> invalid_decode "int" i stmt
  let int64 i stmt =
    match Sqlite3.column stmt i with
    | INT x -> x
    | _ -> invalid_decode "int" i stmt
  let float i stmt =
    match Sqlite3.column stmt i with
    | FLOAT x -> x
    | _ -> invalid_decode "float" i stmt
  let string i stmt =
    match Sqlite3.column stmt i with
    | TEXT x -> x
    | _ -> invalid_decode "string" i stmt
  let date i stmt = assert false (* TODO *)
  let utc i stmt = assert false (* TODO *)
end

let yield = Thread.yield

module Connect_functor = struct
module Make (System : SYSTEM) = struct
  open System

  module type CONNECTION = CONNECTION with type 'a io = 'a System.io

  type 'a io = 'a System.io

  let query_language =
    create_query_language ~name:"sqlite3" ~tag:`SQLite ()

  let connect ?(max_pool_size = 1) uri =

    (* Check URI *)

    assert (Uri.scheme uri = Some "sqlite3");
    begin match Uri.userinfo uri, Uri.host uri with
    | None, (None | Some "") -> return ()
    | _ -> fail (Invalid_argument "Sqlite URI cannot contain user or host \
				   components.")
    end >>= fun () ->

    (* Helpers *)

    let mutex = Mutex.create () in

    let with_db f =
      Preemptive.detach begin fun () ->
	Mutex.lock mutex;
	finally (fun () -> Mutex.unlock mutex)
	  begin fun () ->
	    let db = Sqlite3.db_open (Uri.path uri) in
	    finally
	      (fun () -> while not (Sqlite3.db_close db) do yield () done)
	      (fun () -> f db)
	  end
      end () in

    let raise_rc q rc =
      raise (Caqti.Execute_failed (uri, q, Sqlite3.Rc.to_string rc)) in

    let prim_exec extract q params =
      let qs =
	match q with
	| Prepared {prepared_sql} -> prepared_sql query_language
	| Oneshot qs -> qs in
      let prepare_failed msg =
	match q with
	| Prepared pq -> raise (Caqti.Prepare_failed (uri, pq, msg))
	| Oneshot _ ->   raise (Caqti.Execute_failed (uri, q, msg)) in
      with_db begin fun db ->
	let stmt =
	  try Sqlite3.prepare db qs
	  with Sqlite3.Error msg -> prepare_failed msg in
	finally (fun () -> ignore (Sqlite3.finalize stmt))
	  begin fun () ->
	    for i = 0 to Array.length params - 1 do
	      match Sqlite3.bind stmt i params.(i) with
	      | Sqlite3.Rc.OK -> ()
	      | rc -> prepare_failed (Sqlite3.Rc.to_string rc)
	    done;
	    extract stmt
	  end
      end in

    (* The Module *)

    return (module struct
      type 'a io = 'a System.io
      type param = Data.t
      type tuple = stmt

      module Param = Param
      module Tuple = Tuple

      let drain () = return ()

      let exec q params =
	let extract stmt =
	  match Sqlite3.step stmt with
	  | Sqlite3.Rc.OK -> ()
	  | rc -> raise_rc q rc in
	prim_exec extract q params

      let find q f params =
	let rec extract stmt =
	  match Sqlite3.step stmt with
	  | Sqlite3.Rc.DONE -> None
	  | Sqlite3.Rc.ROW ->
	    let r = f stmt in
	    begin match Sqlite3.step stmt with
	    | Sqlite3.Rc.DONE -> Some r
	    | rc -> raise_rc q rc
	    end
	  | Sqlite3.Rc.BUSY | Sqlite3.Rc.LOCKED -> yield (); extract stmt
	  | rc -> raise_rc q rc in
	prim_exec extract q params

      let fold q f params acc =
	let rec extract stmt =
	  let rec loop acc =
	    match Sqlite3.step stmt with
	    | Sqlite3.Rc.DONE -> acc
	    | Sqlite3.Rc.ROW -> loop (f stmt acc)
	    | Sqlite3.Rc.BUSY | Sqlite3.Rc.LOCKED -> yield (); loop acc
	    | rc -> raise_rc q rc in
	  loop acc in
	prim_exec extract q params

      let fold_s q f params acc =
	let rec extract stmt =
	  let rec loop acc =
	    match Sqlite3.step stmt with
	    | Sqlite3.Rc.DONE -> acc
	    | Sqlite3.Rc.ROW ->
	      let m = f stmt acc in
	      loop (Preemptive.run_in_main (fun () -> m))
	    | Sqlite3.Rc.BUSY | Sqlite3.Rc.LOCKED -> yield (); loop acc
	    | rc -> raise_rc q rc in
	  loop acc in
	prim_exec extract q params

      let iter_s q f params =
	let rec extract stmt =
	  let rec loop () =
	    match Sqlite3.step stmt with
	    | Sqlite3.Rc.DONE -> ()
	    | Sqlite3.Rc.ROW ->
	      let m = f stmt in
	      Preemptive.run_in_main (fun () -> m); loop ()
	    | Sqlite3.Rc.BUSY | Sqlite3.Rc.LOCKED -> yield (); loop ()
	    | rc -> raise_rc q rc in
	  loop () in
	prim_exec extract q params

      let iter_p = iter_s (* TODO *)

    end : CONNECTION)

end (* Make *)
end (* Connect_functor *)

let () = Caqti.register_scheme "sqlite3" (module Connect_functor)
