(* Copyright (C) 2016--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_describe
open Caqti_metadata
open Caqti_prereq
open Caqti_query
open Caqti_sigs
open Printf

module Caldate = CalendarLib.Date
module Caldate_format = CalendarLib.Printer.Date
module Caltime = CalendarLib.Calendar
module Caltime_format = CalendarLib.Printer.Calendar

module Caqtus_functor (System : SYSTEM) = struct

  module Mdb = Mariadb.Nonblocking.Make
    (struct
      open System

      module IO = struct
        type 'a future = 'a System.io
        let (>>=) = System.(>>=)
        let return = System.return
      end

      let wait dbh status =
        Mariadb.Nonblocking.fd dbh |> System.Unix.wrap_fd @@ fun fd ->
        let timeout =
          if Mariadb.Nonblocking.Status.timeout status
          then Some (float_of_int (Mariadb.Nonblocking.timeout dbh))
          else None in
        System.Unix.poll
          ~read:(Mariadb.Nonblocking.Status.read status)
          ~write:(Mariadb.Nonblocking.Status.write status)
          ?timeout fd >|=
        (fun (read, write, timeout) ->
          Mariadb.Nonblocking.Status.create ~read ~write ~timeout ())
    end)
  open Mdb

  module Param = struct
    type t = Field.value

    let null = `Null
    let option f = function None -> null | Some x -> (f x)
    let bool x = `Int (if x then 1 else 0)
    let int x = `Int x
    let int32 x = `Int (Int32.to_int x)
    let int64 x = `Int (Int64.to_int x)
    let float x = `Float x
    let string x = `String x
    let bytes x = `Bytes x
    let sub_bytes x i n = `Bytes (Bytes.sub x i n)
    let date x = `Time (Time.utc_timestamp (Caldate.to_unixfloat x))
    let date_tuple (year, month, day) = `Time (Time.date ~year ~month ~day)
    let date_string s = date (Caldate_format.from_fstring "%F" s)
    let utc x = `Time (Time.utc_timestamp (Caltime.to_unixfloat x))
    let utc_float t = `Time (Time.utc_timestamp t)
    let utc_string s = utc (Caltime_format.from_fstring "%FT%T" s)
    let other = string
  end

  module Tuple = struct
    type t = Field.t array

    let is_null i a = Field.null_value a.(i)
    let option f i a = if is_null i a then None else Some (f i a)
    let bool i a = Field.int a.(i) <> 0
    let int i a = Field.int a.(i)
    let int32 i a = Int32.of_int (Field.int a.(i))
    let int64 i a = Int64.of_int (Field.int a.(i))
    let float i a = Field.float a.(i)
    let string i a =
      (match Field.value a.(i) with
       | `String s -> s
       | `Bytes s -> Bytes.to_string s
       | _ -> ksprintf failwith "%s is not a string" (Field.name a.(i)))
    let bytes i a = Field.bytes a.(i)

    let date i a =
      let t = Field.time a.(i) in
      (* FIXME: Check convensions. *)
      Caldate.make (Time.year t) (Time.month t) (Time.day t)
    let date_tuple i a =
      let t = Field.time a.(i) in
      (Time.year t, Time.month t, Time.day t)
    let date_string i a =
      Caldate_format.sprint "%F" (date i a)

    let utc i a =
      let t = Field.time a.(i) in
      (* FIXME: Check convensions. *)
      Caltime.make
        (Time.year t) (Time.month t) (Time.day t)
        (Time.hour t) (Time.minute t) (Time.second t)
    let utc_float i a =
      Caltime.to_unixfloat (utc i a)
    let utc_string i a =
      Caltime_format.sprint "%FT%T" (utc i a)

    let other i a = Field.string a.(i)
  end

  module Report = struct
    type t = unit
    let returned_count = None (* TODO *)
    let affected_count = None (* TODO *)
  end

  module Wrap (Wrapper : WRAPPER) = struct

    open System

    let backend_info =
      create_backend_info
        ~uri_scheme:"mariadb" ~dialect_tag:`Mysql
        ~parameter_style:(`Linear "?")
        ~describe_has_typed_parameters:false (* TODO *)
        ~describe_has_typed_fields:false (* TODO *)
        ~has_transactions:true ()

    let query_info = make_query_info backend_info

    let prepare_failed uri q (code, msg) =
      let msg' = sprintf "Error %d, %s" code msg in
      fail (Caqti.Prepare_failed (uri, query_info q, msg'))
    let execute_failed uri q (code, msg) =
      let msg' = sprintf "Error %d, %s" code msg in
      fail (Caqti.Execute_failed (uri, query_info q, msg'))
    let transaction_failed uri qs (code, msg) =
      let msg' = sprintf "Error %d, %s" code msg in
      fail (Caqti.Execute_failed (uri, `Oneshot qs, msg'))
    let miscommunication uri q msg =
      fail (Caqti.Miscommunication (uri, query_info q, msg))

    module type CONNECTION = sig
      module Tuple : TUPLE
      module Report : REPORT
      include CONNECTION
        with type 'a io = 'a System.io
         and module Tuple := Tuple
         and module Report := Report
         and type 'a callback = 'a Wrapper (Tuple) (Report).callback
    end

    let make_api uri dbh = (module struct
      type 'a io = 'a System.io
      type param = Mdb.Field.t
      type tuple = Mdb.Row.Array.t
      module Param = Param
      module Tuple = Tuple
      module Report = Report
      module W = Wrapper (Tuple) (Report)
      type 'a callback = 'a W.callback

      let uri = uri
      let backend_info = backend_info
      let disconnect () = Mdb.close dbh
      let validate () = return true (* FIXME *)
      let check f = f true (* FIXME *)
      let describe = None (* TODO: Need bindings *)

      let fail_exec q msg = execute_failed uri q msg
      let fail_fetch q msg = execute_failed uri q msg
      let fail_miss q msg = miscommunication uri q msg

      (* TODO: Cache prepared queries, though it may require Stmt.reset which is
       * currently not bound. *)
      let with_prepared q f =
        let qs =
          (match q with
           | Caqti_query.Prepared pq -> pq.Caqti_query.pq_encode backend_info
           | Caqti_query.Oneshot qsf -> qsf backend_info) in
        prepare dbh qs >>=
        (function
         | Error err -> prepare_failed uri q err
         | Ok stmt ->
            catch
              (fun () ->
                f stmt >>= fun y ->
                Mdb.Stmt.close stmt >>= fun _ ->
                return y)
              (fun exn ->
                Mdb.Stmt.close stmt >>= fun _ ->
                fail exn))

      let with_executed q params f =
        with_prepared q @@ fun stmt ->
        Mdb.Stmt.execute stmt params >>=
        (function
         | Error err -> fail_exec q err
         | Ok res -> f res)

      let fetch0 q res =
        Mdb.Res.fetch (module Mdb.Row.Array) res >>=
        (function
         | Ok None -> return ()
         | Ok (Some _) -> fail_miss q "Received unexpected row."
         | Error err -> fail_fetch q err)

      let fetch1 f' q res =
        Mdb.Res.fetch (module Mdb.Row.Array) res >>=
        (function
         | Ok None -> fail_miss q "Res.fetch did not return an expected row."
         | Ok (Some row) ->
            fetch0 q res >>= fun () ->
            return (f' row)
         | Error err -> fail_fetch q err)

      let exec q params =
        let q' = W.on_query q in
        with_executed q params @@
        (function
         | Some res -> fail_miss q "Did not expect result from statement."
         | None ->
            let _ = W.on_report q' () in
            return ())

      let find q f params =
        let q' = W.on_query q in
        with_executed q params
        (function
         | None ->
            fail_miss q "find: Did not receive query result."
         | Some res ->
            let report' = W.on_report q' () in
            let f' = W.on_tuple f report' in
            (match Mdb.Res.num_rows res with
             | 1 -> fetch1 f' q res
             | n -> ksprintf (fail_miss q) "Received %d tuples, expected 1." n))

      let find_opt q f params =
        let q' = W.on_query q in
        with_executed q params
        (function
         | None ->
            fail_miss q "find_opt: Did not receive query result."
         | Some res ->
            let report' = W.on_report q' () in
            let f' = W.on_tuple f report' in
            (match Mdb.Res.num_rows res with
             | 0 -> fetch0 q res >>= fun () -> return None
             | 1 -> fetch1 f' q res >>= fun y -> return (Some y)
             | n -> ksprintf (fail_miss q)
                             "Received %d tuples, expected at most one." n))

      let iter_s q f params =
        let q' = W.on_query q in
        with_executed q params
        (function
         | None ->
            fail_miss q "iter_s or iter_p: Did not receive query result."
         | Some res ->
            let report' = W.on_report q' () in
            let f' = W.on_tuple f report' in
            let rec loop () =
              Mdb.Res.fetch (module Mdb.Row.Array) res >>=
              (function
               | Ok None -> return ()
               | Ok (Some row) -> f' row >>= loop
               | Error err -> fail_fetch q err) in
            loop ())

      let iter_p = iter_s

      let fold_s q f params acc =
        let q' = W.on_query q in
        with_executed q params
        (function
         | None ->
            fail_miss q "fold_s: Did not receive query result."
         | Some res ->
            let report' = W.on_report q' () in
            let f' = W.on_tuple f report' in
            let rec loop acc =
              Mdb.Res.fetch (module Mdb.Row.Array) res >>=
              (function
               | Ok None -> return acc
               | Ok (Some row) -> f' row acc >>= loop
               | Error err -> fail_fetch q err) in
            loop acc)

      let fold q f params acc =
        let q' = W.on_query q in
        with_executed q params
        (function
         | None ->
            fail_miss q "fold: Did not receive query result."
         | Some res ->
            let report' = W.on_report q' () in
            let f' = W.on_tuple f report' in
            let rec loop acc =
              Mdb.Res.fetch (module Mdb.Row.Array) res >>=
              (function
               | Ok None -> return acc
               | Ok (Some row) -> loop (f' row acc)
               | Error err -> fail_fetch q err) in
            loop acc)

      let start () =
        Mdb.autocommit dbh false >>=
        (function
         | Ok () -> return ()
         | Error err -> transaction_failed uri "# SET autocommit = 0" err)

      let commit () =
        Mdb.commit dbh >>= fun commit_result ->
        Mdb.autocommit dbh true >>= fun autocommit_result ->
        (match commit_result, autocommit_result with
         | Ok (), Ok () -> return ()
         | Error err, _ -> transaction_failed uri "# COMMIT" err
         | _, Error err -> transaction_failed uri "# SET autocommit = 1" err)

      let rollback () =
        Mdb.rollback dbh >>=
        (function
         | Ok () -> return ()
         | Error err -> transaction_failed uri "# ROLLBACK" err)

    end : CONNECTION)

    let connect uri =
      let host = Uri.host uri in
      let user = Uri.user uri in
      let pass = Uri.password uri in
      let port = Uri.port uri in
      let db =
        (match Uri.path uri with
         | "/" -> None
         | path ->
            if Filename.dirname path <> "/" then
              raise (Caqti.Connect_failed (uri, "Bad URI"));
            Some (Filename.basename path)) in
      let flags = None (* TODO *) in
      let socket = Uri.get_query_param uri "socket" in
      Mdb.connect ?host ?user ?pass ?db ?port ?socket ?flags () >>=
      (function
       | Ok dbh ->
          let module C : CONNECTION = (val make_api uri dbh) in
          (* MariaDB returns local times but without time zone, so change it to
           * UTC for Caqti sessions. *)
          C.exec (Caqti_query.oneshot_sql "SET time_zone = '+00:00'") [||] >|=
          fun () -> (module C : CONNECTION)
       | Error (code, msg) ->
          raise (Caqti.Connect_failed (uri, sprintf "Error %d, %s" code msg)))
  end
end

let () = Caqti.register_scheme "mariadb" (module Caqtus_functor)
