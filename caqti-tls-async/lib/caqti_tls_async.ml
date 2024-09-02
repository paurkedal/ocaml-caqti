(* Copyright (C) 2023--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

open Async_kernel
open Async_unix
open Core
open Tls_async

(* BEGIN IMPORT
 *
 * The following functions were copied from the tls_async.ml from the ocaml-tls
 * project (https://github.com/mirleft/ocaml-tls) with minor changes.
 * The original code is under BSD-2-Clause license. *)
let try_to_close t =
  match%bind Session.close_tls t with
  | Ok () -> return ()
  | Error tls_close_error ->
    Caqti_async.System.Log.err (fun m ->
      m "Failed to close TLS session: %a" Error.pp tls_close_error)
;;

let pipe t =
  let b_reader = Bytes.create 0x8000 in
  let rec f_reader writer =
    match%bind Session.read t b_reader with
    | Ok 0 ->
      Pipe.close writer;
      return ()
    | Ok len ->
      let%bind () = Pipe.write writer (Stdlib.Bytes.sub_string b_reader 0 len) in
      f_reader writer
    | Error read_error ->
      let%map () =
        Caqti_async.System.Log.err (fun m ->
          m "TLS read failed: %a" Error.pp read_error)
      in
      Pipe.close writer
  in
  let rec f_writer reader =
    let%bind pipe_read = Pipe.read reader in
    match pipe_read with
    | `Ok s ->
      (match%bind Session.writev t [ s ] with
       | Ok () -> f_writer reader
       | Error (_ : Error.t) -> try_to_close t)
    | `Eof -> try_to_close t
  in
  Pipe.create_reader ~close_on_exception:false f_reader, Pipe.create_writer f_writer
;;

let upgrade_connection tls_session ((_ : Reader.t), outer_writer) =
  let pipe_r, pipe_w = pipe tls_session in
  let%bind inner_reader = Reader.of_pipe (Info.of_string "tls_reader") pipe_r in
  let%map inner_writer, `Closed_and_flushed_downstream inner_cafd =
    Writer.of_pipe (Info.of_string "tls_writer") pipe_w
  in
  Writer.set_raise_when_consumer_leaves inner_writer false;
  let outer_cafd =
    (* Ordering is important here to ensure no data is lost during the session shutdown *)
    let%bind () = Writer.close_finished inner_writer in
    let%bind () = inner_cafd in
    let%bind () = try_to_close tls_session in
    Writer.flushed outer_writer
  in
  tls_session, inner_reader, inner_writer, `Tls_closed_and_flushed_downstream outer_cafd
;;
(* END IMPORT *)

module Tls_provider = struct
  type tls_config = Tls.Config.client
  let tls_config_key = Caqti_tls.Config.client

  let start_tls ~config ?host ((_outer_reader, outer_writer) as outer_rw) =
    (match%bind Tls_async.Session.client_of_fd config ?host outer_rw with
     | Error error ->
        return (Error (Caqti_error.Msg (Error.to_string_hum error)))
     | Ok session ->
        let%map _, inner_reader, inner_writer,
                `Tls_closed_and_flushed_downstream outer_cafd =
          upgrade_connection session outer_rw
        in
        don't_wait_for begin
          let%bind () = outer_cafd (* triggerd by closing inner_writer *) in
          let%bind () = Writer.close outer_writer in
          Reader.close inner_reader
        end;
        Ok (inner_reader, inner_writer))
end

let () = Caqti_async.System.Net.register_tls_provider (module Tls_provider)
