(* Copyright (C) 2017--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

[@@@ocaml.warning "-3"]

open Caqti_prereq

module System = struct

  type 'a io = 'a Lwt.t
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
  let fail = Lwt.fail
  let return = Lwt.return
  let join = Lwt.join
  let catch = Lwt.catch

  module Mvar = struct
    type 'a t = 'a Lwt_mvar.t
    let create = Lwt_mvar.create_empty
    let store x v = Lwt.async (fun () -> Lwt_mvar.put v x)
    let fetch = Lwt_mvar.take
  end

  module Log = struct
    open Lwt_log

    let section = Lwt_log.Section.make "caqti"
    let qnr_section = Lwt_log.Section.make "caqti.qnr"

    let error_f   fmt = Lwt_log.error_f ~section fmt
    let warning_f fmt = Lwt_log.warning_f ~section fmt
    let info_f    fmt = Lwt_log.info_f ~section fmt
    let debug_f   fmt = Lwt_log.debug_f ~section fmt

    let debug_query_enabled () =
      match Lwt_log.Section.level qnr_section with
      | Debug | Info -> true
      | Notice | Warning | Error | Fatal -> false

    let debug_tuple_enabled () =
      match Lwt_log.Section.level qnr_section with
      | Debug -> true
      | Info | Notice | Warning | Error | Fatal -> false

    let debug_query qi params =
      begin match qi with
      | `Oneshot qs ->
        Lwt_log.debug_f ~section:qnr_section "Sent query: %s" qs
      | `Prepared (qn, qs) ->
        Lwt_log.debug_f ~section:qnr_section "Sent query %s: %s" qn qs
      end >>= fun () ->
      if params = [] then
        Lwt.return_unit
      else
        Lwt_log.debug_f ~section:qnr_section "with parameters: %s"
                        (String.concat ", " params)

    let debug_tuple tuple =
      Lwt_log.debug_f ~section:qnr_section "Received tuple: %s"
                      (String.concat ", " tuple)
  end

  module Unix = struct
    type file_descr = Lwt_unix.file_descr

    let wrap_fd f fd = f (Lwt_unix.of_unix_file_descr fd)

    let poll ?(read = false) ?(write = false) ?timeout fd =
      let choices =
        [] |> (fun acc -> if read then Lwt_unix.wait_read fd :: acc else acc)
           |> (fun acc -> if write then Lwt_unix.wait_write fd :: acc else acc)
           |> Option.fold (fun t acc -> Lwt_unix.timeout t :: acc) timeout in
      if choices = [] then
        Lwt.fail_invalid_arg "Caqti_lwt.Unix.poll: No operation specified."
      else
        begin
          Lwt.catch
            (fun () -> Lwt.choose choices >|= fun _ -> false)
            (function
             | Lwt_unix.Timeout -> Lwt.return_true
             | exn -> Lwt.fail exn)
        end >>= fun timed_out ->
        Lwt.return (Lwt_unix.readable fd, Lwt_unix.writable fd, timed_out)
  end

  module Preemptive = Lwt_preemptive

end

include Caqti1_connect.Make (System)
