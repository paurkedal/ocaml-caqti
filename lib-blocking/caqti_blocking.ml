(* Copyright (C) 2018--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_common_priv

module System = struct

  type 'a future = 'a
  let (>>=) x f = f x
  let (>|=) x f = f x
  let return x = x
  let join (_ : unit list) = ()

  module Mvar = struct
    type 'a t = 'a option ref
    let create () = ref None
    let store x v = v := Some x
    let fetch v =
      (match !v with
       | None -> failwith "Attempt to fetch empty mvar from blocking client."
       | Some x -> x)
  end

  module Log = struct
    type 'a log = 'a Logs.log
    let err ?(src = default_log_src) = Logs.err ~src
    let warn ?(src = default_log_src) = Logs.warn ~src
    let info ?(src = default_log_src) = Logs.info ~src
    let debug ?(src = default_log_src) = Logs.debug ~src
  end

  module Unix = struct
    type file_descr = Unix.file_descr
    let wrap_fd f fd = f fd
    let poll ?(read = false) ?(write = false) ?(timeout = -1.0) fd =
      let read_fds = if read then [fd] else [] in
      let write_fds = if write then [fd] else [] in
      let read_fds, write_fds, _ = Unix.select read_fds write_fds [] timeout in
      (read_fds <> [], write_fds <> [], read_fds = [] && write_fds = [])
  end

  module Preemptive = struct
    let detach f x = f x
    let run_in_main f = f ()
  end

  module Stream = Caqti_stream.Make (struct
    type 'a future = 'a
    let (>>=) x f = f x
    let (>|=) x f = f x
    let return x = x
  end)
end

type 'a future = 'a
include Caqti_connect.Make_unix (System)

let or_fail = function
 | Ok x -> x
 | Error (#Caqti_error.t as err) -> raise (Caqti_error.Exn err)
