(* Copyright (C) 2015--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

module type MONAD = sig
  type +'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module type S = sig
  type +'a future

  val read_sql_statement :
    ('a -> char option future) -> 'a -> string option future
end

module Make (Io : MONAD) = struct
  open Io

  let read_sql_statement read_char ic =
    let buf = Buffer.create 256 in
    let finish () =
      match String.trim (Buffer.contents buf) with
      | "" -> return None
      | s -> return (Some s) in
    let rec scan level = function
      | Some ';' when level = 0 -> finish ()
      | Some '\'' -> Buffer.add_char buf '\''; read_char ic >>= scan_quote level
      | Some '-' ->
        read_char ic >>=
          begin function
          | Some '-' -> read_char ic >>= scan_comment level
          | c_opt -> Buffer.add_char buf '-'; scan level c_opt
          end
      | Some '(' -> Buffer.add_char buf '('; read_char ic >>= scan (succ level)
      | Some ')' -> Buffer.add_char buf ')'; read_char ic >>= scan (pred level)
      | Some c -> Buffer.add_char buf c; read_char ic >>= scan level
      | None -> finish ()
    and scan_quote level = function
      | Some '\'' -> Buffer.add_char buf '\''; read_char ic >>= scan level
      | Some c -> Buffer.add_char buf c; read_char ic >>= scan_quote level
      | None -> finish ()
    and scan_comment level = function
      | Some '\n' -> read_char ic >>= scan level
      | Some _ -> read_char ic >>= scan_comment level
      | None -> finish () in
    read_char ic >>= scan 0

end
