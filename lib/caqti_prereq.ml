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

open Printf

let finally cleanup thunk =
  let r = try thunk () with xc -> cleanup (); raise xc in
  cleanup (); r

let datetuple_of_iso8601 s =
  if String.length s = 10 && s.[4] = '-' && s.[7] = '-' then
    (int_of_string (String.sub s 0 4),
     int_of_string (String.sub s 5 2),
     int_of_string (String.sub s 8 2))
  else
    failwith "Caqti_internal.datetuple_of_iso8601"

let iso8601_of_datetuple (y, m, d) =
  sprintf "%04d-%02d-%02d" y m d
