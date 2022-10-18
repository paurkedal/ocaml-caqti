(* Copyright (C) 2019--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Printf

let datetuple_of_iso8601 s =
  if String.length s = 10 && s.[4] = '-' && s.[7] = '-' then
    try
      (int_of_string (String.sub s 0 4),
       int_of_string (String.sub s 5 2),
       int_of_string (String.sub s 8 2))
    with Failure _ ->
      failwith "Caqti_private.datetuple_of_iso8601"
  else
    failwith "Caqti_private.datetuple_of_iso8601"

let iso8601_of_datetuple (y, m, d) =
  sprintf "%04d-%02d-%02d" y m d

let string_of_rfc3339_error ~input err =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Ptime.pp_rfc3339_error ppf err;
  Format.fprintf ppf " in value %S." input;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let ptime_of_rfc3339_utc s =
  let n = String.length s in
  let s' =
    if n < 13 then s else
    if s.[n - 1] = 'Z' then s else
    if s.[n - 3] = '+' || s.[n - 3] = '-' then s ^ ":00" else
    if s.[n - 6] = '+' || s.[n - 6] = '-' then s else
    s ^ "Z"
  in
  (match Ptime.of_rfc3339 s' with
   | Ok (t, _, _) -> Ok t
   | Error (`RFC3339 (_, err)) ->
      Error (string_of_rfc3339_error ~input:s' err))

let pdate_of_iso8601 s =
  (match Ptime.of_date (datetuple_of_iso8601 s) with
   | exception Failure _ ->
      Error (sprintf "Cannot parse date %S." s)
   | None ->
      Error (sprintf "Date %s is out of range." s)
   | Some pdate -> Ok pdate)

let iso8601_of_pdate x = iso8601_of_datetuple (Ptime.to_date x)
