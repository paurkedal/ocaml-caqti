(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

let ident x = x

let (%) g f x = g (f x)
let (%>) f g x = g (f x)
let (%>?) f g x = match f x with Ok y -> g y | Error _ as r -> r
let (|>?) r f = match r with Ok x -> f x | Error _ as r -> r

let rec ncompose n f acc = if n = 0 then acc else ncompose (n - 1) f (f acc)

module Option = struct
  type 'a t = 'a option
  let fold f = function None -> ident | Some x -> f x
end

module Result = struct
  let map f = function Ok x -> Ok (f x) | Error e -> Error e
end

module List = struct
  include List

  let rec fold f = function
   | [] -> fun acc -> acc
   | x :: xs -> f x %> fold f xs

  let rec fold_r f = function
   | [] -> fun acc -> Ok acc
   | x :: xs -> f x %>? fold_r f xs

  let rec iter_r f = function
   | [] -> Ok ()
   | x :: xs -> (match f x with Ok () -> iter_r f xs | Error _ as r -> r)
end

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

let string_of_rfc3339_error ~input err =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  Ptime.pp_rfc3339_error ppf err;
  Format.fprintf ppf " in value %S." input;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let ptime_of_rfc3339_utc s =
  let n = String.length s in
  let s =
    if n < 13 then s else
    if s.[n - 1] = 'Z' then s else
    if s.[n - 3] = '+' || s.[n - 3] = '-' then s ^ ":00" else
    if s.[n - 6] = '+' || s.[n - 6] = '-' then s else
    s ^ "Z" in
  (match Ptime.of_rfc3339 s with
   | Ok (t, (None | Some 0), _) -> Ok t
   | Ok (_, Some _, _) ->
      Error "Non-UTC time."
   | Error (`RFC3339 (_, err)) ->
      Error (string_of_rfc3339_error ~input:s err))

let pdate_of_iso8601 s =
  (match Ptime.of_date (datetuple_of_iso8601 s) with
   | exception Failure _ ->
      Error (sprintf "Cannot parse date %S." s)
   | None ->
      Error (sprintf "Date %s is out of range." s)
   | Some pdate -> Ok pdate)

let iso8601_of_pdate x = iso8601_of_datetuple (Ptime.to_date x)

let default_log_src = Logs.Src.create "caqti"
