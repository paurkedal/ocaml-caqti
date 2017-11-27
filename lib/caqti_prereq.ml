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

open Printf

let ident x = x

let (%) g f x = g (f x)
let (%>) f g x = g (f x)
let (%>?) f g x = match f x with Ok y -> g y | Error _ as r -> r
let (|>?) r f = match r with Ok x -> f x | Error _ as r -> r

module Option = struct
  type 'a t = 'a option
  let fold f = function None -> ident | Some x -> f x
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
  (match Ptime.of_rfc3339 s with
   | Ok (t, (None | Some 0), _) -> Ok t
   | Ok (_, Some _, _) ->
      Error "Non-UTC time."
   | Error (`RFC3339 (_, err)) ->
      Error (string_of_rfc3339_error ~input:s err))

let pday_of_iso8601 s =
  (* TODO: Improve parsing. *)
  (match ptime_of_rfc3339_utc (s ^ "T00:00:00Z") with
   | Ok t -> Ok (fst (Ptime.Span.to_d_ps (Ptime.to_span t)))
   | Error _ as r -> r)

let iso8601_of_pday x =
  (match Ptime.Span.of_d_ps (x, 0L) with
   | None -> Error "POSIX date out of range."
   | Some span ->
      (match Ptime.of_span span with
       | None -> Error "POSIX date out of range."
       | Some t ->
          let (y, m, d) = Ptime.to_date t in
          Ok (sprintf "%04d-%02d-%02d" y m d)))
