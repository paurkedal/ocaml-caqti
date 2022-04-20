(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_common_priv
open Printf

let bool_of_pgstring = function
 | "t" -> true
 | "f" -> false
 | _ -> invalid_arg "Pg_ext.bool_of_string"

let pgstring_of_bool = function true -> "t" | false -> "f"

let pdate_of_pgstring = pdate_of_iso8601

let pgstring_of_pdate x =
  Ptime.to_rfc3339 ~space:true ~tz_offset_s:0 ~frac_s:6 x

let pgstring_of_ptime_span t =
  let is_neg = Ptime.Span.compare t Ptime.Span.zero < 0 in
  let d, ps = Ptime.Span.(to_d_ps (abs t ))in
  let buf = Buffer.create 32 in
  if d <> 0 then bprintf buf "%d days " (if is_neg then -d else d);
  let s, ps = Int64.(div ps 1_000_000_000_000L |> to_int,
                     rem ps 1_000_000_000_000L) in
  let hour, s = s / 3600, s mod 3600 in
  let minute, s = s / 60, s mod 60 in
  if is_neg then Buffer.add_char buf '-';
  bprintf buf "%02d:%02d:%02d" hour minute s;
  if ps <> 0L then bprintf buf ".%06Ld" (Int64.div ps 1_000_000L);
  Buffer.contents buf

let ps_of_decimal s =
  try
    (match String.length s with
     | 2 ->
        Ok (Int64.(mul (of_string s) 1_000_000_000_000L))
     | n when n < 2 ->
        Error "Missing digits in seconds of interval string."
     | _ when s.[2] <> '.' ->
        Error "Expected period after seconds in interval string."
     | n ->
        let buf = Bytes.make 14 '0' in
        Bytes.blit_string s 0 buf 0 2;
        Bytes.blit_string s 3 buf 2 (min 12 (n - 3));
        Ok (Int64.of_string (Bytes.to_string buf)))
  with Failure _ ->
    Error "Seconds in interval string is not numeric."

let ps_of_hms s =
  (match String.split_on_char ':' s with
   | [hours; minutes; seconds] ->
      (try
        let hour = int_of_string hours in
        let minute = int_of_string minutes in
        (match ps_of_decimal seconds with
         | Ok ps ->
            let hm = Int64.of_int (abs hour * 60 + minute) in
            let ps = Int64.(add ps (mul hm 60_000_000_000_000L)) in
            Ok (if s.[0] = '-' then Int64.neg ps else ps)
         | Error msg -> Error msg)
       with Failure _ ->
        Error "Non-integer hour or minute in interval string.")
   | _ ->
      Error "Expected HH:MM:SS[.F] in interval string.")

let ptime_span_of_pgstring s =
  let span_of_d_ps (d, ps) =
    let d, ps =
      if Int64.compare ps Int64.zero >= 0
      then (d, ps)
      else (d - 1, Int64.add ps 86_400_000_000_000_000L) in
    (match Ptime.Span.of_d_ps (d, ps) with
     | Some t -> Ok t
     | None -> Error "Out for range for Ptime.span.") in
  try
    (match String.split_on_char ' ' s with
     | [d; "days"] ->
        span_of_d_ps (int_of_string d, 0L)
     | [d; "days"; hms] ->
        ps_of_hms hms |>? fun ps ->
        span_of_d_ps (int_of_string d, ps)
     | [hms] ->
        ps_of_hms hms |>? fun ps ->
        span_of_d_ps (0, ps)
     | _ ->
        Error "Unhandled interval format.")
  with Failure _ ->
    Error "Non-integer days in interval string."

let cause_of_sqlstate sqlstate =
  (match sqlstate.[0], sqlstate.[1], sqlstate with
   | '2', '3', "23001" -> `Restrict_violation
   | '2', '3', "23502" -> `Not_null_violation
   | '2', '3', "23503" -> `Foreign_key_violation
   | '2', '3', "23505" -> `Unique_violation
   | '2', '3', "23514" -> `Check_violation
   | '2', '3', "23P01" -> `Exclusion_violation
   | '2', '3', _ -> `Integrity_constraint_violation__don't_match
   | '5', '3', "53100" -> `Disk_full
   | '5', '3', "53200" -> `Out_of_memory
   | '5', '3', "53300" -> `Too_many_connections
   | '5', '3', "53400" -> `Configuration_limit_exceeded
   | '5', '3', _ -> `Insufficient_resources__don't_match
   | _ -> `Unspecified__don't_match)
