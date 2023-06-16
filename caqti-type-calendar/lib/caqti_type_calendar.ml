(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open CalendarLib

let msg_cdate_encode = "Failed to convert CalendarLib.Date.t to Ptime.t"
let msg_cdate_decode = "Failed to convert Ptime.t to CalendarLib.Date.t"
let msg_ctime_encode = "Failed to convert CalendarLib.Calendar.t to Ptime.t"
let msg_ctime_decode = "Failed to convert Ptime.t to CalendarLib.Calendar.t"

let cdate =
  let encode date =
    (match Ptime.of_float_s (Date.to_unixfloat date) with
     | Some t -> t
     | None -> raise (Caqti_type.Reject msg_cdate_encode))
  in
  let decode pdate =
    (try Date.from_unixfloat (Ptime.to_float_s pdate) with
     | Unix.Unix_error _ -> raise (Caqti_type.Reject msg_cdate_decode))
  in
  Caqti_type.(product decode @@ proj pdate encode @@ proj_end)

let ctime =
  let encode time =
    (match Ptime.of_float_s (Calendar.to_unixfloat time) with
     | Some t -> t
     | None -> raise (Caqti_type.Reject msg_ctime_encode))
  in
  let decode ptime =
    (try Calendar.from_unixfloat (Ptime.to_float_s ptime) with
     | Unix.Unix_error _ -> raise (Caqti_type.Reject msg_ctime_decode))
  in
  Caqti_type.(product decode @@ proj ptime encode @@ proj_end)
