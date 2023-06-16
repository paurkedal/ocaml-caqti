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

let cdate =
  let name = "CalendarLib.Date.t" in
  let rep = Caqti_type.Field.Pdate in
  let encode date =
    (match Ptime.of_float_s (Date.to_unixfloat date) with
     | None -> Error "Ptime rejected POSIX date float from CalendarLib."
     | Some t -> Ok t)
  in
  let decode pdate =
    (try Ok (Date.from_unixfloat (Ptime.to_float_s pdate)) with
     | _ -> Error "CalendarLib rejected POSIX date float from Ptime.")
  in
  Caqti_type.field (Caqti_type.Field.Custom {name; rep; encode; decode})

let ctime =
  let name = "CalendarLib.Calendar.t" in
  let rep = Caqti_type.Field.Ptime in
  let encode time =
    (match Ptime.of_float_s (Calendar.to_unixfloat time) with
     | Some t -> Ok t
     | None -> Error "Failed to convert Calendar.t to Ptime.t")
  in
  let decode ptime =
    (try Ok (Calendar.from_unixfloat (Ptime.to_float_s ptime)) with
     | _ -> Error "CalendarLib rejected POSIX time float from Ptime.")
  in
  Caqti_type.field (Caqti_type.Field.Custom {name; rep; encode; decode})
