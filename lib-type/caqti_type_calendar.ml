(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

type _ Caqti_type.field +=
  | Cdate : CalendarLib.Date.t Caqti_type.field
  | Ctime : CalendarLib.Calendar.t Caqti_type.field

let () =
  let open Caqti_type.Field in
  let open CalendarLib in
  let get_coding : type a. _ -> a t -> a coding = fun _ -> function
   | Cdate ->
      let encode date =
        (match Ptime.of_float_s (Date.to_unixfloat date) with
         | None -> Error "Ptime rejected POSIX date float from CalendarLib."
         | Some t -> Ok t) in
      let decode pdate =
        (try Ok (Date.from_unixfloat (Ptime.to_float_s pdate)) with
         | _ -> Error "CalendarLib rejected POSIX date float from Ptime.") in
      Coding {rep = Caqti_type.Pdate; encode; decode}
   | Ctime ->
      let encode time =
        (match Ptime.of_float_s (Calendar.to_unixfloat time) with
         | Some t -> Ok t
         | None -> Error "Failed to convert Calendar.t to Ptime.t") in
      let decode ptime =
        (try Ok (Calendar.from_unixfloat (Ptime.to_float_s ptime)) with
         | _ -> Error "CalendarLib rejected POSIX time float from Ptime.") in
      Coding {rep = Caqti_type.Ptime; encode; decode}
   | _ -> assert false
  in
  define_coding Cdate {get_coding};
  define_coding Ctime {get_coding}

let cdate = Caqti_type.field Cdate
let ctime = Caqti_type.field Ctime
