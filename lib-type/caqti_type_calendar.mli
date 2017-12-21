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

(** Caqti date and time field type based on the calendar package. *)

type _ Caqti_type.field +=
  | Cdate : CalendarLib.Date.t Caqti_type.field
  | Ctime : CalendarLib.Calendar.t Caqti_type.field

val cdate : CalendarLib.Date.t Caqti_type.t
val ctime : CalendarLib.Calendar.t Caqti_type.t
