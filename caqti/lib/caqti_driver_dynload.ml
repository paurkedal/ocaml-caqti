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

let dynload_library = ref @@ fun lib ->
  Error
    (Printf.sprintf
      "Neither %s nor the dynamic linker is linked into the application." lib)

let define_loader f = dynload_library := f

let load_library name = !dynload_library name

let scheme_driver_name = function
 | "postgres" | "postgresql" -> "caqti-driver-postgresql"
 | s -> "caqti-driver-" ^ s

let retry_with_library load_driver ~uri scheme =
  (match load_driver ~uri scheme with
   | Error (`Load_failed _) ->
      let driver_name = scheme_driver_name scheme in
      (match load_library driver_name with
       | Ok () ->
          (match load_driver ~uri scheme with
           | Error (`Load_failed _) ->
              let msg = Printf.sprintf
                "The %s did not register a handler for the URI scheme %s."
                driver_name scheme
              in
              Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg))
           | r -> r)
       | Error msg ->
          Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg)))
   | r -> r)
