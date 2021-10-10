(* Copyright (C) 2019--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

#if OCAML_VERSION < (4, 7, 0)
module Float = struct
  let compare (x : float) (y : float) = compare x y
end
#endif

#if OCAML_VERSION < (4, 8, 0)
module Int = struct
  let equal (x : int) (y : int) = x = y
end
module Obj = struct
  include Obj
  module Extension_constructor = struct
    let t = extension_constructor
    let of_val = extension_constructor
    let name = extension_name
  end
end
#endif
