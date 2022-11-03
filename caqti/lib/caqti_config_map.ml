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

open Sexplib0

let (%) f g x = f (g x)

module Conv = struct
  open Sexp_conv

  type 'a t = {
    name: string;
    of_sexps: Sexp.t list -> 'a;
    sexps_of: 'a -> Sexp.t list;
  }

  exception Wrong_number_of_arguments

  let create_with_args ~of_sexps ~sexps_of name =
    {name; of_sexps; sexps_of}

  let create ~of_sexp ~sexp_of name =
    let of_sexps = function
     | [arg] -> of_sexp arg
     | _ -> raise Wrong_number_of_arguments
    in
    let sexps_of v = [sexp_of v] in
    {name; of_sexps; sexps_of}

  let create_atomic ~of_string ~to_string name =
    let of_sexp sexp =
      let str = Sexp_conv.string_of_sexp sexp in
      try of_string str with
       | Failure msg -> of_sexp_error msg sexp
    in
    let sexp_of x = Sexp.Atom (to_string x) in
    create ~of_sexp ~sexp_of name

  let bool = create ~of_sexp:bool_of_sexp ~sexp_of:sexp_of_bool "bool"
  let int = create ~of_sexp:int_of_sexp ~sexp_of:sexp_of_int "int"
  let float = create ~of_sexp:float_of_sexp ~sexp_of:sexp_of_float "float"
  let string = create ~of_sexp:string_of_sexp ~sexp_of:sexp_of_string "string"

  let enum name cases =
    let error_msg =
      Printf.sprintf "expected: %s" (String.concat " | " (List.map fst cases))
    in
    let of_sexp sexp =
      (match List.assoc_opt (string_of_sexp sexp) cases with
       | Some v -> v
       | None -> of_sexp_error error_msg sexp)
    in
    let sexp_of v =
      sexp_of_string (List.find (fun (_, v') -> v = v') cases |> fst)
    in
    create ~of_sexp ~sexp_of name

  let minor_version =
    let error_msg = "expecting an atom of the form <int>.<int>" in
    let of_sexp sexp =
      (match String.split_on_char '.' (string_of_sexp sexp) with
       | [i; j] ->
          (try (int_of_string i, int_of_string j) with
           | Failure _ -> of_sexp_error error_msg sexp)
       | _ -> of_sexp_error error_msg sexp)
    in
    let sexp_of (i, j) = Sexp.Atom (Printf.sprintf "%d.%d" i j) in
    create ~of_sexp ~sexp_of "minor-version"
end

module Key_info = struct
  type 'a t = {
    name: string;
    conv: 'a Conv.t;
  }
end

module H = Hmap.Make (Key_info)

module Key = struct

  type (+'a, 'b) t = 'b H.key

  type +'a any = Any : ('a, 'b) t -> 'a any

  let name key = (H.Key.info key).name

  let conv key = (H.Key.info key).conv

  let create conv name = H.Key.create Key_info.{name; conv}
end

module Key_set = struct
  module M = Map.Make (String)

  type 'a t = 'a Key.any M.t

  let empty = M.empty

  let add (Key.Any k as k') = M.add (Key.name k) k'

  let find = M.find_opt

  let fold f m acc = M.fold (fun _ k -> f k) m acc
end

type 'a driver_id = ..

type 'a binding = B : ('a, 'b) Key.t * 'b -> 'a binding

let binding_of_sexp keys sexp =
  let open Sexp_conv in
  let key_name, args =
    (match sexp with
     | Sexp.Atom _ | Sexp.List [] ->
        of_sexp_error "setting must be a non-empty list" sexp
     | Sexp.List (key_name :: args) -> (string_of_sexp key_name, args))
  in
  if key_name = "driver" then None else
  (match Key_set.find key_name keys with
   | None ->
      let msg = Printf.sprintf "Unknown configuration key %s." key_name in
      of_sexp_error msg sexp
   | Some (Key.Any key) ->
      try
        let value = (H.Key.info key).conv.of_sexps args in
        Some (B (key, value))
      with Conv.Wrong_number_of_arguments ->
        of_sexp_error "wrong number of arguments" sexp)

let sexp_of_binding (H.B (key, value)) =
  let key_info = H.Key.info key in
  Sexp.List (Sexp.Atom key_info.name :: key_info.conv.sexps_of value)

type _ t =
  | Generic : H.t -> [`Generic] t
  | Specific : H.t * 'a driver_id -> [`Generic | `Specific of 'a] t

let map_hmap (type a) f : a t -> a t = function
 | Generic m -> Generic (f m)
 | Specific (m, d) -> Specific (f m, d)

let get_hmap (type a) : a t -> H.t = function
 | Generic m -> m
 | Specific (m, _) -> m

type any = Any : 'a t -> any

type any_specific =
  | Any_specific : [`Generic | `Specific of 'a] t -> any_specific

module Driver = struct
  type 'a id = 'a driver_id = ..

  type reg =
    | Reg : {
        id: 'd id;
        name: string;
        keys: [> `Specific of 'd] Key_set.t;
        add_uri:
          (Uri.t ->
           [`Generic | `Specific of 'd] t ->
           ([`Generic | `Specific of 'd] t, [< Caqti_error.preconnect]) result);
      } -> reg

  let reg_by_name : (string, reg) Hashtbl.t = Hashtbl.create 5
  let reg_by_ckey = Hashtbl.create 5

  let ckey driver_id = Obj.Extension_constructor.(name (of_val driver_id))

  let register ~name ~keys ~add_uri id =
    let reg = Reg {id; name; keys; add_uri} in
    Hashtbl.add reg_by_name name reg;
    Hashtbl.add reg_by_ckey (ckey id) reg

  let name id =
    (match Hashtbl.find_opt reg_by_ckey (ckey id) with
     | Some (Reg {name; _}) -> name
     | None -> assert false)

  let find =
    Caqti_driver_dynload.retry_with_library begin fun ~uri scheme ->
      (match Hashtbl.find_opt reg_by_name scheme with
       | Some d -> Ok d
       | None ->
          let msg =
            Printf.sprintf "Cannot find configuration keys for %s." scheme in
          Error (Caqti_error.load_failed ~uri (Caqti_error.Msg msg)))
    end
end

let empty = Generic H.empty

let add k v = map_hmap (H.add k v)

let add_default k v c =
  (match H.find k (get_hmap c) with None -> add k v c | Some _ -> c)

let add_driver d : [`Generic] t -> [`Generic | `Specific of _] t = function
 | Generic m -> Specific (m, d)

let add_uri uri : [`Generic] t -> (any_specific, _) result = function
 | Generic m ->
    (match Uri.scheme uri with
     | None ->
        let msg = "Missing URI scheme." in
        Error (Caqti_error.load_rejected ~uri (Caqti_error.Msg msg))
     | Some scheme ->
        (match Driver.find ~uri scheme with
         | Ok (Reg {id; add_uri; _}) ->
            (match add_uri uri (Specific (m, id)) with
             | Ok c -> Ok (Any_specific c)
             | Error (#Caqti_error.preconnect as err) -> Error err)
         | Error _ as r -> r))

let as_default : [`Generic | `Specific of _] t -> [`Generic] t = function
 | Specific (m, _) -> Generic m

let find_driver : [`Generic | `Specific of _] t -> _ = function
 | Specific (_, d) -> d

let remove k = map_hmap (H.rem k)

let update k f = map_hmap @@ fun m ->
  (match f (H.find k m) with None -> H.rem k m | Some v -> H.add k v m)

let find k c = H.find k (get_hmap c)

let fold f = H.fold (function H.B (k, v) -> f (B (k, v))) % get_hmap

let right_union cL = map_hmap @@ fun mR ->
  H.fold (function H.B (k, v) -> H.add k v) mR (get_hmap cL)

let sexp_of_t (type a) (c : a t) =
  let sexp_of_driver d = Sexp.(List [Atom "driver"; Atom (Driver.name d)]) in
  let settings_of_hmap m = []
    |> H.fold (List.cons % sexp_of_binding) m
    |> List.rev
  in
  let settings =
    (match c with
     | Generic m -> settings_of_hmap m
     | Specific (m, d) -> sexp_of_driver d :: settings_of_hmap m)
  in
  Sexp.List (Sexp.Atom "connection" :: settings)

let config_uri = Uri.make ~scheme:"config" ()

let extract_driver = function
 | Sexp.List [Sexp.Atom "driver"; Sexp.Atom name] as sexp ->
    (match Driver.find ~uri:config_uri name with
     | Ok driver_id -> Some driver_id
     | Error err -> Sexp_conv.of_sexp_error (Caqti_error.show err) sexp)
 | Sexp.List (Sexp.Atom "driver" :: _) as sexp ->
    Sexp_conv.of_sexp_error "the driver settings takes a single argument" sexp
 | _ -> None

let any_of_sexp = function
 | Sexp.List (Sexp.Atom "connection" :: sexps) as sexp ->
    let driver = List.find_map extract_driver sexps in
    (match driver with
     | None ->
        Sexp_conv.of_sexp_error "a driver must be specified" sexp
     | Some (Driver.Reg {id; keys; _}) ->
        let bindings = List.filter_map (binding_of_sexp keys) sexps in
        let m =
          List.fold_left (fun acc (B (k, v)) -> H.add k v acc) H.empty bindings
        in
        Any (Specific (m, id)))
 | sexp ->
    Sexp_conv.of_sexp_error "(connection ...) expected" sexp
