(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_describe
open Caqti_lwt
module Cal = CalendarLib

let field_separator = ref ","

let csv_quoted s =
  let buf = Buffer.create (String.length s + 2) in
  Buffer.add_char buf '"';
  String.iter
    (function '"' -> Buffer.add_string buf "\"\"" | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"';
  Buffer.contents buf;

module Connection_utils (C : CONNECTION) = struct
  let show_field qd i r =
    match snd qd.querydesc_fields.(i) with
    | `Bool -> if C.Tuple.bool i r then "true" else "false"
    | `Int -> string_of_int (C.Tuple.int i r)
    | `Float -> string_of_float (C.Tuple.float i r)
    | `String -> csv_quoted (C.Tuple.string i r)
    | `Bytes -> csv_quoted (C.Tuple.bytes i r)
    | `Date -> Cal.Printer.Date.to_string (C.Tuple.date i r)
    | `Utc -> Cal.Printer.Calendar.to_string (C.Tuple.utc i r)
    | `Other _ -> C.Tuple.other i r
    | `Unknown -> failwith "Cannot determine field type."
end

let main do_describe uri qs =
  let q = Caqti_query.prepare_any qs in
  let%lwt connection = Caqti_lwt.connect uri in
  let module C = (val connection) in
  let module U = Connection_utils (C) in
  let describe =
    (match C.describe with
     | None -> failwith "The database does not support describe."
     | Some describe -> describe) in
  let%lwt qd = describe q in
  let n = Array.length qd.querydesc_fields in
  let print_tuple r =
    assert (n > 0);
    Lwt_io.print (U.show_field qd 0 r) >>
    for%lwt i = 1 to n - 1 do
      Lwt_io.print !field_separator >>
      Lwt_io.print (U.show_field qd i r)
    done >>
    Lwt_io.print "\n" in
  if do_describe then
    for%lwt i = 0 to n - 1 do
      let name, tdesc = qd.querydesc_fields.(i) in
      Lwt_io.printf "%s : %s\n" name (string_of_typedesc tdesc)
    done
  else
    C.iter_s q print_tuple [||]

let () =
  let arg_desc = ref false in
  let arg_u = ref None in
  let arg_q = ref None in
  let arg_specs = Arg.align [
    "-d", Arg.Unit (fun () -> arg_desc := true),
      " Print the type of the tuples returned by the query.";
    "-u", Arg.String (fun u -> arg_u := Some (Uri.of_string u)),
      "URI Connection URI.";
    "-q", Arg.String (fun q -> arg_q := Some q),
      "QUERY The query string.";
  ] in
  let usage_msg = Sys.argv.(0) ^ " -u URI -q QUERY" in
  Arg.parse arg_specs
    (fun _ -> raise (Arg.Bad "No positional arguments expected."))
    usage_msg;
  let required r =
    match !r with
    | None -> Arg.usage arg_specs usage_msg; exit 64
    | Some x -> x in
  let uri = required arg_u in
  let qs = required arg_q in
  Lwt_main.run (main !arg_desc uri qs)
