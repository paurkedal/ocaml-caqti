(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

let field_separator = ref ";"

let main uri qs =
  let q = Caqti_query.prepare_any qs in
  lwt connection = Caqti_lwt.connect uri in
  let module C = (val connection) in
  let print_tuple r =
    let n = C.Tuple.length r in
    assert (n > 0);
    (* FIXME: Misuse of Tuple.other. *)
    Lwt_io.print (C.Tuple.other 0 r) >>
    for_lwt i = 1 to n - 1 do
      Lwt_io.print !field_separator >>
      Lwt_io.print (C.Tuple.other i r)
    done >>
    Lwt_io.print "\n" in
  C.iter_s q print_tuple [||]

let () =
  let arg_u = ref None in
  let arg_q = ref None in
  let arg_specs = Arg.align [
    "-u", Arg.String (fun u -> arg_u := Some (Uri.of_string u)),
      " URI Connection URI.";
    "-q", Arg.String (fun q -> arg_q := Some q),
      " QUERY The query string.";
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
  Lwt_main.run (main uri qs)
