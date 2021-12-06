(* Copyright (C) 2015--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

let load_uris () =
  (match open_in "uris.conf" with
   | exception Sys_error _ ->
      failwith "No -u option provided and missing tests/uris.conf."
   | ic ->
      let rec loop acc =
        (match input_line ic with
         | exception End_of_file -> close_in ic; List.rev acc
         | ln ->
            let uri = String.trim ln in
            if String.length uri = 0 || uri.[0] = '#' then
              loop acc
            else
              loop (Uri.of_string uri :: acc)) in
      loop [])

let common_args =
  let open Cmdliner in
  let uri =
    let doc = "Database URI(s) to be used for conducting the tests." in
    Arg.(value @@ opt_all string [] @@ info ~doc ["u"])
  in
  let log_level =
    let doc = "Log level." in
    let conv' =
      let pp ppf level =
        Format.pp_print_string ppf (Logs.level_to_string level)
      in
      Arg.conv (Logs.level_of_string, pp)
    in
    let env = Cmdliner.Term.env_info "CAQTI_TEST_LOG_LEVEL" in
    Arg.(value @@ opt conv' (Some Logs.Info) @@ info ~doc ~env ["log-level"])
  in
  let preprocess log_level uris =
    Logs.set_level log_level;
    (match uris with
     | [] -> load_uris ()
     | uris -> List.map Uri.of_string uris)
  in
  Term.(const preprocess $ log_level $ uri)

let test_name_of_uri uri =
  (match Uri.scheme uri with
   | Some scheme -> scheme
   | None -> Uri.to_string uri)

let init_list n f = (* List.init is available from OCaml 4.6.0 *)
 let rec loop acc i = if i < 0 then acc else loop (f i :: acc) (i - 1) in
 loop [] (n - 1)

module Make_alcotest_cli
  (Platform : Alcotest_engine.Platform.MAKER)
  (Monad : Alcotest_engine.Monad.S) =
struct
  include Alcotest_engine.V1.Cli.Make (Platform) (Monad)

  let test_case_sync name speed f =
    test_case name speed (fun x -> Monad.return (f x))

  let run_with_args_dependency ?argv name term make_tests =
    let tests =
      (match Cmdliner.Term.eval_peek_opts ~version_opt:true ?argv term with
       | Some arg, _ -> make_tests arg
       | _ -> [])
    in
    run_with_args ?argv name Cmdliner.Term.(const ignore $ term) tests
end

let () =
  Random.self_init ();
  Logs.set_reporter (Logs.format_reporter ());

  (* Needed for bytecode since plugins link against C libraries: *)
  Dynlink.allow_unsafe_modules true
