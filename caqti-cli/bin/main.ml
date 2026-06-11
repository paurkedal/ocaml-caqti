(* Copyright (C) 2026  Petter A. Urkedal <paurkedal@gmail.com>
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

open Cmdliner
open Cmdliner.Term.Syntax
open Lwt.Infix

(* Helpers *)

let log_src = Logs.Src.create "caqti-cli"
module Log = (val Logs.src_log log_src)
module Log_lwt = (val Logs_lwt.src_log log_src)
let print_endline s = Log.app (fun p -> p "%s" s)

let rec iter_s_result_list f = function
 | [] -> Lwt.return_ok ()
 | x :: xs -> Lwt_result.bind (f x) (fun () -> iter_s_result_list f xs)

let extract_error = Result.map_error @@ function
 | #Caqti.Error.t as err -> Caqti.Error.show err
 | `Msg msg -> msg

let query_conv =
  let parser s =
    Caqti.Template.Query.parse_result s
    |> Result.map_error Caqti.Template.Query.Parse_error.show
  in
  let pp = Caqti.Template.Query.pp in
  Arg.Conv.make ~parser ~pp ~docv:"QUERY" ()

let row_type_conv =
  let open Caqti.Template in
  let exception Invalid of string in
  let parser s =
    let add ft_str (Row_type.Any acc) =
      try
        let Field_type.Any ft = Field_type.of_string_exn ft_str in
        Row_type.(Any (t2 (field ft) acc))
      with Failure _ -> raise (Invalid ft_str)
    in
    try
      Ok (List.fold_right add (String.split_on_char ',' s) Row_type.(Any unit))
    with Invalid ft ->
      Error ("Invalid field type " ^ ft)
  in
  let pp = Row_type.pp_any in
  Arg.Conv.make ~parser ~pp ~docv:"ROW-TYPE" ()

(* Commands *)

let plugin_list =
  let+ () = Term.const () in
  List.iter print_endline (Caqti_plugin.available ());
  Ok ()

let plugin_tryload =
  let+ driver =
    let docv = "DRIVER" in
    Arg.(required @@ pos 0 (some string) None @@ info ~docv [])
  in
  Caqti_plugin.load driver;
  Log.info (fun p -> p "Driver loaded successfully.");
  Ok ()

let exec =
  let+ uri =
    let docv = "URI" in
    Arg.(required @@ opt (some string) None @@ info ~docv ["u"])
  and+ Any row_type =
    let default = Caqti.Template.Row_type.(Any unit) in
    Arg.(value @@ opt row_type_conv default @@ info ["r"])
  and+ queries =
    Arg.(value @@ opt_all query_conv [] @@ info ["c"])
  in
  let print_row value =
    let pp_row = Caqti.Template.Row.pp row_type in
    Log_lwt.app (fun p -> p "%a@." pp_row value) >|= Result.ok
  in
  Caqti_lwt_unix.with_connection (Uri.of_string uri)
    begin fun (module C) ->
      Log_lwt.info (fun p -> p "Connection established.") >>= fun () ->
      iter_s_result_list
        begin fun query ->
          Log_lwt.info (fun p -> p "Exec: %a." Caqti.Template.Query.pp query)
            >>= fun () ->
          let req =
            let open Caqti.Templater in
            direct_gen T.(unit -->* row_type) (fun _ -> query)
          in
          C.iter_s req print_row ()
        end
        queries
    end
  >|= extract_error |> Lwt_main.run

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level ~all:true (Some Logs.Info);
  exit @@ Cmd.eval_result @@ Cmd.group (Cmd.info "caqti") [
    Cmd.group (Cmd.info "plugin") [
      Cmd.make (Cmd.info "list") plugin_list;
      Cmd.make (Cmd.info "try-load") plugin_tryload;
    ];
    Cmd.make (Cmd.info "exec") exec;
  ]
