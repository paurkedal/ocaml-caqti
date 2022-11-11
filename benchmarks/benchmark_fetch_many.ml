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


open Bechamel
open Bechamel.Toolkit

let connect_uri = Uri.of_string "postgresql://"

let fetch_many_request =
  let open Caqti_request.Infix in
  let open Caqti_type.Std in
  unit -->* tup3 int int (tup3 float bool bool) @:- {|
    WITH tmp (i) AS (VALUES (0), (1), (2), (3), (4), (5), (6), (7), (8), (9))
    SELECT a.i, b.i, CAST(c.i AS float), e.i < c.i, e.i < d.i
    FROM tmp a, tmp b, tmp c, tmp d, tmp e
  |}

module type PLATFORM = sig
  type +'a future
  val name : string
  val run : 'a future -> 'a
  val (>>=) : 'a future -> ('a -> 'b future) -> 'b future
  val (>|=) : 'a future -> ('a -> 'b) -> 'b future
  val or_fail : ('a, [< Caqti_error.t]) result -> 'a future
  include Caqti_connect_sig.S with type 'a future := 'a future
end

module Make (P : PLATFORM) = struct
  open P

  let test' _ = Staged.stage @@ fun (module C : CONNECTION) ->
    run begin
      let aux (_, _, (_, _, _)) = succ in
      C.fold fetch_many_request aux () 0 >>= or_fail >|= fun count ->
      assert (count = 100_000)
    end

  let test uris =
    let allocate i = run (connect (List.nth uris i) >>= or_fail) in
    let free (module C : CONNECTION) = run (C.disconnect ()) in
    Test.(make_indexed_with_resource uniq)
      ~name ~args:(List.mapi (fun i _ -> i) uris) ~allocate ~free test'

end

module Test_blocking = Make (struct
  let name = "blocking"
  type 'a future = 'a
  let run x = x
  let (>>=) x f = f x
  let (>|=) x f = f x
  include Caqti_blocking
end)

module Test_lwt = Make (struct
  let name = "lwt-unix"
  type 'a future = 'a Lwt.t
  let run = Lwt_main.run
  include Lwt.Infix
  include Caqti_lwt
end)

(* TODO: Can we run async under a benchamel test? *)

let test uris = Test.make_grouped ~name:"retrieve" [
  Test_blocking.test uris;
  Test_lwt.test uris;
]

(* Benchmark Function *)

let benchmark uris =
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[|run|]
  in
  let instances =
    Instance.[minor_allocated; major_allocated; monotonic_clock]
  in
  let cfg =
    (* TODO: Why does this segfault with ~kde:(Some 1000)? *)
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 1.0) ()
  in
  let raw_results = Benchmark.all cfg instances (test uris) in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  let results = Analyze.merge ols instances results in
  (results, raw_results)

(* TTY Boilerplate *)

let () =
  List.iter
    (fun v -> Bechamel_notty.Unit.add v (Measure.unit v))
    Instance.[minor_allocated; major_allocated; monotonic_clock]

let img (window, results) =
  Bechamel_notty.Multiple.image_of_ols_results ~rect:window
    ~predictor:Measure.run results

open Notty_unix

let main {Testlib.uris; _} =
  let window =
    match winsize Unix.stdout with
    | Some (w, h) -> {Bechamel_notty.w; h}
    | None -> {Bechamel_notty.w = 80; h = 1}
  in
  let results, _ = benchmark uris in
  img (window, results) |> eol |> output_image

let main_cmd =
  let open Cmdliner in
  let term = Term.(const main $ Testlib.common_args) in
  Cmd.v (Cmd.info "benchmark_retrieve") term

let () = exit (Cmdliner.Cmd.eval main_cmd)
