(* Copyright (C) 2019--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

module Query = Caqti_query

module A = struct
  include Alcotest.V1

  let query = testable Query.pp (fun x y -> Query.(equal (normal x) (normal y)))

  let approx_query_string =
    let pp ppf x = Format.fprintf ppf "%S" x in
    let normalize =
      let re = Re.Pcre.regexp {|\$([0-9]|[A-Za-z0-9_]*\.)|} in
      let f g =
        let s = Re.Group.get g 1 in
        if s.[String.length s - 1] = '.' then "$(" ^ s ^ ")" else "?"
      in
      Re.replace re ~f
    in
    testable pp (fun x y -> String.equal (normalize x) (normalize y))
end

let random_letter () = Char.chr (Char.code 'a' + Random.int 26)

let rec random_query n =
  if n <= 1 then
    if Random.bool ()
      then Query.P (Random.int 8)
      else Query.L (String.init (Random.int 3) (fun _ -> random_letter ()))
  else
    Query.S (random_queries n)
and random_queries n =
  if n = 0 then [] else
  if Random.bool () then [random_query n] else
  let m = Random.int (n + 1) in
  random_queries m @ random_queries (n - m)

let test_show_and_hash_once () =
  let q1 = random_query (Random.int 8 + Random.int (1 lsl Random.int 8)) in
  let q2 = random_query (Random.int 8 + Random.int (1 lsl Random.int 8)) in
  let s1 = Query.show q1 in
  let s2 = Query.show q2 in
  if Query.equal q1 q2 then assert (Query.hash q1 = Query.hash q2);
  assert ((s1 = s2) = (Query.(equal (normal q1) (normal q2))))

let test_show_and_hash () =
  try
    for _ = 0 to 9999 do test_show_and_hash_once () done
  with Failure msg ->
    Printf.eprintf "%s\n" msg;
    exit 1

let random_query_string () =
  let random_char _ = Char.chr (0x20 + Random.int 0x60) in
  String.init (Random.int 128) random_char

let test_parse_special_cases () =
  let check_reject ~pos s =
    (match Caqti_query.of_string s with
     | Ok _ ->
        A.failf "Invalid expression %S accepted by parser." s
     | Error (`Invalid (pos', msg)) ->
        if pos' <> pos then
          A.failf "Position %d should be %d for error %S while parsing %s"
                  pos' pos msg s)
  in
  let check_normal' s =
    (match Caqti_query.of_string_exn s with
     | q ->
        A.(check string) "same" s (Caqti_query.show q)
     | exception Failure msg ->
        A.failf "Failed to parse %S: %s" s msg)
  in
  let check_normal s =
    check_normal' s;
    check_normal' (" " ^ s);
    check_normal' (s ^ " ")
  in
  let check_expect q s =
    A.(check query "same" q (Caqti_query.of_string_exn s))
  in
  check_reject ~pos:0 {|$0|}; check_reject ~pos:1 {|x$01|};
  check_reject ~pos:1 {|?0|}; check_reject ~pos:2 {|x?1x|};
  List.iter check_normal [
    {||}; {|a|}; {|ab|}; {| a b |};
    {|''|}; {|'a'|}; {|'''a''b'''|};
    {|""|}; {|"a"|}; {|"""a""b"""|};
    {|$(.)|}; {|$(a.)|}; {|$(ab.)|}; {|$(a)|}; {|$(ab)|};
    {|$$ a $(x.) $(y) b $$|};
    {|$$"$$|}; {|$$'$$|}; {|$QUOTE$ ' " $QUOTE$|};
    {|$QUOTE$ a $x. $. $( z) b ?0 $QUOTE $$QUOTE$|};
    (* Allowed by angstrom_parser_with_semicolon but not by angstrom_parser: *)
    {|a;b|};
  ];
  check_expect (S[L"SELECT "; P 0; L"::smallint"]) {|SELECT ?::smallint|};
  check_expect (S[L"$$ "; E"x"; L" $$"]) "$$ $(x) $$";
  check_expect (S[L"$Q$ $(x) $Q$"]) "$Q$ $(x) $Q$";
  check_expect (S[P 0; L" $$ ? $$ "; P 1; L" "; P 2]) "? $$ ? $$ ? ?"

let test_parse_random_strings () =
  let check_normal_or_exn s =
    (match Caqti_query.of_string s with
     | Ok q ->
        A.(check approx_query_string) "same" s (Caqti_query.show q)
     | Error (`Invalid (_, "Inconsistent parameter style.")) -> ()
     | Error (`Invalid (ok_len, _)) ->
        if ok_len > 0 && ok_len < String.length s then begin
          let s' = String.sub s 0 ok_len in
          (match Caqti_query.of_string s' with
           | Ok q ->
              A.(check approx_query_string) "same" s' (Caqti_query.show q)
           | Error (`Invalid (_, "Inconsistent parameter style.")) ->
              () (* only checked after successful parse *)
           | Error (`Invalid (pos, msg)) ->
              A.failf "Supposed valid substring [0, %d) of %S fails at %d: %s"
                ok_len s pos msg)
        end)
  in
  for _ = 1 to 50_000 do
    check_normal_or_exn (random_query_string ())
  done

let test_expand () =
  let env1 = function
   | "" -> Caqti_query.L"default"
   | "alt" -> Caqti_query.L"other"
   | _ -> raise Not_found
  in
  let env2 = function
   | "." -> Caqti_query.L"default."
   | "alt." -> Caqti_query.L"other."
   | _ -> raise Not_found
  in
  let env3 = function
   | "." -> Caqti_query.L"dot"
   | "cat" -> Caqti_query.L"mouse"
   | "cat." -> Caqti_query.L"dog"
   | _ -> raise Not_found
  in
  let q1 = Caqti_query.of_string_exn
    " $. $(.) $alt. $(alt.) $cat. $(cat) "
  in
  let q1' = Caqti_query.of_string_exn
    " default. default. other. other. $cat. $(cat) "
  in
  let q1'3 = Caqti_query.of_string_exn
    " dot dot $alt. $(alt.) dog mouse "
  in
  A.(check query) "same" q1' (Caqti_query.expand env1 q1);
  A.(check query) "same" q1' (Caqti_query.expand env2 q1);
  A.(check query) "same" q1'3 (Caqti_query.expand env3 q1)

let test_qprintf () =
  let open Caqti_query_fmt in
  let check_expect q1 q2 =
    A.(check query "same" (Caqti_query.normal q1) (Caqti_query.normal q2))
  in
  check_expect
    (S [L"SELECT "; P 0; L" WHERE "; Q"quote"; L" = "; E"env"])
    (qprintf {|%a %a WHERE %a = %a|} query (L"SELECT") param 0 quote "quote" env "env");
  check_expect
    (S [L"WHERE "; E"tbl4"; L".name = "; Q"John Wayne"])
    (qprintf {|WHERE @{<E>tbl%d@}.name = @{<Q>%s Wayne@}|} 4 "John")


let test_cases = [
  A.test_case "show, hash" `Quick test_show_and_hash;
  A.test_case "parse special cases" `Quick test_parse_special_cases;
  A.test_case "parse random strings" `Quick test_parse_random_strings;
  A.test_case "expand" `Quick test_expand;
  A.test_case "qprintf" `Quick test_qprintf;
]
