(* OASIS_START *)
(* OASIS_STOP *)

let () = dispatch begin function

  | Before_options ->
    Options.use_ocamlfind := true

  | After_rules as e ->
    flag ["doc"; "ocaml"] & A"-thread";
    flag ["doc"; "ocaml"; "extension:html"] &
      S[A"-charset"; A"utf8"; A"-t"; A"The Caqti API"];
    dispatch_default e

  | e ->
    dispatch_default e

end
