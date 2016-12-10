open Ocamlbuild_plugin

let () = dispatch @@ function
 | After_rules ->
    (match Sys.getenv "TERM" with
     | exception Not_found -> ()
     | "" | "dumb" -> ()
     | _ -> flag ["ocaml"; "compile"] (S [A"-color"; A"always"]));
    rule "%.mli & %.idem -> %.ml"
      ~deps:["%.mli"; "%.idem"] ~prod:"%.ml"
      (fun env _ -> cp (env "%.mli") (env "%.ml"));
 | _ -> ()
