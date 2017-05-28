#! /usr/bin/env ocaml
#use "topfind"
#require "topkg-jbuilder"

open Topkg

let licenses = List.map Pkg.std_file ["COPYING"; "COPYING.LESSER"]

let () = Topkg_jbuilder.describe ~licenses ~name:"caqti" ()
