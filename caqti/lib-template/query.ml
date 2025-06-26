(* Copyright (C) 2019--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

open Shims

module Private = struct
  type t =
    | L of string
    | V : 'a Field_type.t * 'a -> t
    | Q of string
    | P of int
    | E of string
    | S of t list
end
open Private

type t = Private.t

let empty = S []
let lit frag = L frag
let quote str = Q str
let param i = P i
let var v = E v

let cat q1 q2 =
  (match q1, q2 with
   | S [], qs | qs, S [] -> qs
   | S qs1, S qs2 -> S (List.append qs1 qs2)
   | S qs1, q2 -> S (List.append qs1 [q2])
   | q1, S qs2 -> S (q1 :: qs2)
   | q1, q2 -> S [q1; q2])

let concat =
  let rec loop pfx acc = function
   | [] -> acc
   | q :: qs -> loop pfx (pfx :: q :: acc) qs
  in
  fun ?sep qs ->
    (match sep, qs with
     | _, [] -> S []
     | None, _ -> S qs
     | Some sep, q :: qs -> S (q :: loop (L sep) [] (List.rev qs)))

let parens q = concat [lit "("; q; lit ")"]

let bool x = V (Field_type.Bool, x)
let int x = V (Field_type.Int, x)
let int16 x = V (Field_type.Int16, x)
let int32 x = V (Field_type.Int32, x)
let int64 x = V (Field_type.Int64, x)
let float x = V (Field_type.Float, x)
let string x = V (Field_type.String, x)
let octets x = V (Field_type.Octets, x)
let pdate x = V (Field_type.Pdate, x)
let ptime x = V (Field_type.Ptime, x)
let ptime_span x = V (Field_type.Ptime_span, x)
let const t v = V (t, v)

let rec const_fields_opt : type a. a Row_type.t -> a option -> t list =
  (function
   | Field ft ->
      (function None -> [L "NULL"] | Some x -> [V (ft, x)])
   | Option t ->
      let of_t = const_fields_opt t in
      (function None -> of_t None | Some x -> of_t x)
   | Product (_, pt) ->
      const_fields_opt_of_product pt
   | Annot (_, t) ->
      const_fields_opt t)
and const_fields_opt_of_product
  : type i a. (i, a) Row_type.product -> a option -> t list =
  (function
   | Proj_end -> fun _ -> []
   | Proj (t, p, pt) ->
      let of_t = const_fields_opt t in
      let of_pt = const_fields_opt_of_product pt in
      fun x -> of_t (Option.map p x) @ of_pt x)

let const_fields (t : _ Row_type.t) =
  let f = const_fields_opt t in
  fun x -> f (Some x)

let rec equal_list f xs ys = (* stdlib 4.12.0 *)
  (match xs, ys with
   | [], [] -> true
   | x :: xs', y :: ys' -> f x y && equal_list f xs' ys'
   | [], _ :: _ | _ :: _, [] -> false)

let normal =
  let rec collect acc = function
   | [] -> List.rev acc
   | ((L"" | S[]) :: qs) -> collect acc qs
   | ((P _ | V _ | Q _ | E _ as q) :: qs) -> collect (q :: acc) qs
   | (S (q' :: qs') :: qs) -> collect acc (q' :: S qs' :: qs)
   | (L s :: qs) -> collectL acc [s] qs
  and collectL acc accL = function
   | ((L"" | S[]) :: qs) -> collectL acc accL qs
   | (L s :: qs) -> collectL acc (s :: accL) qs
   | (S (q' :: qs') :: qs) -> collectL acc accL (q' :: S qs' :: qs)
   | [] | ((P _ | V _ | Q _ | E _) :: _) as qs ->
      collect (L (String.concat "" (List.rev accL)) :: acc) qs
  in
  fun q ->
    (match collect [] [q] with
     | [] -> S[]
     | [q] -> q
     | qs -> S qs)

let rec equal t1 t2 =
  (match t1, t2 with
   | L s1, L s2 -> String.equal s1 s2
   | V (t1, v1), V (t2, v2) ->
      (match Field_type.unify t1 t2 with
       | None -> false
       | Some Type.Equal -> Field_type.equal_value t1 v1 v2)
   | Q s1, Q s2 -> String.equal s1 s2
   | P i1, P i2 -> Int.equal i1 i2
   | E n1, E n2 -> String.equal n1 n2
   | S ts1, S ts2 -> equal_list equal ts1 ts2
   | V _, _ -> false
   | L _, _ -> false
   | Q _, _ -> false
   | P _, _ -> false
   | E _, _ -> false
   | S _, _ -> false)

let hash = Hashtbl.hash

let rec pp ppf = function
 | L s -> Format.pp_print_string ppf s
 | V (t, v) -> Field_type.pp_value ppf (t, v)
 | Q s ->
    (* Using non-SQL quoting, to avoid issues with newlines and other control
     * characters when printing to log files. *)
    Format.pp_print_string ppf "E'";
    for i = 0 to String.length s - 1 do
      (match s.[i] with
       | '\\' -> Format.pp_print_string ppf {|\\|}
       | '\'' -> Format.pp_print_string ppf {|\'|}
       | '\t' -> Format.pp_print_string ppf {|\t|}
       | '\n' -> Format.pp_print_string ppf {|\n|}
       | '\r' -> Format.pp_print_string ppf {|\r|}
       | '\x00'..'\x1f' as c -> Format.fprintf ppf {|\x%02x|} (Char.code c)
       | _ -> Format.pp_print_char ppf s.[i])
    done;
    Format.pp_print_char ppf '\''
 | P n -> Format.pp_print_char ppf '$'; Format.pp_print_int ppf (n + 1)
 | E n -> Format.fprintf ppf "$(%s)" n
 | S qs -> List.iter (pp ppf) qs

let show q =
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf q; Format.pp_print_flush ppf ();
  Buffer.contents buf

module Expand_error = struct

  type nonrec t = {
    query: t;
    var: string;
    reason: [`Undefined | `Invalid of t];
  }

  let pp ppf {query; var; reason} =
    let open Format in
    (match reason with
     | `Undefined ->
        fprintf ppf "Undefined variable %s in query %a" var pp query
     | `Invalid expansion ->
        fprintf ppf
          "While expanding %a, lookup of %s gives %a, which is invalid \
           because it contains an environment or parameter reference."
          pp query var pp expansion)
end

exception Expand_error of Expand_error.t

type subst = string -> t

let expand ?(final = false) f query =
  let rec is_valid = function
   | L _ | V _ | Q _ -> true
   | P _ | E _ -> false
   | S qs -> List.for_all is_valid qs
  in
  let rec recurse = function
   | L _ | V _ | Q _ | P _ as q -> q
   | E var as q ->
      let not_found () =
        if not final then q else
        raise (Expand_error {query; var; reason = `Undefined})
      in
      (match f var with
       | q' ->
          if is_valid q' then q' else
          raise (Expand_error {query; var; reason = `Invalid q'})
       | exception Not_found ->
          let l = String.length var in
          if l > 0 && var.[l - 1] = '.' then
            (match f (String.sub var 0 (l - 1)) with
             | frag ->
                (match normal frag with
                 | S[] as q' -> q'
                 | q' -> S[q'; L"."])
             | exception Not_found -> not_found ())
          else
            not_found ())
   | S qs -> S (List.map recurse qs)
  in
  recurse query

module Angstrom_parsers = struct
  open Angstrom

  let failf = Printf.ksprintf fail
  let ign p = p >>| fun _ -> ()

  let is_digit = function '0'..'9' -> true | _ -> false
  let is_digit_nz = function '1'..'9' -> true | _ -> false
  let is_idrfst = function 'a'..'z'|'A'..'Z' | '_' -> true | _ -> false
  let is_idrcnt = function 'a'..'z'|'A'..'Z' | '_' | '0'..'9' -> true | _ -> false
  let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

  let single_quoted = skip_many (ign (not_char '\'') <|> ign (string "''"))
  let double_quoted = skip_many (ign (not_char '"') <|> ign (string "\"\""))

  let tagged_quote_cont =
    consumed (skip is_idrfst *> skip_while is_idrcnt) <* char '$' >>= fun tag ->
    many_till any_char (char '$' *> string tag <* char '$') >>| (fun _ -> ())

  let verbatim =
    let fragment = any_char >>= function
     | '\'' -> single_quoted <* char '\''
     | '"' -> double_quoted <* char '"'
     | '`' -> skip_many (not_char '`') <* char '`'
     | '-' ->
        (peek_char >>= function
         | Some '-' -> skip_while ((<>) '\n') <* char '\n'
         | _ -> return ())
     | '$' -> tagged_quote_cont
     | '?' | ';' as c -> failf "%C is not valid here" c
     | _ -> return ()
    in
    consumed (many1 fragment) >>| (fun s -> (L s))

  let skip_idr = skip is_idrfst *> skip_while is_idrcnt
  let identifier_dot = consumed (skip_idr *> char '.')
  let identifier_dotopt = consumed (option () skip_idr *> option ' ' (char '.'))
  let parameter_number = consumed (skip is_digit_nz *> skip_while is_digit)

  let lookup =
    choice ~failure_msg:"invalid environment lookup" [
      string "$(" *> identifier_dotopt <* char ')' >>| (fun v -> E v);
      string "$." >>| (fun _ -> E ".");
      char '$' *> identifier_dot >>| (fun v -> E v);
    ]

  let untagged_quote =
    let nonlookup =
      consumed (many1 (satisfy (function '$' -> false | _ -> true)))
        >>| (fun s -> (L s))
    in
    string "$$" *> many_till (lookup <|> nonlookup) (string "$$") >>| fun qs ->
    normal (S ([L "$$"] @ qs @ [L "$$"]))

  let atom =
    peek_char_fail >>= function
     | '$' ->
        choice ~failure_msg:"invalid dollar sequence" [
          char '$' *> parameter_number >>| (fun iP -> P (int_of_string iP - 1));
          lookup;
          untagged_quote;
          verbatim;
        ]
     | '?' ->
        let valid_lookahead = peek_char >>= function
         | Some ':' ->
            (peek_string 2 >>= function
             | "::" -> return ()
             | _ -> fail "':' is not allowed after parameter reference '?'")
         | Some ('A'..'Z' | 'a'..'z' | '0'..'9' | '_'
               | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '.'
               | '<' | '=' | '>' | '?' | '@' | '^' | '`' | '|' | '~' as c) ->
            failf "%C is not allowed after parameter reference '?'" c
         | None | Some _ ->
            return ()
        in
        char '?' >>| (fun _ -> P (-1)) <* valid_lookahead
     | _ ->
        verbatim

  let atom_or_semi = (char ';' >>| fun _ -> L";") <|> atom

  let reindex atoms =
    if List.for_all (function P (-1) -> false | _ -> true) atoms then
      return atoms
    else
    let rec loop iP acc = function
     | [] -> return (List.rev acc)
     | P (-1) :: frags -> loop (iP + 1) (P iP :: acc) frags
     | P _ :: _ -> fail "Inconsistent parameter style."
     | frag :: frags -> loop iP (frag :: acc) frags
    in
    loop 0 [] atoms

  let expression =
    let stop =
      peek_char >>= function
       | None | Some ';' -> return ()
       | _ -> fail "unterminated"
    in
    fix (fun p -> (stop *> return []) <|> (List.cons <$> atom <*> p))
      >>= reindex >>| (function [q] -> q | qs -> S qs)

  let expression_with_semi =
    let stop =
      peek_char >>= function
       | None -> return ()
       | _ -> fail "unterminated"
    in
    fix (fun p -> (stop *> return []) <|> (List.cons <$> atom_or_semi <*> p))
      >>= reindex >>| (function [q] -> q | qs -> S qs)

  let expression_list =
    let white =
      many (take_while1 is_space <|> (string "--" *> take_till ((=) '\n')))
        <* commit
    in
    white *> many (expression <* char ';' <* white)
end

let angstrom_parser = Angstrom_parsers.expression
let angstrom_parser_with_semicolon = Angstrom_parsers.expression_with_semi
let angstrom_list_parser = Angstrom_parsers.expression_list

module Parse_error = struct
  type t = {
    position: int;
    message: string;
  }

  let create position message = {position; message}

  let position err = err.position
  let message err = err.message

  let pp ppf err =
    Format.fprintf ppf "Parse error at byte %d: %s" err.position err.message
end

exception Parse_error of Parse_error.t

let parse_result s =
  let open Angstrom.Unbuffered in
  (match parse angstrom_parser_with_semicolon with
   | Partial {committed = 0; continue} ->
      let len = String.length s in
      let bs = Bigstringaf.of_string ~off:0 ~len s in
      (match continue bs ~off:0 ~len Complete with
       | Done (committed, q) when committed = len -> Ok q
       | Done (committed, _) | Partial {committed; _} ->
          let msg = "Expression cannot contain semicolon." in
          Error (Parse_error.create committed msg)
       | Fail (committed, _, msg) ->
          Error (Parse_error.create committed msg))
   | Partial _ | Done _ | Fail _ ->
      assert false)

let parse s =
  (match parse_result s with
   | Ok q -> q
   | Error err -> raise (Parse_error err))

module Infix = struct
  let (@++) = cat
  let (^++) pfx q = cat (lit pfx) q
  let (++^) q sfx = cat q (lit sfx)
end
