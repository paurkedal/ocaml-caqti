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

type t =
  | L of string
  | V : 'a Caqti_type.Field.t * 'a -> t
  | Q of string
  | P of int
  | E of string
  | S of t list

let concat =
  let rec loop pfx acc = function
   | [] -> acc
   | q :: qs -> loop pfx (pfx :: q :: acc) qs
  in
  fun sep -> function
   | [] -> S[]
   | q :: qs -> S (q :: loop (L sep) [] (List.rev qs))

let bool x = V (Caqti_type.Field.Bool, x)
let int x = V (Caqti_type.Field.Int, x)
let float x = V (Caqti_type.Field.Float, x)
let string x = V (Caqti_type.Field.String, x)
let octets x = V (Caqti_type.Field.Octets, x)
let pdate x = V (Caqti_type.Field.Pdate, x)
let ptime x = V (Caqti_type.Field.Ptime, x)
let ptime_span x = V (Caqti_type.Field.Ptime_span, x)

let nulls t = List.init (Caqti_type.length t) (fun _ -> L"NULL")

let rec const_fields : type a. a Caqti_type.t -> a -> t list =
  (function
   | Field ft -> fun x -> [V (ft, x)]
   | Option t -> (function None -> nulls t | Some x -> const_fields t x)
   | Product (_, _, pt) -> const_fields_product pt
   | Annot (_, t) -> const_fields t)
and const_fields_product : type a i. (a, i) Caqti_type.product -> a -> t list =
  (function
   | Proj_end -> fun _ -> []
   | Proj (t, p, pt') ->
      fun x -> const_fields t (p x) @ const_fields_product pt' x)

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
      (match Caqti_type.Field.unify t1 t2 with
       | None -> false
       | Some Caqti_type.Equal -> Caqti_type.Field.equal_value t1 v1 v2)
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
 | V (t, v) -> Caqti_type.Field.pp_value ppf (t, v)
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

type expand_error = {
  query: t;
  var: string;
  reason: [`Undefined | `Invalid of t];
}

let pp_expand_error ppf {query; var; reason} =
  let open Format in
  (match reason with
   | `Undefined ->
      fprintf ppf "Undefined variable %s in query %a" var pp query
   | `Invalid expansion ->
      fprintf ppf
        "While expanding %a, lookup of %s gives %a, which is invalid \
         because it contains an environment or parameter reference."
        pp query var pp expansion)

exception Expand_error of expand_error

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
            (match normal (f (String.sub var 0 (l - 1))) with
             | S[] as q' -> q'
             | q' -> S[q'; L"."]
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
end

let angstrom_parser = Angstrom_parsers.expression
let angstrom_parser_with_semicolon = Angstrom_parsers.expression_with_semi

let of_string s =
  let open Angstrom.Unbuffered in
  (match parse angstrom_parser_with_semicolon with
   | Partial {committed = 0; continue} ->
      let len = String.length s in
      let bs = Bigstringaf.of_string ~off:0 ~len s in
      (match continue bs ~off:0 ~len Complete with
       | Done (committed, q) when committed = len -> Ok q
       | Done (committed, _) | Partial {committed; _} ->
          Error (`Invalid (committed, "Expression cannot contain semicolon."))
       | Fail (committed, _, msg) ->
          Error (`Invalid (committed, msg)))
   | Partial _ | Done _ | Fail _ ->
      assert false)

let of_string_exn s =
  (match of_string s with
   | Ok q -> q
   | Error (`Invalid (pos, msg)) ->
      Printf.ksprintf failwith "Parse error at byte %d: %s" pos msg)

(* Printf-like helpers *)

type Format.stag += Stag_query of t

let query ppf q =
  Format.pp_open_stag ppf (Stag_query q);
  Format.pp_print_string ppf "... SQL FRAGMENT ...";
  Format.pp_close_stag ppf ()

let quote ppf q = query ppf (Q q)
let env ppf e = query ppf (E e)
let param ppf p = query ppf (P p)

type mode = Mode_literal | Mode_ignore | Mode_raw of (string -> t)

let kqprintf k fmt =
  (* The formatter can be in three different modes, which is determined
     depending on the currently open tags (we exploit the fact that
     [mark_open_stag] and [mark_close_tag] are called immediately before the tag
     would actually be output to the formatter, and so can we can effectively
     use them as control directives for the behavior of [output_string]).

     Internally, the formatter holds a list of query elements in the reverse
     order that they have been produced in. Once we have finished parsing, that
     list is reversed, wrapped into the [S] constructor, and returned. We will
     call this list of elements we are building "the queue" in the following.

     Initially, the formatter starts in literal mode. In literal mode, we simply
     push each string that we receive from the formatter into the queue
     unmodified as a literal.

     When we encounter a tag in literal mode, we enter one of two auxiliary
     modes:

      - If the tag is the string tag "Q" or "E" (denoting a nested quote or env
        var format, respectively), we enter raw mode, parameterized by a
        function that returns either [Q] or [E]. In raw mode, we accumulate the
        content that gets printed into a buffer, and when exiting raw mode
        (i.e. when the corresponding tag gets closed), we wrap the accumulated
        content as a string into either the [Q] or [E] constructor, and push it
        into the queue. Then, we exit go back to literal mode.

      - If the tag is custom tag [Stag_query], added by the {!query} printer, we
        push the embedded query without modifications onto the queue, then
        switch to ignore mode. In ignore mode, we simply discard what is printed
        until we exit ignore mode by closing the corresponding tag. This is
        because, in ignore mode, the content that we are interested in is the
        parameter of the [Stag_query], not the textual content within the tag.

     Nesting of tags is not supported, so if we see one of the tags above in
     either of these modes, we raise a [Failure] exception.
  *)
  let elems = ref [] in
  let buf = Buffer.create 512 in

  let push q = elems := q :: !elems in
  let mode = ref Mode_literal in
  let flush_raw () =
    let mk = match !mode with Mode_raw f -> f | _ -> assert false in
    push @@ mk @@ Buffer.contents buf;
    Buffer.reset buf;
    mode := Mode_literal
  and flush_ignore () =
    match !mode with Mode_ignore -> mode := Mode_literal | _ -> assert false
  and flush_literal () =
    (* Unlike the similar cases in [flush_raw] and [flush_ignore], it is actually
       possible to call [flush_literal] while not in literal mode by nesting <Q>
       and/or <E> semantic tags.

       There is actually nothing to do here, because we push output strings into
       the queue on the fly. *)
    begin
      match !mode with
      | Mode_literal -> ()
      | _ -> failwith "invalid nesting of query tags; did you forget a `@}`?"
    end
  and output_string s p n =
    match !mode with
    | Mode_literal ->
      if n > 0 then
        if p = 0 && n = String.length s then
          push (L s)
        else
          push (L (String.sub s p n))
    | Mode_raw _ -> Buffer.add_substring buf s p n
    | Mode_ignore -> ()
  in
  let ppf = Format.make_formatter output_string flush_literal in
  let Format.
        { mark_open_stag; mark_close_stag; print_open_stag; print_close_stag } =
    Format.pp_get_formatter_stag_functions ppf ()
  in
  (* Note that we call [flush_literal] when *opening* tags but the other [flush]
     functions when *closing* tags.  Since [Format] enforces the
     well-parenthesising of tags, we are guaranteed to always be in the correct
     mode when calling [flush_raw] and [flush_ignore], but that is *not* the
     case for [flush_litera].
  *)
  let mark_open_stag = function
    | Format.String_tag "Q" ->
        flush_literal ();
        mode := Mode_raw (fun s -> Q s);
        ""
    | Format.String_tag "E" ->
        flush_literal ();
        mode := Mode_raw (fun s -> E s);
        ""
    | Stag_query q ->
        flush_literal ();
        push q;
        mode := Mode_ignore;
        ""
    | t -> mark_open_stag t
  and mark_close_stag = function
    | Format.String_tag ("Q" | "E") ->
        flush_raw ();
        ""
    | Stag_query _ ->
        flush_ignore ();
        ""
    | t -> mark_close_stag t
  in
  Format.pp_set_formatter_stag_functions ppf
    { mark_open_stag; mark_close_stag; print_open_stag; print_close_stag };
  Format.pp_set_mark_tags ppf true;
  Format.kfprintf
    (fun ppf ->
      Format.pp_print_flush ppf ();
      k (S (List.rev !elems)))
    ppf fmt

let qprintf fmt = kqprintf Fun.id fmt
