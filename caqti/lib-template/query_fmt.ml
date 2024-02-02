(* Copyright (C) 2023--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

open Query

type 'a t = Format.formatter -> 'a -> unit

type Format.stag += Stag_query of Query.t

let query ppf q =
  Format.pp_open_stag ppf (Stag_query q);
  Format.pp_print_string ppf "... SQL FRAGMENT ...";
  Format.pp_close_stag ppf ()

let quote ppf q = query ppf (Q q)
let env ppf e = query ppf (E e)
let param ppf p = query ppf (P p)

type mode = Mode_literal | Mode_ignore | Mode_raw of (string -> Query.t)

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

let bool ppf x = query ppf (V (Field_type.Bool, x))
let int ppf x = query ppf (V (Field_type.Int, x))
let float ppf x = query ppf (V (Field_type.Float, x))
let string ppf x = query ppf (V (Field_type.String, x))
let octets ppf x = query ppf (V (Field_type.Octets, x))
let pdate ppf x = query ppf (V (Field_type.Pdate, x))
let ptime ppf x = query ppf (V (Field_type.Ptime, x))
let ptime_span ppf x = query ppf (V (Field_type.Ptime_span, x))
