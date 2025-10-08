(* Copyright (C) 2025  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Reified constructors used for product row types.

    Usage of this module is somewhat technical and only needed when defining
    type descriptors for custom parametric types, or if, for some other reason,
    the type descriptor cannot be defined once statically.
    For statically defined types, the descriptor can be generated automatically.

    The descriptor defined here bundles a bare constructor function with a tag
    from an open GADT encoding its type.
    The type may be parametric as long as parameters in the result type occurs
    in argument types.

    As an example, consider the record type:
    {[
      type 'a acquired_value = {
        source: string;
        value: 'a;
        confidence: float;
      }
    ]}
    The naîve way of defining the descriptor,
    {[
      open Caqti_template
      open Caqti_template.Shims

      let acquired_value_rowtype value_rowtype =
        let open Row_type in
        product (fun source value confidence -> Ok {source; value; confidence})
        @@ proj string (fun {source; _} -> source)
        @@ proj value_rowtype (fun {value; _} -> value)
        @@ proj float (fun {confidence; _} -> confidence)
        @@ proj_end

      let () =
        let t1 = acquired_value_rowtype Row_type.int in
        let t2 = acquired_value_rowtype Row_type.int in
        assert (Row_type.unify t1 t2 = None) (* Bad! *)
    ]}
    has the issue that the function is generative, so each time a part of the
    application instantiates the descriptor with a descriptor of the value type,
    a fresh type descriptor is returned.
    In particular, this means that {!Row_type.unify} will fail to unify two
    descriptors describing the same type, if they are not physically equal.

    The right way of defining this descriptor is to use {!Row_type.product'},
    which takes the constructor descriptor instead of the bare constructor.
    To create the custom descriptor, you need to define a {!type-tag} with
    correct signature and a corresponding function to support type-unifying
    equality:
    {[
      type (_, _) Constructor.tag +=
        Acquired_value : (
          string -> 'a -> float -> 'a acquired_value Constructor.return,
          'a acquired_value
        ) Constructor.tag

      let acquired_value_constructor =
        let open Constructor in
        let unify_tag
          : type j b a. (j, b) tag ->
            (string -> a -> float -> a acquired_value return, j) unifier option =
          (function
           | Acquired_value ->
              Some (Assume (fun Type.Equal ->
                    Assume (fun Type.Equal ->
                    Assume (fun Type.Equal -> Equal))))
           | _ -> None)
        in
        let construct source value confidence = Ok {source; value; confidence} in
        {tag = Acquired_value; unify_tag; construct}
    ]}
    Our original attempt to define the descriptor can then be adjusted to
    support parametricity:
    {[
      let acquired_value_rowtype value_rowtype =
        let open Row_type in
        product' acquired_value_constructor
        @@ proj string (fun {source; _} -> source)
        @@ proj value_rowtype (fun {value; _} -> value)
        @@ proj float (fun {confidence; _} -> confidence)
        @@ proj_end

      let () =
        let t1 = acquired_value_rowtype Row_type.int in
        let t2 = acquired_value_rowtype Row_type.int in
        assert (Row_type.unify t1 t2 <> None) (* Good! *)
    ]} *)

open Shims

type 'a return = ('a, string) result
(** The result type of the constructor function. *)

type ('i, 'j) unifier =
  | Equal : ('i return, 'i return) unifier
  | Assume : (('a, 'b) Type.eq -> ('i, 'j) unifier) ->
      ('a -> 'i, 'b -> 'j) unifier (**)
(** [('i, 'j) unifier] witness that two constructors of types [('i, _) t] and
    [('j, _) t] are equal and provides a dependent unification of ['i] and ['j]
    in the following sense:

      - The ['i] and ['j] parameters are constrained by this type definition to
        have the same function shape which terminates in a result type.  I.e.
        they look like ['a1 -> ... -> 'aN -> 'b return].
      - For each constructor argument, a node [Assume f] is provided, where [f]
        accepts an equality proof of the constructor argument and returns a
        proof of the remaining constructor type.
      - The final node [Equal] witness, after resolving [Assume] nodes, that the
        constructed values have the same type.

    By constraining the return type, we avoid that the type accepted by {!Equal}
    overlaps with {!Assume}, meaning we can refute {!Equal} patterns when the
    type in question is known to be a function type. *)

type (_, _) tag = ..
(** [('a1 -> ... -> 'aN -> 'r return, 'r) tag] represents the type of a
    constructor which takes arguments of type ['a1], ..., ['aN] and returns
    values of type ['r].
    These tags is normally only passed around in the combination {!type-t}. *)

type (!'i, 'a) t = {
  tag: ('i, 'a) tag;
    (** The constructor type. *)
  unify_tag: 'j 'b. ('j, 'b) tag -> ('i, 'j) unifier option;
    (** Unifying equality for the constructor type. *)
  construct: 'i;
    (** The bare constructor. *)
}
(** [('a1 -> ... -> 'aN -> 'r return, 'r) t] represents a constructor which
    takes arguments of type ['a1], ..., ['aN] and constructs values of type
    ['r].  The public record type is exposed to allow passing the {!unify_tag}
    field in a way which preserves universal quantification.

    This type is only a reification of the constructor to allow comparison,
    disallowed for bare functions, and type unification.
    Ideally the {!construct} field is unique, while {!tag} and {!unify_tag} are
    implied by the type; the rest is technicalities which could be handle by a
    PPX or other kind of code generator.  *)

val unify : ('i, 'a) t -> ('j, 'b) t -> ('i, 'j) unifier option
(** [unify t t'] is [Some witness] if [t] and [t'] are equal, otherwise [None].
    In the former case, [witness] provides the unification of the result type of
    the construction, provided the unifications of each constructor argument
    type. *)
