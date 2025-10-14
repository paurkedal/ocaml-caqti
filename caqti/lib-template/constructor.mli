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

(** Reified constructors for product row types.

    Usage of this module is somewhat technical and only needed when defining
    type descriptors for custom {e parametric} types, or if, for some other
    reason, the type descriptor cannot be defined once statically.
    For statically defined types, {!Row_type.product} can generate a descriptor
    from the bare function.

    This module bundles a bare constructor function with a fresh tag from an
    open GADT, used to identify it along with its type.
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
    A straight forward but not quite correct way to define a type descriptor for
    each parametric instance of this type constructor would be:
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
    The function [acquired_value_rowtype] is, however, generative; a fresh type
    descriptor is returned for each call, even for identical arguments.
    In particular, this means that {!Row_type.unify} will fail to unify two
    descriptors describing the same type, unless they are physically equal.

    The correct way of defining this descriptor is to use {!Row_type.product'},
    which expects a constructor descriptor instead of a bare constructor
    function.
    This is the purpose of this module.
    To create the custom descriptor, first define a {!type-tag} with correct
    signature and a corresponding type-unifying equality predicate:
    {[
      type (_, _) Constructor.tag +=
        Acquired_value : (
          string -> 'a -> float -> 'a acquired_value Constructor.return,
          'a acquired_value
        ) Constructor.tag

      let acquired_value_constructor =
        let tag = Acquired_value in
        let unify_tag
          : type j b a. (j, b) Constructor.tag ->
            (string -> a -> float -> a acquired_value Constructor.return, j)
              Constructor.unifier option =
          (function
           | Acquired_value ->
              Some (Constructor.Assume (fun Type.Equal ->
                    Constructor.Assume (fun Type.Equal ->
                    Constructor.Assume (fun Type.Equal -> Constructor.Equal))))
           | _ -> None)
        in
        let construct source value confidence = Ok {source; value; confidence} in
        {Constructor.tag; unify_tag; construct}
    ]}
    Our original attempt to define the descriptor can now be adjusted to support
    parametricity:
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
(** The result type of a constructor function. *)

type ('i, 'j) unifier =
  | Equal : ('i return, 'i return) unifier
  | Assume : (('a, 'b) Type.eq -> ('i, 'j) unifier) ->
      ('a -> 'i, 'b -> 'j) unifier (**)
(** [('i, 'j) unifier] witness that two constructors of types [('i, _) t] and
    [('j, _) t] are equal, providing a dependent unification of ['i] and ['j] in
    the following sense:

      - ['i] and ['j] are constrained by this definition to have the shape of a
        function which terminates in a {!result} type, like
        ['a1 -> ... -> 'aN -> 'b return].
      - For each constructor argument, a node [Assume f] is provided, where [f]
        accepts an equality proof of the constructor argument and returns a
        proof of the remaining constructor type.
      - A final node [Equal] which, after resolving [Assume] nodes, witness that
        constructed values have the same type.

    By constraining the return type to a non-abstract non-function type, we
    ensure that {!Equal}- and {!Assume}-patterns match disjoint types, so that
    we can refute {!Equal} patterns when the type in question is known to be a
    function type. *)

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
    (** The bare constructor function. *)
}
(** [('a1 -> ... -> 'aN -> 'r return, 'r) t] represents a constructor which
    takes arguments of type ['a1], ..., ['aN] and constructs values of type
    ['r].  The public record type is exposed to allow passing the {!unify_tag}
    field in a way which preserves universal quantification.

    This type is only a reification of the constructor to allow comparison,
    disallowed for bare functions, and type unification.
    Ideally the {!field-construct} field is unique, while {!field-tag} and
    {!field-unify_tag} are implied by the type; the rest is technicalities which
    could be handle by a PPX or other kind of code generator.  *)

val unify : ('i, 'a) t -> ('j, 'b) t -> ('i, 'j) unifier option
(** [unify t t'] is [Some witness] if [t] and [t'] are equal, otherwise [None].
    In the former case, [witness] provides the unification of the result type of
    the construction, provided the unifications of each constructor argument
    type. *)
