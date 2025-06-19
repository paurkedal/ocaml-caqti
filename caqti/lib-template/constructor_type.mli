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

(** Type descriptor for constructors used for decoding rows.
    Usage of this module is somewhat technical and only needed when defining
    type descriptors for custom parametric types, or if, for some other reason,
    the type descriptor cannot be defined once statically.
    For statically defined types, the descriptor can be generated automatically.

    The constructor type descriptor encodes the shape of the constructed value
    and the arguments, which may include type parameters.
    Type descriptors for the arguments are included here.

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
    which takes the constructor type descriptor as an extra first argument.
    To create the custom descriptor, you need to define a {!tag} with correct
    signature and a corresponding function to support type-unifying equality:
    {[
      type (_, _) Constructor_type.tag +=
        Acquired_value : (
          string -> 'a -> float -> 'a acquired_value Constructor_type.return,
          'a acquired_value
        ) Constructor_type.tag

      let acquired_value_constructor_type =
        let open Constructor_type in
        let unify_tag
          : type j b a. (j, b) tag ->
            (string -> a -> float -> a acquired_value return, j) dep_eq option =
          (function
           | Acquired_value ->
              Some (Dep (fun Type.Equal ->
                    Dep (fun Type.Equal ->
                    Dep (fun Type.Equal -> Eq))))
           | _ -> None)
        in
        {tag = Acquired_value; unify_tag}
    ]}
    Our original attempt to define the descriptor can then be adjusted to
    support parametricity:
    {[
      let acquired_value_rowtype value_rowtype =
        let open Row_type in
        product' acquired_value_constructor_type
          (fun source value confidence -> Ok {source; value; confidence})
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

type ('i, 'j) dep_eq =
  | Eq : ('i return, 'i return) dep_eq
  | Dep : (('a, 'b) Type.eq -> ('i, 'j) dep_eq) -> ('a -> 'i, 'b -> 'j) dep_eq
(** A variant of {!Stdlib.Type.eq} which interprets function types such that
    domains translates to proof obligations and the codomain translates to a
    subsequent proof. *)

type (_, _) tag = ..

type (!'i, 'a) t = {
  tag: ('i, 'a) tag;
  unify_tag: 'j 'b. ('j, 'b) tag -> ('i, 'j) dep_eq option;
}

val unify : ('i, 'a) t -> ('j, 'b) t -> ('i, 'j) dep_eq option
(** [unify x y] is [Some eq] if [x] and [y] are equal, otherwise [None].  In the
    former case, [eq] records the type unifications between [x] and [y]. *)
