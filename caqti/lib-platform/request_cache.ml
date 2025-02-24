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

[@@@alert "-caqti_private"]
open Caqti_template

module type S = sig
  type elt
  type t

  val create : ?dynamic_capacity: int -> Dialect.t -> t

  val find_and_promote : t -> ('a, 'b, 'm) Request.t -> elt option
  val add : t -> ('a, 'b, 'm) Request.t -> elt -> unit
  val remove_and_discard : t -> ('a, 'b, 'm) Request.t -> unit
  val deallocate : t -> ('a, 'b, 'm) Request.t -> (elt * (unit -> unit)) option
  val iter : (elt -> unit) -> t -> unit
  val elements : t -> elt list
  val trim : ?max_promote_count: int -> t -> elt list * (unit -> unit)
  val clear_and_discard : t -> unit
  val dynamic_weight : t -> int
end

module Key = struct

  type t =
    T : {
      param_type: 'a Row_type.t;
      row_type: 'b Row_type.t;
      row_mult: 'm Row_mult.t;
      query: Query.t;
    } -> t

  let create request dialect =
    T {
      param_type = Request.param_type request;
      row_type = Request.row_type request;
      row_mult = Request.row_mult request;
      query = Request.query request dialect;
    }

  let equal (T k1) (T k2) =
    Query.equal k1.query k2.query
      && Row_type.unify k1.param_type k2.param_type <> None
      && Row_type.unify k1.row_type k2.row_type <> None

  let hash (T k) =
    (* TODO: Consider also hashing over the types. *)
    Query.hash k.query

end

let is_static request =
  (match Request.prepare_policy request with
   | Request.Direct ->
      failwith "Prepare_cache must not be used with direct requests."
   | Request.Dynamic -> false
   | Request.Static -> true)

module Make (Elt : Lru.Weighted) = struct

  type elt = Elt.t

  (* This module adds a weak pointer to the request template, so that we can
   * promote the associated prepare query in the LRU cache if the request has
   * not been garbage collected.  To avoid extra engineering with limited
   * gain, we only track the first request producing a certain key.  This seems
   * better than tracking the latest request in the case there are a mixture of
   * short- and long-lived request, since it gives longer lived requests a
   * better chance of holding on to the liveness slot. *)
  module Dynamic_node = struct
    type t = {
      elt: Elt.t;
      liveness_witness: Request.liveness_witness Weak.t;
    }

    let create request elt =
      let liveness_witness = Weak.create 1 in
      Weak.set liveness_witness 0 (Some (Request.liveness_witness request));
      {elt; liveness_witness}

    let is_alive node = Weak.check node.liveness_witness 0

    let elt node = node.elt

    let weight node = Elt.weight node.elt
  end

  module Static_cache = Hashtbl.Make (Key)
  module Dynamic_cache = Lru.M.Make (Key) (Dynamic_node)

  type t = {
    dialect: Caqti_template.Dialect.t;
    static_cache: Elt.t Static_cache.t;
    dynamic_cache: Dynamic_cache.t;
    mutable dynamic_orphans: Elt.t list;
  }

  let create ?(dynamic_capacity = 20) dialect = {
    dialect;
    static_cache = Static_cache.create 11;
    dynamic_cache = Dynamic_cache.create dynamic_capacity;
    dynamic_orphans = [];
  }

  let find_and_promote cache request =
    let key = Key.create request cache.dialect in
    if is_static request then
      (* Try the static map first, then the dynamic map.  If found in the
       * latter, move the binding to the former, since we have a witness of the
       * static lifetime of the associated prepared query. *)
      (match Static_cache.find_opt cache.static_cache key with
       | None ->
          (match Dynamic_cache.find key cache.dynamic_cache with
           | None -> None
           | Some node ->
              let elt = Dynamic_node.elt node in
              Static_cache.add cache.static_cache key elt;
              Dynamic_cache.remove key cache.dynamic_cache;
              Some elt)
       | Some elt -> Some elt)
    else
      (* Try the dynamic map first, then the static map. *)
      (match Dynamic_cache.find key cache.dynamic_cache with
       | None ->
          Static_cache.find_opt cache.static_cache key
       | Some node ->
          Dynamic_cache.promote key cache.dynamic_cache;
          Some (Dynamic_node.elt node))

  let rec trim' ~max_promote_count cache =
    let cap = Dynamic_cache.capacity cache.dynamic_cache in
    if Dynamic_cache.weight cache.dynamic_cache > cap then
      (match Dynamic_cache.lru cache.dynamic_cache with
       | None -> assert false
       | Some (key, node) when Dynamic_node.is_alive node ->
          if max_promote_count > 0 then
            begin
              Dynamic_cache.promote key cache.dynamic_cache;
              trim' ~max_promote_count:(max_promote_count - 1) cache
            end
       | Some (_, node) ->
          cache.dynamic_orphans <- node.elt :: cache.dynamic_orphans;
          Dynamic_cache.drop_lru cache.dynamic_cache;
          trim' ~max_promote_count cache)

  let trim ?(max_promote_count = 1) cache =
    trim' ~max_promote_count cache;
    (cache.dynamic_orphans, (fun () -> cache.dynamic_orphans <- []))

  let add cache request elt =
    trim' ~max_promote_count:0 cache;
    let key = Key.create request cache.dialect in
    assert (not (Static_cache.mem cache.static_cache key));
    assert (not (Dynamic_cache.mem key cache.dynamic_cache));
    if is_static request then
      Static_cache.add cache.static_cache key elt
    else
      let node = Dynamic_node.create request elt in
      Dynamic_cache.add key node cache.dynamic_cache

  let remove_and_discard cache request =
    let key = Key.create request cache.dialect in
    if is_static request then
      begin
        assert (Static_cache.mem cache.static_cache key);
        Static_cache.remove cache.static_cache key
      end
    else
      begin
        assert (Dynamic_cache.mem key cache.dynamic_cache);
        Dynamic_cache.remove key cache.dynamic_cache
      end

  let deallocate cache request =
    let key = Key.create request cache.dialect in
    if is_static request then
      (match Static_cache.find_opt cache.static_cache key with
       | None -> None
       | Some elt ->
          let commit () = Static_cache.remove cache.static_cache key in
          Some (elt, commit))
    else
      (match Dynamic_cache.find key cache.dynamic_cache with
       | None -> None
       | Some node ->
          let commit () = Dynamic_cache.remove key cache.dynamic_cache in
          Some (node.elt, commit))

  let iter f cache =
    Static_cache.iter (Fun.const f) cache.static_cache;
    Dynamic_cache.iter (fun _ node -> f node.elt) cache.dynamic_cache

  let elements cache =
    let add_static _ elt acc = elt :: acc in
    let add_dynamic _ node acc = node.Dynamic_node.elt :: acc in
    [] |> Static_cache.fold add_static cache.static_cache
       |> Fun.flip (Dynamic_cache.fold add_dynamic) cache.dynamic_cache

  let clear_and_discard cache =
    Static_cache.clear cache.static_cache;
    let cap = Dynamic_cache.capacity cache.dynamic_cache in
    Dynamic_cache.resize 0 cache.dynamic_cache;
    Dynamic_cache.trim cache.dynamic_cache;
    Dynamic_cache.resize cap cache.dynamic_cache

  let dynamic_weight cache = Dynamic_cache.weight cache.dynamic_cache
end
