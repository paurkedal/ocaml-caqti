(* Copyright (C) 2017--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

type dialect_tag = [`Mysql | `Pgsql | `Sqlite | `Other]
type sql_dialect_tag = [`Mysql | `Pgsql | `Sqlite]
type parameter_style =
  [ `None
  | `Linear of string
  | `Indexed of (int -> string) ]

type t = {
  uri_scheme: string;
  dialect_tag: dialect_tag;
  parameter_style: parameter_style;
  can_transact: bool;
  can_pool: bool;
  can_concur: bool;
  dummy_dialect: Dialect.t;
}

let create
    ~uri_scheme
    ?(dialect_tag = `Other)
    ?(parameter_style = `None)
    ~can_pool
    ~can_concur
    ~can_transact
    ~dummy_dialect
    () =
  {
    uri_scheme;
    dialect_tag;
    parameter_style;
    can_transact;
    can_pool;
    can_concur;
    dummy_dialect;
  }

let dummy = create
  ~uri_scheme:"dummy"
  ~can_pool:false ~can_concur:false ~can_transact:false
  ~dummy_dialect:(Dialect.create_unknown ~purpose:`Dummy ())
  ()

let uri_scheme di = di.uri_scheme
let dialect_tag di = di.dialect_tag
let parameter_style di = di.parameter_style
let can_pool di = di.can_pool
let can_concur di = di.can_concur
let can_transact di = di.can_transact

let dummy_dialect di = di.dummy_dialect

let of_dialect = function
 | Dialect.Pgsql {client_library = `postgresql; _} as dialect ->
    create
      ~uri_scheme:"postgresql"
      ~dialect_tag:`Pgsql
      ~parameter_style:(`Indexed (fun i -> "$" ^ string_of_int (succ i)))
      ~can_pool:true
      ~can_concur:true
      ~can_transact:true
      ~dummy_dialect:dialect
      ()
 | Dialect.Pgsql {client_library = `pgx; _} as dialect ->
    create
      ~uri_scheme:"pgx"
      ~dialect_tag:`Pgsql
      ~parameter_style:(`Indexed (fun i -> "$" ^ string_of_int (succ i)))
      ~can_pool:true
      ~can_concur:true
      ~can_transact:true
      ~dummy_dialect:dialect
      ()
 | Dialect.Mysql _ as dialect ->
    create
      ~uri_scheme:"mariadb"
      ~dialect_tag:`Mysql
      ~parameter_style:(`Linear "?")
      ~can_pool:true
      ~can_concur:true
      ~can_transact:true
      ~dummy_dialect:dialect
      ()
 | Dialect.Sqlite _ as dialect ->
    create
      ~uri_scheme:"sqlite3"
      ~dialect_tag:`Sqlite
      ~parameter_style:(`Linear "?")
      ~can_pool:true
      ~can_concur:false
      ~can_transact:true
      ~dummy_dialect:dialect
      ()
 | _ -> dummy
