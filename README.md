[![CircleCI](https://circleci.com/gh/paurkedal/ocaml-caqti.svg?style=svg)](https://circleci.com/gh/paurkedal/ocaml-caqti)

## Synopsis

Caqti provides a monadic cooperative-threaded OCaml connector API for
relational databases.

The purpose of Caqti is further to help make applications independent of a
particular database system.  This is achieved by defining a common
signature, which is implemented by the database drivers.  Connection
parameters are specified as an URI, which is typically provided at run-time.
Caqti then loads a driver which can handle the URI, and provides a
first-class module which implements the driver API and additional
convenience functionality.

Caqti does not make assumptions about the structure of the query language,
and only provides the type information needed at the edges of communication
between the OCaml code and the database; i.e. for encoding parameters and
decoding returned tuples.  It is hoped that this agnostic choice makes it a
suitable target for higher level interfaces and code generators.

## Drivers

The following drivers are available.

  - MariaDB (`mariadb://`)
    - Implemented in terms of
      [ocaml-mariadb](https://github.com/andrenth/ocaml-mariadb)
      using asynchronous calls.
    - Supports transactions.
    - Pools connections and caches statements.
  - PostgreSQL (`postgresql://`)
    - Implemented in terms of
      [postgresql-ocaml](https://mmottl.github.io/postgresql-ocaml/)
      using asynchronous calls.
    - Supports transactions.
    - Pools connections and caches statements.
  - SQLite3 (`sqlite3://`)
    - Implemented in terms of
      [sqlite3-ocaml](https://github.com/mmottl/sqlite3-ocaml)
      using preemtive threading for non-blocking operation.
    - Supports transactions.
    - Does not pool connections but caches statements.

If you link against `caqti-dynload`, then drivers are loaded dynamically
based on the URI.  If dynamic loading is unavailable on your platform, you
may instead link against the `caqti-driver-*` libraries which you expect to
use.

## Documentation

For a gentle introduction I recommend reading [Interfacing OCaml and
PostgreSQL with Caqti][BP-2018] by Bobby Priambodo.  There is also a
[documented example][bikereg] in this repository.

A resent rendering of the [full API reference][API] is available online.
You can also generate the API reference matching your installed version
using [odig][].  Finally, `topkg doc` builds the reference from a Git
checkout using [topkg-care][].

As the main entry point, you would normally use either of

    Caqti_lwt : Caqti_connect_sig.S with type 'a future := 'a Lwt.t
    Caqti_async : Caqti_connect_sig.S with type 'a future := 'a Deferred.t
    Caqti_blocking : Caqti_connect_sig.S with type 'a future = 'a

provided by `caqti-lwt`, `caqti-async`, and `caqti.blocking`, respectively.
These provide a connect functions which receives an URI, loads the
appropriate driver, and returns a connection as a first-class module
containing query functionality for the database.

The most important modules to know about are:

  - `Caqti_type` and `Caqti_request` for constructing prepared or one-shot
    queries.
  - `Caqti_lwt`, `Caqti_async`, and `Caqti_blocking` for connecting to the
    database and obtaining a first class module implementing
    `Caqti_connection_sig.S`.
  - `Caqti_connection_sig.S` and `Caqti_response_sig.S` for executing
    queries.

## Running under utop

Dynamic linking does not work under utop.  The workaround is to link against
the needed database driver.  E.g.
```ocaml
# #require "caqti-lwt";;
# #require "caqti-driver-postgresql";;
# open Lwt.Infix;;

(* Create a DB handle. *)
# module Db = (val Caqti_lwt.connect (Uri.of_string "postgresql://") >>= Caqti_lwt.or_fail |> Lwt_main.run);;
module Db : Caqti_lwt.CONNECTION

(* Create a request which merely adds two parameters. *)
# let plus = Caqti_request.find Caqti_type.(tup2 int int) Caqti_type.int "SELECT ?::integer + ?::integer";;
val plus : (int * int, int, [< `Many | `One | `Zero > `One ]) Caqti_request.t =
  <abstr>

(* Run it. *)
# Db.find plus (7, 13);;
- : (int, [> Caqti_error.call_or_retrieve ]) result = Ok 20
```

[API]: http://paurkedal.github.io/ocaml-caqti/index.html
[BP-2018]: https://medium.com/@bobbypriambodo/interfacing-ocaml-and-postgresql-with-caqti-a92515bdaa11
[bikereg]: tests/bikereg.ml
[odig]: http://erratique.ch/software/odig
[topkg-care]: http://erratique.ch/software/topkg
