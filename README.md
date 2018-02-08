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

## Status

Large parts of Caqti was rewritten for 0.8 and 0.9, the latter being the
first release targeted for to the official OPAM repository.  The old API is
still available in findlib libraries under the suffix `.v1`.  I need to keep
these for the time being, but do not use them for new code.

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
    - Does not pool connections or cache statements.

If you link against `caqti-dynload`, then drivers are loaded dynamically
based on the URI.  If dynamic loading is unavailable on your platform, you
may instead link against the `caqti-driver-*` libraries which you expect to
use.

## Documentation

As the main entry point, you would normally use either of

    Caqti_lwt : Caqti_connection_sig.S with type 'a io = 'a Lwt.t
    Caqti_async : Caqti_connection_sig.S with type 'a io = 'a Deferred.t

which is provided by `caqti-lwt` or `caqti-async`, respectively.  These
provide a connect functions which receives an URI, loads the appropriate
driver, and returns a connection as a first-class module containing query
functionality for the database.

A good place to start is with the [documented example][bikereg].  The API
reference is currently not online, but can be generated with `topkg doc`
from the source code or with [odig](http://erratique.ch/software/odig) from
an OPAM installation.

The most important modules to know about are:

  - `Caqti_type` and `Caqti_request` for constructing prepared or one-shot
    queries.
  - `Caqti_lwt` and `Caqti_async` for connecting to the database and
    obtaining a first class module implementing `Caqti_connection_sig.S`.
  - `Caqti_connection_sig.S` and `Caqti_response_sig.S` for executing
    queries.

## Running under utop

Dynamic linking does not work under utop.  The workaround is to link against
the needed database driver.  E.g.
```ocaml
> #require "caqti-lwt";;
> #require "caqti-driver-postgresql";;
> open Lwt.Infix;;
> let plus = Caqti_request.find Caqti_type.(tup2 int int) Caqti_type.int "SELECT ?::integer + ?::integer";;
val plus : (int * int, int, [< `Many | `One | `Zero > `One ]) Caqti_request.t = <abstr>
> module Db = (val Caqti_lwt.connect (Uri.of_string "postgresql://") >>= Caqti_lwt.or_fail |> Lwt_main.run);;
module Db : Caqti_lwt.CONNECTION
> Db.find plus (7, 13);;
- : (int, [> Caqti_error.call_or_retrieve ]) result = Ok 20
```

[bikereg]: tests/bikereg.ml
