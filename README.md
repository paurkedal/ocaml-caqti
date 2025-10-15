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

Caqti does not generate or analyze SQL but provides templating and uniform
query parameter handling, including encoding and decoding data according to
declared types.  It is hoped that this agnostic choice makes it a suitable
target for higher level interfaces and code generators.

The following drivers are available:

RDBMS      | URI scheme      | library        | Unix | MirageOS
---------- | --------------- | -------------- | ---- | --------
MariaDB    | `mariadb://`    | [mariadb][]    | yes  | no
PostgreSQL | `postgresql://` | [postgresql][] | yes  | no
PostgreSQL | `pgx://`        | [pgx][]        | yes  | yes
SQLite3    | `sqlite3://`    | [sqlite3][]    | yes  | no

The PGX based driver is experimental and only recommended for MirageOS.
More about the drivers below.

## Documentation

### Tutorials and Examples

  - The [caqti-study][] repository is a tutorial with examples, which we
    will keep up to date with the latest release of Caqti.  It is work in
    progress; suggestions and contributions are welcome.
  - [Interfacing OCaml and PostgreSQL with Caqti][BP-2018] by Bobby
    Priambodo gives a gentle introduction, though the Caqti API has changed
    to some extend since it was written.
  - [The documented example][bikereg] in this repository can give a first
    idea.

### API Documentation for Stable Releases

The stable API documentation is hosted on [https://ocaml.org](), where you
can search package by name.

A full Caqti release contains the following packages:

  - [caqti](https://ocaml.org/p/caqti/latest):
    Core libraries and blocking unix connector.
  - [caqti-lwt](https://ocaml.org/p/caqti-lwt/latest):
    Lwt support library and connector.
  - [caqti-async](https://ocaml.org/p/caqti-async/latest):
    Async connector.
  - [caqti-eio](https://ocaml.org/p/caqti-eio/latest):
    Experimental EIO connector.
  - [caqti-miou](https://ocaml.org/p/caqti-miou/latest):
    Experimental Miou connector.
  - [caqti-mirage](https://ocaml.org/p/caqti-mirage/latest):
    Experimental MirageOS connector.
  - [caqti-driver-mariadb](https://ocaml.org/p/caqti-driver-mariadb):
    Driver for MariaDB and MySQL using C bindings from [mariadb][].
  - [caqti-driver-postgresql](https://ocaml.org/p/caqti-driver-postgresql):
    Driver for PostgreSQL using C bindings from [postgresql][].
  - [caqti-driver-sqlite3](https://ocaml.org/p/caqti-driver-sqlite3):
    Driver for local SQlite3 databases using C bindings from [sqlite3][].
  - [caqti-driver-pgx](https://ocaml.org/p/caqti-driver-pgx):
    Experimental driver for PostgreSQL using the [pgx][] library.
  - [caqti-tls](https://ocaml.org/p/caqti-tls):
    TLS configuration currently only used by caqti-mirage, and only relevant
    for pgx, since drivers based on C bindings have external TLS support.

The connector modules provide a connect functions which receives an URI,
dispatches to an appropriate driver, and returns a connection object as a
first-class module, which contains query functionality for the database.
The application can either link against the drivers it needs or the link
against the `caqti.plugin` library in order to load the appropriate driver
at runtime.

(A few package not mentioned include unreleased TLS packages and the
(semi-)deprecated packages
[caqti-type-calendar](https://ocaml.org/p/caqti-type-calendar/latest) and
[caqti-dynload](https://ocaml.org/p/caqti-dynload/latest).)

### API Documentation for Development Snapshots

Apart from the above links, the [GitHub pages][caqti-ghpages] are updated
occasionally with a rendering from the master branch.  You can also build
the API reference matching your installed version using [odig][] or run
`dune build @doc` in a Git checkout.

## Running under utop

Dynamic linking does not work under utop.  The workaround is to link against
the needed database driver.  E.g.
```ocaml
# #require "caqti-lwt";;
# #require "caqti-driver-postgresql";;
# open Lwt.Infix;;
# open Caqti_request.Infix;;

(* Create a DB handle. *)
# module Db = (val Caqti_lwt_unix.connect (Uri.of_string "postgresql://") >>= Caqti_lwt.or_fail |> Lwt_main.run);;
module Db : Caqti_lwt.CONNECTION

(* Create a request which merely adds two parameters. *)
# let plus = Caqti_request.(Caqti_type.(t2 int int) ->! Caqti_type.int) "SELECT ? + ?";;
val plus : (int * int, int, [< `Many | `One | `Zero > `One ]) Caqti_request.t =
  <abstr>

(* Run it. *)
# Db.find plus (7, 13);;
- : (int, [> Caqti_error.call_or_retrieve ]) result = Ok 20
```

## Related Software

  - [ppx\_rapper](https://github.com/roddyyaga/ppx_rapper) - a syntax
    extension for Caqti queries, simplifying type specifications.

## Sponsor

<a href="https://ocaml-sf.org">
<img align="left" alt="OCSF logo" src="https://ocaml-sf.org/assets/ocsf_logo.svg"/>
</a>
Thanks to the <a href="https://ocaml-sf.org">OCaml Software Foundation</a>
for economic support to the development of Caqti.


[caqti-ghpages]: http://paurkedal.github.io/ocaml-caqti/index.html
[BP-2018]: https://medium.com/@bobbypriambodo/interfacing-ocaml-and-postgresql-with-caqti-a92515bdaa11
[bikereg]: examples/bikereg.ml
[caqti-study]: https://github.com/paurkedal/caqti-study/
[odig]: http://erratique.ch/software/odig
[mariadb]: https://github.com/andrenth/ocaml-mariadb
[pgx]: https://github.com/arenadotio/pgx
[postgresql]: https://mmottl.github.io/postgresql-ocaml
[sqlite3]: https://mmottl.github.io/sqlite3-ocaml
