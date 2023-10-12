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

RDBMS      | URI scheme      | library        | Unix | MirageOS
---------- | --------------- | -------------- | ---- | --------
MariaDB    | `mariadb://`    | [mariadb][]    | yes  | no
PostgreSQL | `postgresql://` | [postgresql][] | yes  | no
PostgreSQL | `pgx://`        | [pgx][] (p)    | yes  | yes
SQLite3    | `sqlite3://`    | [sqlite3][]    | yes  | no

**The PGX based driver is not production-ready yet** due to the lack of TLS,
esp. given that the only supported authentication mechanism is plaintext.

If you link against `caqti-dynload`, then drivers are loaded dynamically
based on the URI.  If dynamic loading is unavailable on your platform, link
instead against the `caqti-driver-*` libraries which you expect to use.

## Documentation

Tutorials and examples:

  - The [caqti-study][] repository is a tutorial with examples, which we
    will keep up to date with the latest release of Caqti.  It is work in
    progress; suggestions and contributions are welcome.
  - [Interfacing OCaml and PostgreSQL with Caqti][BP-2018] by Bobby
    Priambodo gives a gentle introduction, though the Caqti API has changed
    to some extend since it was written.
  - [The documented example][bikereg] in this repository can give a first
    idea.

API documentation from ocaml.org contains documentation for matching
releases of individual packages:

  - [caqti](https://ocaml.org/p/caqti/latest)
  - [caqti-lwt](https://ocaml.org/p/caqti-lwt/latest)
  - [caqti-async](https://ocaml.org/p/caqti-async/latest)
  - [caqti-eio](https://ocaml.org/p/caqti-eio/latest) (pending release)
  - [caqti-mirage](https://ocaml.org/p/caqti-mirage/latest) (pending
    release)
  - [caqti-driver-mariadb](https://ocaml.org/p/caqti-driver-mariadb)
  - [caqti-driver-postgresql](https://ocaml.org/p/caqti-driver-postgresql)
  - [caqti-driver-sqlite3](https://ocaml.org/p/caqti-driver-sqlite3)
  - [caqti-driver-pgx](https://ocaml.org/p/caqti-driver-sqlite3) (pending
    release)

If the above lacks links to modules, it may be due to build issues.  You may
instead refer to my own latest rendering:

  - [Caqti API Reference][API] contains the index with the most important
    entry points being:
  - [caqti-blocking][] for Unix system library with no concurrency, supports
    all drivers
  - [caqti-lwt.unix][] for Lwt with Unix system library, supports all
    drivers
  - [caqti-async][] for Async with Unix system library, supports all drivers
  - [caqti-eio.unix][] for experimental Eio support.
  - [caqti-mirage][] for experimental MirageOS support.

The linked modules provide a connect functions which receives an URI, loads
the appropriate driver, and returns a connection as a first-class module
containing query functionality for the database.

You can also build the API reference matching your installed version using
[odig][] or run `dune build @doc` in a Git checkout.

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


[API]: http://paurkedal.github.io/ocaml-caqti/index.html
[BP-2018]: https://medium.com/@bobbypriambodo/interfacing-ocaml-and-postgresql-with-caqti-a92515bdaa11
[bikereg]: examples/bikereg.ml
[caqti-async]: https://paurkedal.github.io/ocaml-caqti/caqti-async/Caqti_async/index.html
[caqti-blocking]: https://paurkedal.github.io/ocaml-caqti/caqti/Caqti_blocking/index.html
[caqti-eio.unix]: https://paurkedal.github.io/ocaml-caqti/caqti-eio/Caqti_eio_unix/index.html
[caqti-lwt.unix]: https://paurkedal.github.io/ocaml-caqti/caqti-lwt/Caqti_lwt_unix/index.html
[caqti-mirage]: https://paurkedal.github.io/ocaml-caqti/caqti-mirage/Caqti_mirage/index.html
[caqti-study]: https://github.com/paurkedal/caqti-study/
[odig]: http://erratique.ch/software/odig
[topkg-care]: http://erratique.ch/software/topkg
[mariadb]: https://github.com/andrenth/ocaml-mariadb
[pgx]: https://github.com/arenadotio/pgx
[postgresql]: https://mmottl.github.io/postgresql-ocaml
[sqlite3]: https://mmottl.github.io/sqlite3-ocaml
