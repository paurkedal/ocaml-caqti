## Synopsis

Caqti is an library providing a common API to SQL databases and similar data
interfaces.  The assumption is that the data service is queried by passing a
string along with typed parameters, and responds with a sequence of typed
tuples.
Caqti may be used directly or to provide multi-database support for
higher-level solutions, like code generators and syntax extensions.

The interface abstracts over an IO monad to support cooperative threading.
Async and Lwt instantiations are shipped with the main library, as well as a
functor for instantiating over other IO monads and system libraries.

## Status

A revised API is mostly ready, pending a final evaluation.  If you generate
the documentation, you will see that module titles are marked with "(v1)"
or "(v2)", indicating which revision of the interface they belong to.
Unmarked module are used by both revisions.  If all goes well, "(v1)" will
be deprecated in favour of "(v2)".

## Drivers

The following drivers are available.

  - MariaDB (`mariadb://`)
    - Implemented in terms of
      [ocaml-mariadb](https://github.com/andrenth/ocaml-mariadb)
      using asynchronous calls.
    - Supports transactions.
    - Does not currently support describe.
    - Pools connections and caches statements.
  - PostgreSQL (`postgresql://`)
    - Implemented in terms of
      [postgresql-ocaml](https://mmottl.github.io/postgresql-ocaml/)
      using asynchronous calls.
    - Supports transactions.
    - Supports describe.
    - Pools connections and caches statements.
  - SQLite3 (`sqlite3://`)
    - Implemented in terms of
      [sqlite3-ocaml](https://github.com/mmottl/sqlite3-ocaml)
      using preemtive threading for non-blocking operation.
    - Supports transactions.
    - Supports describe, but without type information for parameters.
    - Does not pool connections or cache statements.

Drivers are loaded dynamically based on the URI, but if dynamic loading is
unavailable, you can statically link the `caqti-driver-*` libraries which
you expect to use into your application.

## Documentation

As the main entry point, you would normally use either of

    Caqti_lwt.V2 : Caqti_connection_sig.S with type 'a io = 'a Lwt.t
    Caqti_async.V2 : Caqti_connection_sig.S with type 'a io = 'a Deferred.t

which is provided by `caqti-lwt` or `caqti-async`, respectively.  These
provide a connect functions which receives an URI, loads the appropriate
driver, and returns a connection as a first-class module containing query
functionality for the database.

The best place to start may be with a [documented example][bikereg].  The
API reference is currently not online, but can be generated with `topkg doc`
from the source code or with [odig](http://erratique.ch/software/odig) from
an OPAM installation.

The most important modules to know about are:

  - `Caqti_type` and `Caqti_request` for constructing prepared or one-shot
    queries.
  - `Caqti_lwt.V2` and `Caqti_async.V2` for connecting to the database and
    obtaining a first class module implementing `Caqti_connection_sig.S`.
  - `Caqti_connection_sig.S` and `Caqti_response_sig.S` for executing
    queries.

[bikereg]: tests/bikereg.ml
