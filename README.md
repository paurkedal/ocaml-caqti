## Synopsis

Caqti is an library providing a common API to SQL databases and similar data
interfaces.  The assumption is that the data service is queried by passing a
string along with typed parameters, and responds with a sequence of typed
tuples.
Caqti may be used directly or to provide multi-database support to
higher-level solutions, like code generators and syntax extensions.

The interface abstracts over an IO monad to support cooperative threading.
Async and Lwt instantiations are shipped with the main library, as well as a
functor for instantiating over other IO monads.

## Status

The library is under development; expect backwards incompatible API changes.
Backends are implemented for

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

## Documentation

As the main entry point, you would normally use either of

    Caqti_lwt : Caqti_sigs.CAQTI with type 'a io = 'a Lwt.t
    Caqti_async : Caqti_sigs.CAQTI with type 'a io = 'a Deferred.Or_error.t

which is provided by `caqti.lwt` or `caqti.async`, respectively.  These
provide a connect functions which receives an URI, loads the appropriate
backend, and returns a connection as a first-class module containing query
functionality for the database.

The best place to start may be with a [documented example][bikereg].  The
main documentation is the [API Reference][apiref].  Apart from the above
connectors, the most important modules to know are:

  - `Caqti_query` is used for constructing query strings.
  - `Caqti_sigs.CAQTI.CONNECTION` is the interface to a connected database.


[apiref]: http://paurkedal.github.io/ocaml-caqti/
[bikereg]: tests/bikereg.ml
