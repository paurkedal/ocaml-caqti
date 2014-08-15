## Synopsis

Caqti is an interface to data sources which accept a parametrised query
string and responds with a sequence of tuples.  It is especially aimed at
relational databases, where SQL is the common query language.  Caqti may be
used directly or as an intermediate layer between database client libraries
and high-level solutions like code generators and syntax extensions.

The interface abstracts over an IO monad to support cooperative threading.
Async and Lwt are provided by the main library, as well as functor for
instantiating over other IO monads.

## Status

The library is under development; expect backwards incompatible API changes.
Backends are implemented for

  - PostgreSQL (`postgresql:/`)
    - Implemented in terms of postgresql-ocaml.
    - Supports real prepared statements and connection pooling.
  - SQLite3 (`sqlite3:/`)
    - Implemented in terms of sqlite3-ocaml.
    - Uses preemtive threading to provide non-blocking operation.
    - Connections are not pooled.

Feedback is welcome.

## Documentation

As the main entry point, you would normally use either of

    Caqti_lwt : Caqti_sigs.CONNECT with type 'a io = 'a Lwt.t
    Caqti_async : Caqti_sigs.CONNECT with type 'a io = 'a Deferred.Or_error.t

which is provided by `caqti.lwt` or `caqti.async`, respectively.  These
provide a connect functions which receives an URI, loads the appropriate
backend, and returns a connection as a first-class module containing query
functionality for the database.

The best place to start may be with a [documented example][bikereg].  The
main documentation is the [API Reference][apiref].  Apart from the above
connectors, the most important modules to know are:

  - `Caqti_query` is used for constructing query strings.
  - `Caqti_sigs.CONNECTION` is the interface to a connected database.


[apiref]: http://paurkedal.github.io/ocaml-caqti/
[bikereg]: https://github.com/paurkedal/ocaml-caqti/blob/master/examples/bikereg.ml
