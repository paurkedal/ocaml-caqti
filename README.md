# Cooperative-threaded Access to Relational Data

Caqti is an interface to data sources which accept a parametrised query
string and responds with a sequence of tuples.  It is especially aimed at
relational databases, where SQL is the common query language.  Caqti may be
used directly or as an intermediate layer between database client libraries
and high-level solutions like code generators and syntax extensions.

The interface abstracts over an IO monad to support cooperative threading.
Currently only Lwt is implemented.  A functor is available for instantiating
other IO monads.

## Status

The library is under development; expect backwards incompatible API changes.
Backends are implemented for

  - PostgreSQL (`postgresql:/`)
    - Implemented in terms of postgresql-ocaml.
    - Connect is blocking, execute is non-blocking.
    - Supports real prepared statements and connection pooling.
  - SQLite3 (`sqlite3:/`)
    - Implemented in terms of sqlite3-ocaml.
    - Uses preemtive threading to provide full non-blocking operation.
    - No connection pooling, reconnects on each execute.

Feedback is welcome.

## Documentation

The main documentation is the
[API Reference](http://paurkedal.github.io/ocaml-caqti/).
For the main entry point, you would normally use either of

    Caqti_lwt : Caqti_sigs.CONNECT with type 'a io = 'a Lwt.t
    Caqti_async : Caqti_sigs.CONNECT with type 'a io = 'a Deferred.Or_error.t

which is provided by `caqti.lwt` or `caqti.async`, respectively.  The former
is the most well-tested.
