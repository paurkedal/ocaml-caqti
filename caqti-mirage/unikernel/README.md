## Synopsis

This directory provides a MirageOS Unikernel which performs a simple query
against a PostgreSQL database using the PGX driver.

## Building

This directory is not integrated in the normal Caqti build.  A regular
executable can be built with
```console
$ opam switch create . 4.14.0
$ export OPAMSWITCH=./mirage
$ eval `opam config env`
$ opam install mirage
$ mirage configure -t unix
$ opam install --deps-only ./mirage
$ mirage build
```
See `mirage configure --help` for other targets (`-t`).

## Running

You can start a throw-away PostgreSQL instance in a Docker container with
```console
$ docker run --rm -it -e POSTGRES_PASSWORD=KWRIsr6TPjBX -p 127.0.0.1:15432:5432 postgres:13
```
and in a parallel shell run
```console
$ _build/default/caqti-test-unikernel --database-uri pgx://postgres:KWRIsr6TPjBX@127.0.0.1:15432
```
Other targets may require a hypervisor and direct access to a network
interface.  Documentation can be found on the [MirageOS
site](https://mirage.io/).
