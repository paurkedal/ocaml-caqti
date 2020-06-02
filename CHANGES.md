## v1.2.3 - 2020-06-07

- Silence PostgreSQL notifications which were printed to stderr.  They can
  be re-enabled with a parameter on the connection URI (#33).
- Improve details in exceptions in two places.
- Fix info about connection caching for sqlite in README (#35, dgllghr).

## v1.2.2 - 2019-12-19

- Update for core v0.13 (GPR#31 Xavier Clerc).

## v1.2.1 - 2019-12-08

- Support TEXT literals in queries.
- Fix decoding of binary results for PostgreSQL (thanks to James Owen).
- Implement copy-mode for PostgreSQL (GPR#30 James Owen).

## v1.2.0 - 2019-10-02

- Add a signature for the populate function, and add basic implementations
  for all drivers (GPR#28).
- Add stream for result extraction (GPR#22 Nathan Rebours).
- Use the postgres driver when `postgres` is specified as the URL scheme
  (GPR#25 James Owen).
- Documentation improvements (GPR#20 Nathan Rebours, etc.).
- Reimplemented partly ineffective prepare-cache for PostgreSQL.
- Backwards incompatible changes to the driver API.
- Backwards incompatible changes to modules marked internal but exposed due
  to being split across packages.
- Fix forward compatibility past OCaml 4.08 as announced by deprecations.

## v1.1.0 - 2019-02-03

- Add pretty printer for requests.
- Add variance to `'a future` declarations.
- Add blocking instance of API.
- Generalize `$.` to `$<var>.` in queries.
- Infer the expansion of `$(<var>.)` from `$(<var>)` if not provided.
- Fix connection recovery for PostgreSQL (issue #19, Dave Aitken).
- Fix some unhandled exceptions for PostgreSQL.
- Fix connection validation for MariaDB.

## v1.0.0 - 2018-08-26

- Added `Caqti_type.Std : Caqti_type_sig.Std`, containing type descriptors
  needed for building requests, for easy inclusion in custom modules.
- Fix ptime to string conversion for PostgreSQL (monstasat).
- Support microsecond precision for MariaDB.
- Removed `Caqti_type.Field.ex` and renamed `Caqti_type.ex` to
  `Caqti_type.any` and related functions.
- Removed other deprecated definitions.
- Moved `Caqti_system_sig` into `Caqti_driver` and split up the signature to
  make room for future drivers on alternative platforms.

## v0.11.0 - 2018-05-25

Added and improved:

- Compliance with Lwt 4.0 contributed by Brendan Long.
- Switched to the logs library for logging.
- Strengthen detection of concurrent use of connection.
- Support microsecond precision for timestamps for PostgreSQL and
  millisecond for Sqlite3.
- Use float for time spans in Sqlite3.
- Map the `octets` type to BLOBs for MariaDB and Sqlite3.
- Log recoverable errors for MariaDB and Sqlite3.

Removed:

- The v1 API is now removed.

Ecosystem:

- CI setup contributed by Brendan Long.
- Various documentation updates.

## v0.10.2 - 2018-04-05

- Fix incompatibility with `ppx_optcomp` v0.11.0 by dropping it, since the
  conditionals where no longer needed.
- Restore test during opam build.
- The v1 API is now fully `[@@@deprecated]` and will be removed in the next
  major release.

## v0.10.1 - 2018-02-27

- Fix a pool size counting bug when resource allocation fails.  This can
  cause deadlock due to pool exhaustion.

## v0.10.0 - 2018-02-14

- Added `-linkall` flags to driver libraries to fix direct linking (#9).
- Added convenience functions `collect_list` and `rev_collect_list` (#8).
- Renamed `template` to `query` and related function, leaving deprecated
  aliases.
- Added `ptime_span` field type mapping to SQL intervals.
- Be more permissive about types of data returned from MariaDB when
  expecting numerical results.

## v0.9.0 - 2018-01-04

- Move v1 into findlib sublibraries and announce deprecation.
- Port remaining tests to v2 and fix issues in MariaDB and Sqlite3 drivers.
- Don't include password component of URIs in error messages.
- Rename `io` type constructor to `future`.
- Fix pool size limit enforcement.
- Drop error on disconnect.
- Support substitutions in request convenience functions.

## v0.8.1 - 2017-12-06

- Fix `start` transaction for PostgreSQL v2.
- Fix dependency on ptime in `caqti.opam` in distribution.

## v0.8.0 - 2017-12-04

- Remove most deprecations.
- Add new v2 API.
- Move away most of the v1 API, but keep deprecated aliases for the client
  API.

The main difference of the new API compared to the old is:

- It uses typed parameters and rows with an extensible variant for field
  type and a custom definable compound type.
- The connection has a `call` method which gives access to a result object
  before iterating over the returned rows.
- Error handling is done with the `result` type, using a collection of error
  types formed as polymorphic variants of records.

*Notice:* This version is mostly backwards compatible.  New code should use
v2, but v1 will be kept for now.  To keep using the v1 in upcoming releases:

- Fix the new deprecations from this release.
- Link with `caqti-dynload` to keep using dynamic loading, or link with the
  needed `caqti-driver-*.v1` driver libraries.
- Link with `caqti.v1` instead of `caqti`.
- Link with `caqti-lwt.v1` instead of `caqti-lwt`.
- Link with `caqti-async.v1` instead of `caqti-async`.

## v0.7.0 - 2017-11-03

- Remove WRAPPER and REPORT functionality.
- Remove deprecated findlib aliases.
- Renamed `date` and `utc` conversions to `date_cl` and `utc_cl`.
- Deprecate `Caqti_metadata` in favour of the new `Caqti_driver_info`.
- Move various signature to separate files, leaving deprecated aliases.

## v0.6.1 - 2017-10-26

- Fixed support for lwt >= 3, but keep supporting 2.7 as well for now.
- Fixed preparation of query after reconnect to PostgreSQL.
- Fixed support for ocaml 4.03.x.
- Added `Tuple.length`.
- Deprecated `Param.sub_types`.
- Deprecated `Param.other` and `Tuple.other`.

## v0.6.0 - 2017-06-05

- Split into separate packages to avoid depopts and rename findlib names
  accordingly.
- Use findlib to load DB drivers, and use `-linkall`. This hopefully solves
  loading issues.

## v0.5.3 - 2017-05-22

- Fix handling of spaces in Sqlite3 URIs and allow setting mode.
- Split `Caqti` (now deprecated) into `Caqti_errors` and `Caqti_connect`.
- Update to async 0.9.0.

## v0.5.2 - 2017-02-25

- Fix plugin loader to handle modernised `META`.

## v0.5.1 - 2017-02-25 retracted

- Rename libraries to match `META`.

## v0.5.0 - 2017-02-25 retracted

- Added MariaDB backend.
- Fixed VARCHAR for SQLite3 describe.
