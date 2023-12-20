## v2.1.0 - unreleased

  - The networking code (for PGX) has been revised to support TLS.  The TLS
    implemantion is shipped in separate packages (`caqti-tls*`).  The
    revision also adds buffering for EIO, which significatly improves
    performarce.

  - Connect functions now accept an optional configuration.  For the time
    being, this is used for TLS parameters and for the `tweaks_version`
    setting.

  - The newly introduced `Caqti_query.qprintf` and associated formatters
    have been moved to a separate module `Caqti_query_fmt` (#108), to limit
    clashes when used in local open, and since the formatters were nominally
    underqualified.

  - Constants of any field type can now be embedded in queries using the new
    `Caqti_query.V` constructor and associated shortcuts.  This allows
    taking advantage of driver-specific encoders, e.g. to correctly convert
    a `Ptime.Span.t` to the representation expected by the target database
    system.

  - Equality with type unification evidence is now available for
    `Caqti_type.t` and `Caqti_type.Field.t`.  The equality type introduced
    for the purpose is compatible with `Type.eq` from OCaml 5.1, but will
    not be aliased yet.  This required a backwards incompatible adjustment
    of the `Caqti_type.Product` constructor.

  - Some updates to external dependencies, esp. we are now at EIO 0.12.

## v2.0.1 - 2023-08-14

Fixes:

  - Upgrade Eio to 0.11.
  - Update README and make a note about caqti-eio and caqti-mirage being
    experimental.
  - Fix build and packaging issues.

This release was sponsored by the OCaml Software Foundation.

## v2.0.0 - 2023-08-10

New features:

  - Added driver based on the pure-OCaml PGX library (#38).  However, due to
    lack of TLS support it is not suitable for production, unless you have a
    dedicated secure network.

  - Added support for MirageOS.  This only works for the PGX driver, since
    the other drivers link against external C libraries.

  - Added experimental support for Eio, both pure-OCaml and Unix.  The
    former supports PGX while the latter supports all drivers.

  - Implemented single-row mode for PostgreSQL (#24), but while this avoids
    uncontrolled memory consumption when a large number of rows is returned,
    it is a lot less efficient.

  - A new pool connection parameter `?max_idle_age` provides removal
    resources from pools which have been unused for the given period.

  - Printf-style function `Caqti_query.qprintf` for dynamic queries
    (GPR#103, Basile Clément).

  - Added general-purpose product type to provide functionality covering
    both product and custom types.

  - Added new tuple types `t3`, ..., `t8` based on the new product type and
    deprecated `tup2`, `tup3`, `tup4` which are now aliases for these.

Breaking changes:

  - The minimal OCaml requirement is now 4.08.0.

  - The connection functions previously found in `caqti-lwt` has been moved
    into a sublibrary `caqti-lwt.unix` as part of the addition of
    `caqti-mirage`, whereas shared functionality remains in `caqti-lwt`.
    The LWT connection function is therefore now found in `Caqti_lwt_unix`.

  - Removed all deprecated functionality and a few unintended or
    undocumented definitions like the type `Caqti_blocking.future` and
    `Caqti_driver_info.describe_has_typed_*`.

  - Reworked the driver API to accommodate the addition of the PGX driver
    and MirageOS and Eio support, and to avoid exposting internal interfaces
    in the main library.  These modules are now found in the sublibraries
    `caqti.platform` and `caqti.platform.unix`, depending on their external
    depnedencies.  They are not meant for use by applications, though this
    might not have fully clear previously.

  - Removed constructors Custom, Unit, Tup2, Tup3, Tup4 from `Caqti_type.t`.
    They were private but could in prinicle be matched against.

  - The extensibility of `Caqti_type.Field.t` has been dropped along with
    the associated functions for registering conversions.  This means that
    there are no custom field types any more, only custom row types.  The
    purpose of custom field types where to support non-standard types for
    3rd party drivers; we should coordinate additions to the core type
    instead if needed.

  - According to the above point, the `caqti-type-calendar` is reimplemented
    as a row type, which means the `Cdate` and `Ctime` constructors are
    gone.

  - There are adjustments for the pretty printer for types.  (The context is
    assumed to start at the lowest precedence, meaning parentheses around
    lowest-precedence expressions are dropped.  Also, `redacted` is shown
    with suffix notation, allowing us to remove one precedence level.)

  - Moved pool tuning parameters into a configuration object.

Fixes:

  - Fixed a missing `Preemptive.detach` call for `to_stream` for Sqlite3.

  - Incorrect error reporting related to type conversions were discovered by
    exhaustiveness checks when making field types non-extensible.

Other:

  - The documentation of `call` now makes clear that the result *must* be
    retrieved in order to make sure the request is performed.  This is made
    relevant by the addition of the PGX driver where request and retrieval
    are fused.

  - Customized the top-level index and some other documentation work.
    Removal of private modules from the main library should also help to
    make the documentation more accessible.

  - Added a benchmark to measure row decoding performance.

  - Improved decoding performance by partially applying the type descriptor.

This release was sponsored by the OCaml Software Foundation.

## v1.9.1 - 2022-09-21

This release only updates the caqti-driver-postgresql package, with the
following fixes:

  - PostgreSQL expects "=" in query values to be URI-encoded (#95).

  - The validation logic for PostgreSQL was missing a call to consume
    available input, meaning that broken pooled connections were reused.

This release was sponsored by the OCaml Software Foundation.

## v1.9.0 - 2022-09-06

New features:

  - Allow unquoted semicolons in query strings in the new API.  There are
    corner cases where it is needed, as reported in issue #87, and a parser
    which rejects semicolons are still available for loading schema files
    statement by statement.

  - Add support for MySQL and MariaDB configuration files, as a solution to
    issue #86.

  - Add a limit to the number of times a database connection is reused when
    pooling connections (#94).  Thanks to Peter Mondlock for investigating
    resource usage server side motivating this addition.

  - Provide access to the raw SQLite3 connection handle for the purpose of
    defining custom functions (#56).

Fixes:

  - Add missing dune dependency on unix (GPR#85 by David Allsopp).

  - Documentation fixes (GPR#82, GPR#83, GPR#84 by Reynir Björnsson,
    GPR#88 by Jonathan Duarte, and GPR#92 by Jim Tittsler).

Deprecations:

  - `Caqti_type.field` was deprecated in favour of `Caqti_type.Field.t`.

Other:

  - Replace deprecated core\_kernel dependency with core.

This release was sponsored by the OCaml Software Foundation.

## v1.8.0 - 2022-03-24

New features:

  - A matchable representation of common causes of errors on the database
    side is now available, with limitations.  It focuses on conditions which
    seem most likely useful to handle.  At the moment we lack extended error
    codes from SQLite3 needed to make the cause fully precise.

  - Expose the underlying error details from database client libraries.
    This is meant to be use as a last resort, and requires directly linking
    with the relevant drivers.

  - A second set of request construction operators `->.`, `->?`, `->!`, and
    `->*` were introduced after experience with converting existing code.
    Given the parameter and result type they return a function which
    constructs a request directly from a query string.  Avoiding the need to
    compose with `@:-` simplifies local opens and usage with `List.map` etc.

  - Environment variables are now expanded in the debug log when using the
    new request constructors introduced in 1.7.0.

  - A new `?tweaks_version` connection parameter has been added to control
    when the client is ready to adapt to changes in database session
    parameters or other adjustments of the interaction with specific
    database systems.

  - Enable foreign key constraint checks for SQLite3 starting at tweaks
    version 1.8.

Fixes:

  - Fixed debug logging to pass the correct driver info to the query
    callback instead of a dummy driver info which would cause a failure if
    unsupported.

Deprecations:

  - The `-->` operator was renamed to `-->!`, with a deprecated alias, for
    consistency with the new `->!` operator.

  - The old convenience interface for creating requests has been deprecated
    in favour of the new infix operators and the new query template parser.

  - Documented-only deprecations of `Caqti_sql_io`, `Caqti_lwt_sql_io`, and
    `Caqti_async_sql_io` have been annotated.

This release was sponsored by the OCaml Software Foundation.

## v1.7.0 - 2022-02-13

New features:

  - Added a new query-string parser based on Angstrom.  Its main advantage
    is that it can be used stand-alone e.g. to load SQL schemas from files
    split into individual statements which can be sent to the database.

  - Support passing a query environment to the connect and pool construction
    functions.  This avoids using globals to modify the environment and
    facilitates e.g. targeting different database schemas with different
    connetions or connection pools.

  - A new `Caqti_query.E` case was added to support the two above cases.
    This is a breaking change, but hopefully does not break existing code.
    The exception (not counting `module type of`) would be if the
    constructors are aliased along with an alias of the type.  On the other
    hand, pattern matching should be compatible since the new constructor is
    only emitted by newly introduced functions.

  - Added a module `Caqti_request.Infix` providing a new high-level API for
    constructing requests.  It uses two-stage combinators, first to apply
    type arguments then to apply the query string.  Apart from allowing
    nicer looking code, this solves the problem of switching to a the query
    string parser while maintaining backwards compatibility.

  - Added `set_statement_timeout` to connection modules, used to set query
    timeout.  It is only supported for PostgreSQL and MariaDB. (#74)

  - Added `with_transaction` function to connection modules.  This is just
    fail-safed wrapper around `start`, `commit`, and `rollback`.

Fixes:

  - Fixed delayed recovery of connection pools after loosing connections to
    PostgreSQL.  Due to a missing call to consume inputs, the pool validator
    did not discover that a connection was lost before it was attempted
    reused.

  - Improved resilience against exceptions and monadic errors in callbacks
    and fixed in-use checking.

  - Changed Sqlite3 driver to use non-linear parameters and improve error
    reporting when the wrong number of arguments are received (#77 & GPR#79
    Reynir Björnsson).

Other changes:

  - The Caqti license now uses the LGPL-3.0 Linking Exception instead of a
    rephrasing of the OCaml LGPL Linking Exception which was written for
    LGPL-2.1.

  - Improved documentation, pretty-printing, logging, and exception details.

  - The test suite has been switched to use Alcotest; not without some
    struggle due to the use of test harness and argument-dependent test
    suite, but the result seems like a clear improvement.

This release was sponsored by the OCaml Software Foundation.

## v1.6.0 - 2021-06-10

- Set the time zone of PostgreSQL connections to UTC to mitigate an
  undesirable implicit conversion to the local time zone for `timestamp`.
  This issue was exposed by the specification of field types introduced in
  version 1.4.0.  Earlier versions worked as expected, if only accidentally,
  since the time zone is ignored when a string is converted to a
  `timestamp`.  While this change makes `timestamp` more usable again for
  storing UTC time stamps, I strongly recommend using `timestamp with time
  zone` since it's interpretation is unambiguous.  The API reference is now
  updated with details about how the `ptime` OCaml type is mapped for
  different database systems.
- Drop specification of OCaml `string` as SQL `text` for PostgreSQL. This is
  due to issues with implicit conversions and function overloading when the
  desired type on the SQL side is `char`, `varchar`, or `jsonb`.
- Add `Caqti_type.redact` to protect sensitive information from being
  logged.
- Only log parameters if `$CAQTI_DEBUG_PARAM` is set to "`true`".
- When logging requests, show underlying values for custom types.
- Reject multi-row response in `find_opt` implementation for sqlite3.
- Tolerate Lwt promise rejections in `Pool.use`.

## v1.5.1 - 2021-04-18

- Fix option recognition in PostgreSQL driver (GPR#67 mefyl).
- Fix option recognition in MariaDB driver and add test (Petter A. Urkedal).

## v1.5.0 - 2021-04-11

- Request the full UTF-8 character for the MariaDB connection.
- Support int16 and enum types for parameters and rows.

## v1.4.0 - 2021-03-11

- Fix infinite loop when deserializing an optional tuple (GPR#63 mefyl).
- Add `Caqti_connect_sig.S.with_connection` (GPR#61 Anton Bachin).
- Pass parameter types to PostgreSQL prepare and query functions. This
  avoids the need to CAST parameters on the SQL side in some cases.
- Add `?post_connect` callback to `connect_pool`.
- Documentation fixes and improvements (Aaron L. Zeng, Anton Bachin, Petter
  A. Urkedal).

## v1.3.0 - 2021-01-17

- Implement `affected_count` for sqlite backend (GPR#46 jakob).
- Add method `exec_with_affected_count` to `Caqti_connection_sig.S` (GPR#45
  jakob).
- Add `?max_idle_size` to pool creation functions.
- Dropped dependency on `ppx_deriving` due to issue with static compilation
  (GPR#50 Ulrik Strid).
- Pass through `$<var>$` in query strings and deprecate `$$`.
- Log statements to be executed at debug level.
- Add COPYING.OCAML and fix license expression in opam files.
- Misc improvements to tests and documentation (GPR#51 Philippe Wang, GPR#54
  Reynir Björnsson, etc.).

## v1.2.4 - 2020-06-07

- Switch to TEXT format for PostgreSQL populate implementation.

## v1.2.3 - 2020-06-02

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
