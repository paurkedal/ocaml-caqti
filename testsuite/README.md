## Main Testsuite

This directory contains tests to be run across different concurrency engines
and across different database drivers.  Additional tests can be found in the
`test` subdirectories of some per-package subdirectories.

By default the tests are only run against sqlite3.  To run it against other
database systems, create a file `uris.conf` in the current directory
containing a list of database URLs, one per line.  This will cause each test
executable to run a duplicate of the testsuite for each URL.
