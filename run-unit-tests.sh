#!/bin/sh

if ! test -f scmtap/scm/taptest.scm; then
    printf 'Testing module not available: Skipping unit tests.\n'
    exit 0
elif ! test -f test-dispatch/runtests; then
    printf 'Test-bundle dispatcher not available: Skipping unit tests.\n'
    exit 0
fi

VERBOSE=${VERBOSE:-1}

if test $VERBOSE -ge 1; then
    _options="-verbose"
fi
if test $VERBOSE -ge 2; then
    _options="$_options -dispatch-verbose"
fi
if test $VERBOSE -ge 3; then
    _options="$_options -debug"
fi

ROOT=$PWD

cd t/unit

sh "$ROOT/test-dispatch/runtests" $_options \
                                  -source-dir "$ROOT" \
                                  -binary-dir "$ROOT" \
                                  -dispatch-root "." \
                                  -dispatch-bin-root "." \
                                  -dispatch "$ROOT/test-dispatch/dispatch"
exit $?
