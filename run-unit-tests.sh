#!/bin/sh

if ! test -f scmtap/scm/taptest.scm; then
    printf 'Testing module not available: Skipping unit tests.\n'
    exit 0
elif ! test -f test-dispatch/runtests; then
    printf 'Test-bundle dispatcher not available: Skipping unit tests.\n'
    exit 0
fi

VERBOSE=${VERBOSE:-0}

if test $VERBOSE -ge 1; then
    RUNTESTS_PROVE_VERBOSE=1
    export RUNTESTS_PROVE_VERBOSE
fi
if test $VERBOSE -ge 2; then
    DISPATCH_VERBOSE=1
    export DISPATCH_VERBOSE
fi

ROOT=$PWD

cd t/unit

sh "$ROOT/test-dispatch/runtests" "$ROOT" \
                                  "$ROOT" \
                                  . \
                                  . \
                                  "$ROOT/test-dispatch/dispatch"
exit $?
