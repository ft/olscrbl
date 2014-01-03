#!/bin/sh

got_program () {
    oldifs_="$IFS"
    IFS=:
    found_=0
    for p in $PATH; do
        if [ -x "$p/$1" ]; then
            found_=1
            break
        fi
    done
    IFS="$oldifs_"
    [ "$found_" = 1 ] && return 0
    return 1
}

got_module () {
    guile --no-auto-compile -q -c "(use-modules $1)" 2> /dev/null
}

if ! got_module "(test tap)"; then
    printf 'Testing module not available: Skipping unit tests.\n'
    exit 0
elif ! got_program run-tests; then
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

sh "run-tests" $_options \
               -source-dir "$ROOT" \
               -binary-dir "$ROOT" \
               -dispatch-root "." \
               -dispatch-bin-root "."
exit $?
