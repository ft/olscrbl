#!/bin/sh

_test_call_program () {
    sh "$program" "$@"  > client-stdout.log \
                       2> client-stderr.log
    _TEST_RETURN=$?
}

if ! test -f logs/all.stamp; then
    test -d logs && rm -Rf logs
    mkdir -p logs
fi

rootdir=$(cd ..; pwd)
program="$rootdir/bin/olscrbl"
GUILE_LOAD_PATH="${rootdir}/scheme"
GUILE_AUTO_COMPILE=0
export GUILE_LOAD_PATH
export GUILE_AUTO_COMPILE
mkdir -p twd
cd twd
[ -n "$_TEST_SERVER" ] && perl ../test-server $_TEST_SERVER  > test-server.pid \
                                                            2> test-server.out
