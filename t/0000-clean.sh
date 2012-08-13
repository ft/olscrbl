#!/bin/sh

d="$PWD"
_TEST_SERVER="-b"
. "$d/_setup.sh"
sh "$program" --config "$d/test-config.scm" "$d/slg/scrobbler-a-few.log" \
     > client-stdout.log \
    2> client-stderr.log
_TEST_RETURN=$?
. "$d/_teardown.sh"
exit "$_TEST_RETURN"
