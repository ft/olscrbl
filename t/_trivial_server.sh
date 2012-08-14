#!/bin/sh

d="$PWD"
. "$d/_setup.sh"
_test_call_program --config "$d/test-config.scm" "$d/slg/scrobbler-a-few.log"
. "$d/_teardown.sh"
exit "$_TEST_RETURN"
