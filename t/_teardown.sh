#!/bin/sh

_test_backup () {
    from=$1; shift
    to=$1; shift

    for file in "$from"/*; do
        to_file="$to/${file##*/}"
        cat "$file" >> $to_file
    done
}

if test -f test-server.pid; then
    read server_pid < test-server.pid
    kill $server_pid
    rm -f test-server.pid
fi
cd ..
if test "$_TEST_RETURN" = 0; then
    test -d logs && _test_backup twd logs
    test -d twd && { rm -f twd/*;
                     rmdir twd; }
else
    printf 'Test failed, leaving `twd'\'' intact.\n'
fi
