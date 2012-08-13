#!/bin/sh

if test -f test-server.pid; then
    read server_pid < test-server.pid
    kill $server_pid
    rm -f test-server.pid
fi
cd ..
if test "$_TEST_RETURN" = 0; then
    test -d twd && { rm -f twd/*;
                     rmdir twd; }
else
    printf 'Test failed, leaving `twd'\'' intact.\n'
fi
