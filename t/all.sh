#!/bin/sh

for i in [0-9][0-9][0-9][0-9]-*.sh; do
    printf ' * %s...\n' "$i"
    sh "$i" || exit
done
exit 0
