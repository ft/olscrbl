#!/bin/sh

if test x"$1" = "x-x"; then
    opts=-x
else
    opts=
fi

test -d logs && rm -Rf logs
mkdir -p logs
touch logs/all.stamp

for i in [0-9][0-9][0-9][0-9]-*.sh; do
    printf ' * %s...\n' "$i"
    sh $opts "$i" || exit
done
exit 0
