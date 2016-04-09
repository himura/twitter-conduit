#!/bin/bash

BASEDIR="$PWD"
TARGET="$1"
DIST="$(stack path | awk '/^dist-dir/ {print $2}')"

stack sdist

if [ ! -f ${DIST%/}/${TARGET}-*.tar.gz ]; then
    echo "NG: missing archive"
    exit 1
fi

cd "$DIST"
srctgz=(${TARGET}*.tar.gz)
srctgz="${srctgz[0]}"
pkgname="${srctgz%.tar.gz}"
tar xvzf "${srctgz}"
cd "$BASEDIR"
NG=$(git ls-tree -r HEAD | while read perm blob hash name; do [ -e "$DIST/$pkgname/$name" ] || echo "$name"; done)
if [ -n "$NG" ]; then
    echo "Missing files:"
    echo $NG
    exit 1
fi
