#!/bin/sh -e
dune build ./src/octo.exe
cp _build/default/src/octo.exe /usr/bin/octo
cp -r src/lib /usr/lib/octo/
echo "Done"
