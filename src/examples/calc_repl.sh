#!/bin/sh -e

while true;
do
    echo -n "> "
    read -r r
    O=$(./o "$r")
    echo "${O%%.000000}"
done
