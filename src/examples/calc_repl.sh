#!/bin/sh -e

while true;
do
    read -r r
    O=$(./o "$r")
    echo "${O%%.000000}"
done
