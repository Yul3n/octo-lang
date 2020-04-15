#!/bin/sh

build() {
    echo "Building $1 test."
    # Without that dune would output build informations
    cd ../ && # SC2164
    # Adds && to make sure that no step fails
        dune exec -- ./octo.exe "tests/$1.oc" > /dev/null &&
        cd tests &&
        gcc ../out.c -o o &&
        echo "Finished"
}

test() {
    echo "Testing $1"
    build "$1"
    ret=$(./o "$2")
    [ "$3" = "$ret" ] && echo "✓ Test $1 passed" && return 0
    echo "Test $1 failed: expected $3 returned $ret" && exit
}

test "id" 1 1
test "id" 2 2
test "id" 3 3

test "cons1" 1 1
test "cons1" 2 2
test "cons1" 3 3

test "cons2" 1 1
test "cons2" 2 1
test "cons2" 3 1

test "fact" 1 1
test "fact" 2 2
test "fact" 3 6

echo "✓ All tests passed"
rm o out.c
