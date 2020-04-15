#!/bin/sh

build() {
    # Without that dune would output build informations
    cd ../ && # SC2164
    # Adds && to make sure that no step fails
        dune exec -- ./octo.exe "tests/$1.oc" > /dev/null &&
        cd tests &&
        gcc ../out.c -o o
}

test() {
    build "$1" # First build the program and compare its output with the expected one
    ret=$(./o "$2")
    [ "$3" = "$ret" ] && echo "✓ Test $1 passed" && return 0
    # This will only get execute if the last command failed because there is a return.
    echo "Test $1 failed: expected $3 returned $ret" && exit
}

# Run the tests
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

test "if1" 1 1
test "if1" 2 2
test "if1" 3 3

test "if2" 1 0
test "if2" 2 0
test "if2" 3 0

test "if3" 1 1
test "if3" 2 2
test "if3" 3 3

test "if4" 1 0
test "if4" 2 0
test "if4" 3 0

echo "✓ All tests passed"
# clean the files

rm o ../out.c ../core.h
