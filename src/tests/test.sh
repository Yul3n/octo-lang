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
    [ "$3" = "$ret" ] && return 0
    # This will only get execute if the last command failed because there is a return.
    echo "Test $1 failed: expected $3 returned $ret" && exit
}

test4() {
    test "$1" "$2" "$3" &&
        test "$1" "$4" "$5" &&
        test "$1" "$6" "$7" &&
        test "$1" "$8" "$9" &&
        echo "✓ Test $1 passed"
}

# Run the tests
# The couples of argument represent the argument and the expected output

test4 "id"    0 0 1 1 2 2 3 3
test4 "cons1" 0 0 1 1 2 2 3 3
test4 "cons2" 0 1 1 1 2 1 3 1
test4 "fact"  0 1 1 1 2 2 3 6
test4 "if1"   0 0 1 1 2 2 3 3
test4 "if2"   0 0 1 0 2 0 3 0
test4 "if3"   0 0 1 1 2 2 3 3
test4 "if4"   1 0 1 0 2 0 3 0
test4 "list1" 0 0 1 1 2 2 3 3
test4 "list2" 0 0 1 1 2 2 3 3
test4 "list3" 0 0 1 1 2 2 3 3
test4 "list4" 0 0 1 0 2 0 3 0

echo "✓ All tests passed"

# clean the files
rm o ../out.c ../core.h
