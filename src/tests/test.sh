#!/bin/sh

build() {
    # Without that dune would output build informations
    cd ../ && # SC2164
    # Adds && to make sure that no step fails
        dune exec -- ./octo.exe "tests/$1.oc" > /dev/null &&
        cd tests &&
        gcc ../out.c -o o -lm
}

test() {
    build "$1" # First build the program and compare its output with the expected one
    ret=$(./o "$2")
    [ "$3" = "${ret%%.000000}" ] && return 0
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

#             I O I O I O I O
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
test4 "list5" 0 0 1 1 2 2 3 3
test4 "head1" 0 0 1 1 2 2 3 3
test4 "head2" 0 1 1 1 2 1 3 1
test4 "tail1" 0 1 1 1 2 1 3 1
test4 "tail2" 0 0 1 1 2 2 3 3
test4 "fib"   0 0 1 1 2 1 3 2
test4 "case1" 0 1 1 2 2 0 3 7
test4 "case2" 0 1 1 7 2 8 3 9
test4 "casel" 0 1 1 1 2 0 3 1
test4 "empty" 0 0 1 1 2 2 3 3
test4 "map"   0 1 1 1 2 2 3 6
test4 "pow1"  0 1 1 2 2 4 3 8
test4 "fst1"  0 1 1 1 2 1 3 1
test4 "fst2"  0 0 1 1 2 2 3 3
test4 "snd1"  0 1 1 1 2 1 3 1
test4 "snd2"  0 0 1 1 2 2 3 3
test4 "comp"  0 0 1 1 2 2 3 3
test4 "cor"   0 0 1 0 2 0 3 1

echo "✓ All tests passed"

# clean the files
rm o ../out.c ../core.h
