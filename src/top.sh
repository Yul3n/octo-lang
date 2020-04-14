#!/bin/sh

ocamlc -a syntax.ml -o syntax.cma
ocamlc -a utils.ml -o utils.cma
ocamlc -a lexer.ml -o lexer.cma
ocamlc -a parser.ml -o parser.cma
ocamlc -a types.ml -o types.cma
ocamlc -a core.ml -o core.cma
ocamlc -a closure.ml -o closure.cma
ocamlc -a compile.ml -o compile.cma

utop syntax.cma utils.cma lexer.cma parser.cma types.cma core.cma closure.cma compile.cma

rm ./*.cm*
