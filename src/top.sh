#!/bin/sh

ocamlc -a syntax.ml -o syntax.cma
ocamlc -a lexer.ml -o lexer.cma
ocamlc -a utils.ml -o utils.cma
ocamlc -a parser.ml -o parser.cma
ocamlc -a types.ml -o types.cma
ocamlc -a closure.ml -o closure.cma
ocamlc -a compile.ml -o compile.cma

utop lexer.cma syntax.cma utils.cma parser.cma types.cma compile.cma closure.cma

rm ./*.cm*
