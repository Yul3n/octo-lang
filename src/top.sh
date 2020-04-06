#!/bin/sh

ocamlc -a syntax.ml -o syntax.cma
ocamlc -a lexer.ml -o lexer.cma
ocamlc -a utils.ml -o utils.cma
ocamlc -a parser.ml -o parser.cma

utop lexer.cma syntax.cma utils.cma parser.cma

rm ./*.cm*
