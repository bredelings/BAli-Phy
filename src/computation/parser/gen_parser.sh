#!/bin/sh
rm -f parser.hh parser.cc lexer.cc
#flex -o lexer.cc lexer.l

bison parser.y --defines=parser.hh --output=parser.cc
reflex -y lexer.l --header-file --bison-complete −−bison-locations −−namespace=yy -d --exception
