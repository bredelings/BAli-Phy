#!/bin/sh
flex -o lexer.cc lexer.l
bison parser.y --defines=parser.hh --output=parser.cc
