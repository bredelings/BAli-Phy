#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "computation/parser/haskell.H"

// OK, so we need to be able to typecheck a MODULE. probably.
// We need to be able to translate a module's (i) classes, types, instances, and values to something that can be exported, eventually.
// We need to be able to translate the values to... a binding where all the variables have types, or something?

class Module;
void typecheck(const Module& m, const Hs::ModuleDecls& M);
#endif
