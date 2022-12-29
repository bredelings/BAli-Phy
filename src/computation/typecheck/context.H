#ifndef TYPECHECKERCONTEXT_H
#define TYPECHECKERCONTEXT_H

#include <string>
#include <vector>
#include "err_context.H"

struct TypeCheckerContext
{
//    yy::location source_span;
    std::vector< Note > notes;
//    std::vector< Hs::Var > binder_stack;
//    int level = 0;

    void pop_note();
    void push_note(const Note& e);
    std::string print_note() const;
};

#endif
