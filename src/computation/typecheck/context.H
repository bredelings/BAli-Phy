#ifndef TYPECHECKERCONTEXT_H
#define TYPECHECKERCONTEXT_H

#include <string>
#include <vector>
#include "message.H"

// Hold with a shared_ptr so that implications can reference this?

struct TypeCheckerContext
{
//    yy::location source_span;
    std::vector< Note > notes;
    int level = 0;
//    local type environment               - Ids and TypeVars defined in this module
//    std::vector< Hs::Var > binder_stack; - 
//    WantedConstraints wanteds;           - the LIE.
//    vector<Message> messages;            - Why local?
    
    void pop_note();
    void push_note(const Note& e);
    std::string print_note() const;

    TypeCheckerContext() = default;
};

#endif
