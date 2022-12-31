#ifndef TYPECHECKERCONTEXT_H
#define TYPECHECKERCONTEXT_H

#include <string>
#include <vector>
#include "message.H"
#include <utility>
#include "computation/haskell/coretype.H"
#include "computation/haskell/haskell.H"
#include "immer/map.hpp" // for immer::map

// Hold with a shared_ptr so that implications can reference this?

typedef immer::map<std::string, std::pair<Hs::Var, Type>> mono_env_t;

struct TypeCheckerContext
{
//    yy::location source_span;
    int level = 0;
    mono_env_t mono_env;
    std::vector< Note > notes;
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
