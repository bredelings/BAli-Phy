#ifndef TYPECHECKERCONTEXT_H
#define TYPECHECKERCONTEXT_H

#include <string>
#include <vector>
#include "message.H"
#include <utility>
#include "computation/haskell/coretype.H"
#include "computation/haskell/haskell.H"
#include "immer/map.hpp" // for immer::map
#include "env.H"

typedef immer::map<std::string, std::pair<Hs::Var, Type>> mono_env_t;

struct TypeCheckerContext
{
    std::optional<yy::location> source_span;
    int level = 0;
    mono_env_t mono_env;
    value_env poly_env;

    std::vector< Note > notes;
//    std::vector< Hs::Var > binder_stack; - 
//    WantedConstraints wanteds;           - the LIE.
//    vector<Message> messages;            - local errors could become global warnings, or be dropped...
    
    void pop_note();
    void push_note(const Note& e);
    std::string print_note() const;

    TypeCheckerContext() = default;
};

#endif
