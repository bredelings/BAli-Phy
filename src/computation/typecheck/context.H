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
#include "expected.H"

typedef immer::map<Hs::Var, std::pair<Hs::Var, Type>> mono_env_t;


struct IDType
{
    Hs::Var var;
    Type type;
};

struct IDExpType
{
    Hs::Var var;
    Expected exp_type;
};

typedef std::variant<IDType, IDExpType> binder_info;

struct TypeCheckerContext
{
    std::vector<yy::location> locs;
    int level = 0;
    mono_env_t mono_env;
    value_env poly_env;

    Notes notes;
    std::vector< std::variant<IDType,IDExpType> > binder_stack;
//    WantedConstraints wanteds;           - the LIE.
//    vector<Message> messages;            - local errors could become global warnings, or be dropped...
    
    void push_source_span(const yy::location&);
    void pop_source_span();
    std::optional<yy::location> source_span() const;

    void pop_note();
    void push_note(const Note& e);
    std::string print_note() const;

    void push_binder(const binder_info& binder);
    void pop_binder();

    TypeCheckerContext() = default;
};

#endif
