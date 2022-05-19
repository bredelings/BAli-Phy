#ifndef FRESH_VARS_H
#define FRESH_VARS_H

#include <vector>
#include <string>

#include "computation/expression/var.H" // for var
#include "computation/parser/haskell.H" // for Hs::Var

class FreshVarState
{
    int var_index = 1;

public:

    int current_index() const {return var_index;}
    int get_index() {return var_index++;}
};

class FreshVarSource
{
    FreshVarState& state;

public:
    var get_fresh_var();

    var get_fresh_var(const std::string& name);
    var get_fresh_var(const var& x);
    var get_fresh_var(const Hs::Var& x);

    Hs::Var get_fresh_Var(const std::string& name);
    Hs::Var get_fresh_Var(const var& x);

    int current_index() const;
    int get_index();

    FreshVarSource(FreshVarState& s);
};

var make_var(const Hs::Var& v);

std::vector<var> make_vars(const std::vector<Hs::Var>& vs);

#endif
