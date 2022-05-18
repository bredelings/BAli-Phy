#ifndef FRESH_VARS_H

#include <vector>
#include <string>

#include "computation/expression/var.H" // for var
#include "computation/parser/haskell.H" // for Hs::Var

class fresh_vars
{
    int var_index = 1;

public:
    var get_fresh_var();

    var get_fresh_var(const std::string& name);
    var get_fresh_var(const var& x);
    var get_fresh_var(const Hs::Var& x);

    Hs::Var get_fresh_Var(const std::string& name);
    Hs::Var get_fresh_Var(const var& x);

    int current_index() const {return var_index;}
    int get_index() {return var_index++;}
};

var make_var(const Hs::Var& v);

std::vector<var> make_vars(const std::vector<Hs::Var>& vs);

#endif
