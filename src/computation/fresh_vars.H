#ifndef FRESH_VARS_H
#define FRESH_VARS_H

#include <vector>
#include <string>
#include <optional>

#include "computation/expression/var.H" // for var
#include "computation/haskell/haskell.H" // for Hs::Var, Hs::TypeVar

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
    std::optional<std::string> mod_name;

    Hs::TypeVar get_fresh_type_var(const std::string&, bool meta, const Hs::Kind&);

public:
    var get_fresh_var();

    var get_fresh_var(const std::string& name);
    var get_fresh_var(const var& x);
    var get_fresh_var_copy(var x);
    var get_fresh_var(const Hs::Var& x);

    std::string qualified_name(const std::string&) const;
    Hs::Var get_fresh_Var(const std::string& name, bool qualified);
    Hs::Var get_fresh_Var(const var& x, bool qualified);

    // "Rigid" type vars come from forall-quantified variables.
    // "Wobbly" type vars come from existentially-quantified variables (I think).  We don't have any.
    // "Meta" type vars are unification type vars.
    Hs::TypeVar fresh_rigid_type_var(const Hs::Kind& k);

    Hs::TypeVar fresh_meta_type_var(const Hs::Kind& k);

    Hs::TypeVar fresh_other_type_var();

    int current_index() const;
    int get_index();

    FreshVarSource(FreshVarState& s);
    FreshVarSource(FreshVarState& s, const std::string& mn);
};

var make_var(const Hs::Var& v);

std::vector<var> make_vars(const std::vector<Hs::Var>& vs);

#endif
