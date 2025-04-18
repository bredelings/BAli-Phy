#include "computation/fresh_vars.H"
#include "computation/haskell/ids.H"
#include "util/myexception.H"
#include "symbols.H"

using std::vector;
using std::string;

string remove_suffix(const string& name)
{
    int i = name.size()-1;
    while(i >=0 and name[i] >= '0' and name[i] <= '9')
        i--;
    if (i > 0 and name[i] == '_')
        return name.substr(0,i);
    else
        return name;
}

string add_suffix(const string& name, int i)
{
    return remove_suffix(name)+"_"+std::to_string(i);
}


string FreshVarSource::qualified_name(const string& uname) const
{
    assert(not is_qualified_symbol(uname));
    if (not mod_name)
        throw myexception()<<"Trying to create qualified name without knowing the module name";
    return (*mod_name) + "." + uname;
}

Hs::Var FreshVarSource::add_mod_name(const Hs::Var& x) const
{
    auto x2 = x;
    x2.name = qualified_name(x2.name);
    return x2;
}

var FreshVarSource::get_fresh_var()
{
    return var( get_index() );
}

var FreshVarSource::get_fresh_var(const std::string& name)
{
    assert(not is_haskell_builtin_con_name(name));
//    assert(name.empty() or not is_qualified_symbol(name));
    return var(name, get_index() );
}

Core2::Var<> FreshVarSource::get_fresh_core_var(const std::string& name)
{
    assert(not is_haskell_builtin_con_name(name));
//    assert(name.empty() or not is_qualified_symbol(name));
    return Core2::Var<>(name, get_index() );
}

Occ::Var FreshVarSource::get_fresh_occ_var(const std::string& name)
{
    assert(not is_haskell_builtin_con_name(name));
//    assert(name.empty() or not is_qualified_symbol(name));
    return Occ::Var(name, get_index());
}

Levels::Var FreshVarSource::get_fresh_levels_var(const std::string& name)
{
    assert(not is_haskell_builtin_con_name(name));
//    assert(name.empty() or not is_qualified_symbol(name));
    return Levels::Var(name, get_index());
}

var FreshVarSource::get_fresh_var(const std::string& name, bool qualified)
{
    assert(not is_haskell_builtin_con_name(name));
    string name2 = add_suffix(get_unqualified_name(name), get_index() );

    if (qualified)
        name2 = qualified_name(name2);

    return var(name2);
}

Core2::Var<> FreshVarSource::get_fresh_core_var(const std::string& name, bool qualified)
{
    assert(not is_haskell_builtin_con_name(name));
    string name2 = add_suffix(get_unqualified_name(name), get_index() );

    if (qualified)
        name2 = qualified_name(name2);

    return Core2::Var<>(name2);
}

var FreshVarSource::get_fresh_var(const var& x)
{
//    assert(x.index >= 0);
    assert(not x.is_exported);
    assert(check_index(x.index));

    return get_fresh_var( x.name );
}

var FreshVarSource::get_fresh_var_copy(var x)
{
//    assert(x.index >= 0);
    assert(not x.is_exported);
    assert(check_index(x.index));

    x.index = get_index();

    return x;
}

Occ::Var FreshVarSource::get_fresh_var_copy(Occ::Var x)
{
//    assert(x.index >= 0);
    assert(not x.is_exported);
    assert(check_index(x.index));

    x.index = get_index();

    return x;
}

Hs::Var FreshVarSource::get_fresh_Var(const std::string& name, bool qualified)
{
    string name2 = add_suffix(get_unqualified_name(name), get_index() );

    if (qualified)
        name2 = qualified_name(name2);

    return Hs::Var(name2);
}

Hs::Var FreshVarSource::get_fresh_Var(const Hs::Var& x, bool qualified)
{
    return get_fresh_Var(x.name, qualified);
}

Hs::Var FreshVarSource::get_fresh_Var(const var& x, bool qualified)
{
//    assert(x.index >= 0);
    assert(not x.is_exported);
    assert(check_index(x.index));

    return get_fresh_Var(x.name, qualified);
}

// "Rigid" type vars come from forall-quantified variables.
// "Wobbly" type vars come from existentially-quantified variables (I think).  We don't have any.
// "Meta" type vars are unification type vars.

MetaTypeVar FreshVarSource::fresh_meta_type_var(int level, const string& name, const Kind& k)
{
    return MetaTypeVar(level, add_suffix(name, get_index() ), k);
}

MetaTypeVar FreshVarSource::fresh_meta_type_var(int level, const Kind& k)
{
    return fresh_meta_type_var(level, "t", k);
}

TypeVar FreshVarSource::fresh_rigid_type_var(int level, const string& name, const Kind& k)
{
    return TypeVar(level, add_suffix(name, get_index() ), k);
}

TypeVar FreshVarSource::fresh_rigid_type_var(int level, const Kind& k)
{
    return fresh_rigid_type_var(level, "t", k);
}

bool FreshVarSource::check_index(int i) const
{
    return state.check_index(i);
}

int FreshVarSource::get_index()
{
    return state.get_index();
}

FreshVarSource::FreshVarSource(FreshVarState& s)
    :state(s)
{ }

FreshVarSource::FreshVarSource(FreshVarState& s, const string& mn)
    :state(s), mod_name(mn)
{ }

var make_var(const Hs::Var& v)
{
    assert(v.wrap.is_identity());
    var v2(v.name);
    
    return v2;
}

Core2::Var<> make_core_var(const Hs::Var& v)
{
    assert(v.wrap.is_identity());
    Core2::Var v2(v.name);

    return v2;
}

vector<var> make_vars(const vector<Hs::Var>& vs)
{
    vector<var> vs2;
    for(auto& v: vs)
        vs2.push_back(make_var(v));
    return vs2;
}

