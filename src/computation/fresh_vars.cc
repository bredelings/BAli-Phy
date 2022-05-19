#include "computation/fresh_vars.H"
#include "computation/module.H"

using std::vector;
using std::string;

var FreshVarSource::get_fresh_var()
{
    return var( get_index() );
}

var FreshVarSource::get_fresh_var(const std::string& name)
{
    assert(not is_haskell_builtin_con_name(name));
    return var(name, get_index() );
}

var FreshVarSource::get_fresh_var(const var& x)
{
    assert(not x.is_exported);
    assert(x.index <= current_index());
    return get_fresh_var( x.name );
}

Hs::Var FreshVarSource::get_fresh_Var(const std::string& name) {
    string name2 = name + "@" + std::to_string( get_index() );
    return Hs::Var({noloc,name2});
}

Hs::Var FreshVarSource::get_fresh_Var(const var& x)
{
    assert(not x.is_exported);
    assert(x.index <= current_index());

    return get_fresh_Var(x.name);
}

int FreshVarSource::current_index() const
{
    return state.current_index();
}

int FreshVarSource::get_index()
{
    return state.get_index();
}

FreshVarSource::FreshVarSource(FreshVarState& s)
    :state(s)
{ }

var make_var(const Hs::Var& v)
{
    return var(unloc(v.name));
}

vector<var> make_vars(const vector<Hs::Var>& vs)
{
    vector<var> vs2;
    for(auto& v: vs)
        vs2.push_back(make_var(v));
    return vs2;
}

