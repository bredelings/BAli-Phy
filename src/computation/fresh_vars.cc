#include "computation/fresh_vars.H"
#include "computation/haskell/ids.H"
#include "util/myexception.H"

using std::vector;
using std::string;


string FreshVarSource::qualified_name(const string& uname) const
{
    assert(not is_qualified_symbol(uname));
    if (not mod_name)
        throw myexception()<<"Trying to create qualified name without knowing the module name";
    return (*mod_name) + "." + uname;
}

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
//    assert(x.index >= 0);
    assert(not x.is_exported);
    assert(x.index <= current_index());

    return get_fresh_var( x.name );
}

var FreshVarSource::get_fresh_var_copy(var x)
{
//    assert(x.index >= 0);
    assert(not x.is_exported);
    assert(x.index <= current_index());

    x.index = get_index();

    return x;
}

Hs::Var FreshVarSource::get_fresh_Var(const std::string& name, bool qualified)
{
    string name2 = get_unqualified_name(name) + "@" + std::to_string( get_index() );

    if (qualified)
        name2 = qualified_name(name2);

    return Hs::Var({noloc,name2});
}

Hs::Var FreshVarSource::get_fresh_Var(const var& x, bool qualified)
{
//    assert(x.index >= 0);
    assert(not x.is_exported);
    assert(x.index <= current_index());

    return get_fresh_Var(x.name, qualified);
}

// "Rigid" type vars come from forall-quantified variables.
// "Wobbly" type vars come from existentially-quantified variables (I think).  We don't have any.
// "Meta" type vars are unification type vars.

Hs::MetaTypeVar FreshVarSource::fresh_meta_type_var(const string& name, const Hs::Kind& k)
{
    Hs::MetaTypeVar tv({noloc, name+std::to_string( get_index() )});
    tv.kind = k;
    return tv;
}

Hs::MetaTypeVar FreshVarSource::fresh_meta_type_var(const Hs::Kind& k)
{
    return fresh_meta_type_var("t", k);
}

Hs::TypeVar FreshVarSource::fresh_rigid_type_var(const string& name, const Hs::Kind& k)
{
    Hs::TypeVar tv({noloc, name+std::to_string( get_index() )});
    tv.kind = k;
    tv.info = Hs::typevar_info::rigid;
    return tv;
}

Hs::TypeVar FreshVarSource::fresh_rigid_type_var(const Hs::Kind& k)
{
    return fresh_rigid_type_var("t", k);
}

Hs::TypeVar FreshVarSource::fresh_other_type_var(const string& name, const Hs::Kind& k)
{
    Hs::TypeVar tv({noloc, name+std::to_string( get_index() )});
    tv.kind = k;
    return tv;
}

Hs::TypeVar FreshVarSource::fresh_other_type_var(const Hs::Kind& k)
{
    return fresh_other_type_var("t", k);
}

Hs::TypeVar FreshVarSource::fresh_other_type_var(const string& name)
{
    Hs::TypeVar tv({noloc, name+std::to_string( get_index() )});
    return tv;
}

Hs::TypeVar FreshVarSource::fresh_other_type_var()
{
    return fresh_other_type_var(std::string("t"));
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

FreshVarSource::FreshVarSource(FreshVarState& s, const string& mn)
    :state(s), mod_name(mn)
{ }

var make_var(const Hs::Var& v)
{
    assert(not v.index);
    assert(not v.wrap);
    return var(unloc(v.name));
}

vector<var> make_vars(const vector<Hs::Var>& vs)
{
    vector<var> vs2;
    for(auto& v: vs)
        vs2.push_back(make_var(v));
    return vs2;
}

