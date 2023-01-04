#include "computation/fresh_vars.H"
#include "computation/haskell/ids.H"
#include "util/myexception.H"

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
    unloc(x2.name) = qualified_name(unloc(x2.name));
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

var FreshVarSource::get_fresh_var(const std::string& name, bool qualified)
{
    assert(not is_haskell_builtin_con_name(name));
    string name2 = add_suffix(get_unqualified_name(name), get_index() );

    if (qualified)
        name2 = qualified_name(name2);

    return var(name2);
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
    string name2 = add_suffix(get_unqualified_name(name), get_index() );

    if (qualified)
        name2 = qualified_name(name2);

    return Hs::Var({noloc,name2});
}

Hs::Var FreshVarSource::get_fresh_Var(const Hs::Var& x, bool qualified)
{
    auto x2 = get_fresh_Var(unloc(x.name), qualified);
    x2.name.loc = x.name.loc;
    x2.type = x.type;
    return x2;
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

MetaTypeVar FreshVarSource::fresh_meta_type_var(int level, const string& name, const Kind& k)
{
    MetaTypeVar tv(level, {noloc, add_suffix(name, get_index() )});
    tv.kind = k;
    return tv;
}

MetaTypeVar FreshVarSource::fresh_meta_type_var(int level, const Kind& k)
{
    return fresh_meta_type_var(level, "t", k);
}

TypeVar FreshVarSource::fresh_rigid_type_var(int level, const string& name, const Kind& k)
{
    TypeVar tv(level, {noloc, add_suffix(name, get_index() )});
    tv.kind = k;
    return tv;
}

TypeVar FreshVarSource::fresh_rigid_type_var(int level, const Kind& k)
{
    return fresh_rigid_type_var(level, "t", k);
}

TypeVar FreshVarSource::fresh_other_type_var(const string& name, const Kind& k)
{
    TypeVar tv({noloc, add_suffix(name, get_index() )});
    tv.kind = k;
    return tv;
}

TypeVar FreshVarSource::fresh_other_type_var(const Kind& k)
{
    return fresh_other_type_var("t", k);
}

TypeVar FreshVarSource::fresh_other_type_var(const string& name)
{
    TypeVar tv({noloc, add_suffix(name, get_index() )});
    return tv;
}

TypeVar FreshVarSource::fresh_other_type_var()
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
    assert(v.wrap.is_identity());
    return var(unloc(v.name));
}

vector<var> make_vars(const vector<Hs::Var>& vs)
{
    vector<var> vs2;
    for(auto& v: vs)
        vs2.push_back(make_var(v));
    return vs2;
}

