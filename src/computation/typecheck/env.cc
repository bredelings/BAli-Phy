#include "env.H"
#include "util/string/join.H"
#include "util/set.H"                 // for add( , )
#include "types.H"
#include "substitution.H"

using std::string;
using std::vector;
using std::optional;

int TypeSynonymInfo::arity() const
{
    return type_vars.size();
}

Type TypeSynonymInfo::expand(const std::vector<Type>& args) const
{
    if (args.size() < arity())
        throw myexception()<<name<<" takes "<<arity()<<" arguments, but got "<<args.size()<<"!";

    substitution_t s;
    for(int i=0; i < arity(); i++)
        s = s.insert({type_vars[i], args[i]});

    auto type2 = apply_subst(s, result);

    for(int i=arity(); i< args.size(); i++)
        type2 = TypeApp(type2, args[i]);

    return type2;
}

string print(const value_env& env)
{
    std::ostringstream oss;
    vector<string> ss;
    for(auto& [value,type]: env)
    {
        ss.push_back(value+" :: "+type.print());
    }
    return "{ " + join(ss, "; ") + " }";
}

LIE& operator+=(LIE& lie1, const LIE& lie2)
{
    lie1.insert(lie1.end(), lie2.begin(), lie2.end());
    return lie1;
}


void add_prefer_right(value_env& e1, const value_env& e2)
{
    if (e1.empty())
        e1 = e2;
    else
        for(auto& [x,t]: e2)
            e1 = e1.insert({x,t});
}

value_env plus_prefer_right(const value_env& e1, const value_env& e2)
{
    auto e3 = e1;
    add_prefer_right(e3,e2);
    return e3;
}

void add_no_overlap(value_env& e1, const value_env& e2)
{
    if (e1.empty())
        e1 = e2;
    else
        for(auto& [x,t]: e2)
        {
            if (e1.count(x))
                throw myexception()<<"Both environments contain variable "<<x<<"!";
            e1 = e1.insert({x,t});
        }
}

value_env plus_no_overlap(const value_env& e1, const value_env& e2)
{
    auto e3 = e1;
    add_no_overlap(e3,e2);
    return e3;
}

value_env& operator+=(value_env& env1, const value_env& env2)
{
    add_no_overlap(env1, env2);
    return env1;
}

value_env operator+(const value_env& env1, const value_env& env2)
{
    return plus_no_overlap(env1, env2);
}

void add_no_overlap(type_con_env& e1, const type_con_env& e2)
{
    if (e1.empty())
        e1 = e2;
    else
        for(auto& [x,t]: e2)
        {
            if (e1.count(x))
                throw myexception()<<"Both environments contain variable "<<x<<"!";
            e1.insert({x,t});
        }
}

type_con_env plus_no_overlap(const type_con_env& e1, const type_con_env& e2)
{
    auto e3 = e1;
    add_no_overlap(e3,e2);
    return e3;
}

type_con_env& operator+=(type_con_env& tycons1, const type_con_env tycons2)
{
    add_no_overlap(tycons1, tycons2);
    return tycons1;
}

type_con_env operator+(const type_con_env& tycons1, const type_con_env& tycons2)
{
    auto tycons3 = tycons1;
    tycons3 += tycons2;
    return tycons3;
}

std::set<TypeVar> free_type_variables(const value_env& env)
{
    std::set<TypeVar> free;
    for(auto& [x,type]: env)
        add(free, free_type_variables(type));
    return free;
}

std::set<TypeVar> free_type_variables(const LIE& env)
{
    std::set<TypeVar> free;
    for(auto& constraint: env)
        add(free, free_type_variables(constraint.pred));
    return free;
}

std::set<MetaTypeVar> free_meta_type_variables(const value_env& env)
{
    std::set<MetaTypeVar> free;
    for(auto& [x,type]: env)
        add(free, free_meta_type_variables(type));
    return free;
}

std::set<MetaTypeVar> free_meta_type_variables(const LIE& env)
{
    std::set<MetaTypeVar> free;
    for(auto& constraint: env)
        add(free, free_meta_type_variables(constraint.pred));
    return free;
}

vector<Type> DataConInfo::all_constraints() const
{
    auto cs = top_constraints;
    for(auto& constraint: gadt_eq_constraints)
        cs.push_back(constraint);
    for(auto& constraint: written_constraints)
        cs.push_back(constraint);
    return cs;
}

vector<Type> DataConInfo::dictionary_preds() const
{
    return ::dictionary_preds(all_constraints());
}

vector<Type> DataConInfo::equality_preds() const
{
    return ::equality_preds(all_constraints());
}

int DataConInfo::dict_arity() const
{
    return dictionary_preds().size();
}

int DataConInfo::arity() const
{
    return field_types.size();
}

Type DataConInfo::result_type() const
{
    return type_apply( data_type, uni_tvs );
}

Type DataConInfo::constructor_type() const
{
    auto type = function_type(field_types, result_type());

    // add_constraints will merge the constraints, here.
    type = add_constraints(written_constraints, type);
    type = add_constraints(gadt_eq_constraints, type);
    type = add_forall_vars(exi_tvs, type);

    // FIXME: Only add top constraints for type variables in constructor fields
    type = add_constraints(top_constraints, type);
    type = add_forall_vars(uni_tvs, type);

    return type;
}

TypeFamInfo::TypeFamInfo(const vector<TypeVar>& as, const Kind& k, const optional<string>& s)
    :args(as), result_kind(k), associated_class(s)
{
    for(auto& arg: args)
        arg.kind = arg.kind.value_or(kind_type());
}

RenameTyvarEnv2 rename_binders2(RenameTyvarEnv2 env, const vector<TypeVar>& tvs1, const vector<TypeVar>& tvs2)
{
    assert(tvs1 == tvs2);
    for(int i=0;i<tvs1.size();i++)
    {
        env = rename_binder2(env, tvs1[i], tvs2[i]);
    }
    return env;
}

RenameTyvarEnv2 rename_binder2(RenameTyvarEnv2 env, const TypeVar& tv1, const TypeVar& tv2)
{
    auto [env2, _] = rename_binder2_var(env, tv1, tv2);
    return env2;
}

TypeVar locally_unique_tyvar(const VarSet& vars)
{
    TypeVar tv = TypeVar({noloc,"tvX"});
    int index = 0;
    for(auto& var: vars)
    {
        if (var.index and *var.index > index)
            index = *var.index;
    }
    tv.index = index + 1;
    return tv;
}

std::tuple<RenameTyvarEnv2,TypeVar> rename_binder2_var(RenameTyvarEnv2 env, const TypeVar& tv1, const TypeVar& tv2)
{
    TypeVar tv_new = tv1;
    if (env.out_vars.count(tv_new))
    {
        tv_new = tv2;
        if (env.out_vars.count(tv_new))
            tv_new = locally_unique_tyvar(env.out_vars);
    }

    env.left = extendVarEnv(env.left, tv1, tv_new);
    env.right = extendVarEnv(env.right, tv1, tv_new);
    env.out_vars = extendVarSet(env.out_vars, tv_new);

    return {env, tv_new};
}

VarEnv extendVarEnv(VarEnv env, const TypeVar& tv_in, const TypeVar& tv_out)
{
    env = env.insert({tv_in, tv_out});
    return env;
}

VarSet extendVarSet(VarSet set, const TypeVar& tv_out)
{
    assert(not set.count(tv_out));
    set = set.insert(tv_out);
    return set;
}

TypeVar RenameTyvarEnv2::map_left(const TypeVar& tv) const
{
    if (auto it = left.find(tv))
        return *it;
    else
        return tv;
}

TypeVar RenameTyvarEnv2::map_right(const TypeVar& tv) const
{
    if (auto it = right.find(tv))
        return *it;
    else
        return tv;
}

