#include <memory>
#include <vector>
#include <map>
#include <utility>
#include "typecheck.H"
#include "immer/map.hpp" // for immer::map
#include "expression/var.H"
#include "expression/types.H"
#include "expression/let.H"
#include "expression/apply.H"
#include "expression/case.H"
#include "expression/lambda.H"
#include "expression/constructor.H"
#include "expression/tuple.H"
#include "operation.H"

using std::pair;
using std::map;
using std::vector;
using std::shared_ptr;
using std::string;

typedef immer::map<type_var,expression_ref> substitution_t;

// I don't think this is right!
// When combining two substitutions, we really need to *unify* the equations!
// Don't we?

expression_ref apply_subst(const substitution_t& s, const expression_ref& t)
{
    if (is_type_var(t))
    {
        auto& tv = t.as_<type_var>();
        if (auto t2 = s.find(tv))
            return *t2;
        else
            return t;
    }
    else if (is_type_forall(t))
    {
        auto& x = t.sub()[0].as_<type_var>();
        auto s2 = s.erase(x);
        auto t2 = apply_subst(s2, t.sub()[1]);
        return type_forall(x,t2);
    }
    else if (is_type_apply(t))
    {
        auto t1 = apply_subst(s, t.sub()[0]);
        auto t2 = apply_subst(s, t.sub()[1]);
        return type_apply(t1,t2);
    }
    else if (is_type_con(t))
        return t;

    std::abort();
}

// This should yield a substitution that is equivalent to apply FIRST s1 and THEN s2,
// like f . g
substitution_t compose(substitution_t s2, substitution_t s1)
{
    if (s2.empty()) return s1;

    auto s3 = s2;
    for(auto& [tv,e]: s1)
        s3 = s3.insert({tv,apply_subst(s2,e)});
    return s3;
}

bool occurs_check(const type_var& tv, const expression_ref& t)
{
    if (is_type_var(t))
        return tv == t.as_<type_var>();
    else if (is_type_forall(t))
    {
        auto& x = t.sub()[0].as_<type_var>();
        if (x == tv)
            return false;
        else
            return occurs_check(tv, t.sub()[1]);
    }
    else if (is_type_apply(t))
    {
        auto& t1 = t.sub()[0];
        auto& t2 = t.sub()[1];
        return occurs_check(tv,t1) or occurs_check(tv,t2);
    }
    else if (is_type_con(t))
        return false;
    else
    {
        throw myexception()<<"types do not unify!";
    }
}



// Is there a better way to implement this?
substitution_t unify(const expression_ref& t1, const expression_ref& t2)
{
    if (is_type_apply(t1) and is_type_apply(t2))
    {
        auto s1 = unify(t1.sub()[0], t2.sub()[0]);
        auto s2 = unify(apply_subst(s1,t1.sub()[1]), apply_subst(s1, t2.sub()[1]));
        return compose(s2,s1);
    }
    else if (is_type_var(t1))
    {
        substitution_t s;
        auto& tv = t1.as_<type_var>();
        if (t1 == t2)
            return s;
        if (occurs_check(tv,t2))
            throw myexception()<<"Occurs check: cannot construct infinite type: "<<tv<<" ~ "<<t2<<"\n";
        return s.insert({tv, t2});
    }
    else if (is_type_var(t2))
    {
        substitution_t s;
        auto& tv = t2.as_<type_var>();
        if (t1 == t2)
            return s;
        if (occurs_check(tv,t1))
            throw myexception()<<"Occurs check: cannot construct infinite type: "<<tv<<" ~ "<<t1<<"\n";
        return s.insert({tv, t1});
    }
    else if (is_type_con(t1) and
             is_type_con(t2) and
             t1.as_<type_con>() == t2.as_<type_con>())
    {
        return {};
    }
    else
    {
        throw myexception()<<"types do not unify!";
    }
}


typedef immer::map<var,expression_ref> type_environment_t;

type_environment_t apply_subst(const substitution_t& s, const type_environment_t& env1)
{
    type_environment_t env2;
    for(auto& [x,type]: env1)
        env2 = env2.insert({x, apply_subst(s,type)});
    return env2;
}

struct data
{
    expression_ref type;
    map<constructor, std::vector<expression_ref>> constructors;
};

data apply_subst(const substitution_t& s, data d)
{
    d.type = apply_subst(s, d.type);
    for(auto& [con,types]: d.constructors)
        for(auto& type: types)
            type = apply_subst(s, type);
    return d;
}

struct typechecker_state
{
    map<constructor,shared_ptr<data>> con_to_data;

    int next_var_index = 1;

    type_var fresh_type_var() {
        type_var tv("t"+std::to_string(next_var_index), next_var_index);
        next_var_index++;
        return tv;
    }

    void add_data_type(const shared_ptr<data>& d)
    {
        for(auto& [con,_]: d->constructors)
            con_to_data[con] = d;
    }

    type_var named_type_var(const string& name)
    {
        type_var tv(name+"_"+std::to_string(next_var_index), next_var_index);
        next_var_index++;
        return type_var(next_var_index++);
    }

    expression_ref instantiate(const expression_ref& t);

    pair<substitution_t,expression_ref>
    infer_type(const type_environment_t& env, const expression_ref& E);

    pair<substitution_t,type_environment_t>
    infer_type_for_decls(const type_environment_t& env, const CDecls& E);
};

void get_free_type_variables(const type_environment_t& env, std::set<type_var>& free)
{
    for(auto& [x,type]: env)
        get_free_type_variables(type, free);
}

std::set<type_var> free_type_variables(const type_environment_t& env)
{
    std::set<type_var> free;
    get_free_type_variables(env, free);
    return free;
}

expression_ref generalize(const type_environment_t& env, const expression_ref& monotype)
{
    auto ftv1 = free_type_variables(monotype);
    auto ftv2 = free_type_variables(env);
    for(auto tv: ftv2)
        ftv1.erase(tv);

    auto polytype = monotype;
    for(auto tv: ftv1)
    {
        polytype = type_forall(tv,polytype);
    }
    return polytype;
}

expression_ref typechecker_state::instantiate(const expression_ref& t)
{
    substitution_t s;
    auto t2 = t;
    while(is_type_forall(t2))
    {
        auto& tv = t2.sub()[0].as_<type_var>();
        s = s.insert({tv,fresh_type_var()});
        t2 = t2.sub()[1];
    }
    return apply_subst(s,t2);
}

pair<substitution_t,type_environment_t>
typechecker_state::infer_type_for_decls(const type_environment_t& env, const CDecls& decls)
{
    // 1. Add each let-binder to the environment with a fresh type variable
    auto env2 = env;
    for(auto& [x,e]: decls)
    {
        auto t = fresh_type_var();
        env2 = env2.insert({x,t});
    }

    // 2. Infer the types of each of the x[i]
    substitution_t s;
    for(auto& [x_i, e_i]: decls)
    {
        auto t_x_i = env2[x_i];
        auto [s_i, t_e_i] = infer_type(env2, e_i);

        s = compose(s_i, compose(unify(t_x_i, t_e_i), s));

        env2 = apply_subst(s, env2);
    }

    // 3. Generalize each type over variables not in the *original* environment
    for(auto& [x,e]: decls)
    {
        auto monotype = env2[x];
        auto polytype = generalize(env,monotype);
        env2 = env2.insert({x,polytype});
    }

    return {s, env2};
}


// This is mostly algorithm W from wikipedia: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system#Algorithm_W
// Also see: http://dev.stephendiehl.com/fun/006_hindley_milner.html
//      see: https://gist.github.com/chrisdone/0075a16b32bfd4f62b7b#binding-groups

pair<substitution_t,expression_ref>
typechecker_state::infer_type(const type_environment_t& env, const expression_ref& E)
{
    if (is_var(E))
    {
        auto& x = E.as_<var>();

        auto sigma = env.find(x);

        // x should be in the type environment
        if (not sigma)
            throw myexception()<<"infer_type: can't find type of variable '"<<x.print()<<"'";

        auto tau = instantiate(*sigma);

        return {{},tau};
    }
    else if (not E.size())
    {
        if (E.is_int())
            return {{},type_con("Num")};
        else if (E.is_double())
            return {{},type_con("Num")};
        else if (E.is_log_double())
            return {{},type_con("Num")};
        else if (E.is_char())
            return {{},type_con("Char#")};
        else if (is_constructor(E))
        {
            auto& con = E.as_<constructor>();
            if (con.name() == "()")
                return {{},type_con("()")};
            else if (con.name() == "[]")
            {
                auto tau = fresh_type_var();
                return {{},type_apply(type_con("[]"),tau)};
            }
        }

        // We can't handle constants correctly, so always given them a new type.
        auto tau = fresh_type_var();
        return {{},tau};
    }
    else if (is_apply_exp(E))
    {
        assert(E.size() >= 2);

        auto e1 = E.sub()[0];
        substitution_t s;

        auto [s1,t1] = infer_type(env,e1);

        for(int i=1;i<E.size();i++)
        {
            auto e2 = E.sub()[i];

            // tv <- fresh
            auto tv = fresh_type_var();

            // This is now done by the previous iteration of the loop!
            // (s1, t1) <- infer env e1
            // auto [s1,t1] = infer_type(env, e1);

            // (s2, t2) <- infer (apply s1 env) e2
            auto [s2,t2] = infer_type(apply_subst(s1,env), e2);

            // s3       <- unify (apply s2 t1) (TArr t2 tv)
            auto s3 = unify (apply_subst(s2,t1), function_type(t2,tv));

            s1 = compose(s3,compose(s2,s1));
            t1 = apply_subst(s3,tv);
        }

        // This is now done by the setup for the next loop iteration.
        // return {compose(s3,compose(s2,s1)), apply_subst(s3,tv)};
        return {s1,t1};
    }
    else if (is_lambda_exp(E))
    {
        // \x->e
        auto& x = E.sub()[0].as_<var>();
        auto& e = E.sub()[1];

        // tv <- fresh
        expression_ref tv = fresh_type_var();

        // let env' = env `extend` (x, Forall [] tv)
        auto env2 = env.insert({x,tv});

        // (s1, t1) <- infer env' e
        auto [s1,t1] = infer_type(env2, e);

        // return (s1, apply s1 tv `TArr` t1)
        return {s1, apply_subst(s1,function_type(tv,t1))};
    }
    else if (is_let_expression(E))
    {
        // let x[i] = e[i] in e
        auto decls = let_decls(E);

        // 1. Extend environment with types for decls, get any substitutions
        auto [s_decls, env_decls] = infer_type_for_decls(env, decls);

        // 2. Compute type of let body
        auto& e_body = E.sub()[1];
        auto [s_body, t_body] = infer_type(env_decls, e_body);

        // return (s1 `compose` s2, t2)
        return {compose(s_body, s_decls), t_body};
    }
    else if (is_constructor_exp(E))
    {
        substitution_t s;
        type_environment_t env2 = env;
        vector<expression_ref> arg_types;
        for(int i=0; i<E.size(); i++)
        {
            auto [s_i, t_i] = infer_type(env2, E.sub()[i]);
            arg_types.push_back(t_i);
            s = compose(s_i, s);
            env2 = apply_subst(s_i, env2);
        }
        auto& con = E.head().as_<constructor>();
        if (con.name() == "(,)")
        {
            expression_ref t = type_con("(,)");
            t = type_apply(t,arg_types[0]);
            t = type_apply(t,arg_types[1]);
            t = apply_subst(s,t);
            return {s,t};
        }
        else if (con.name() == ":")
        {
            assert(E.size() == 2);

            auto t1 = arg_types[0];
            t1 = apply_subst(s,t1);

            auto t2 = arg_types[1];

            auto s2 = unify (type_apply(type_con("[]"),t1), t2);

            return {compose(s2,s), apply_subst(s,t2) };
            // (:) :: a -> [a] -> [a]
        }

        auto t = fresh_type_var();
        return {s,t};
    }
    else if (is_non_apply_op_exp(E))
    {
        std::abort();
        // this includes builtins like Prelude::add
    }
    else if (is_case(E))
    {
        expression_ref object;
        vector<expression_ref> patterns;
        vector<expression_ref> bodies;
        std::abort();
    }
    std::abort();
}



expression_ref typecheck_topdecls(const expression_ref& topdecls)
{
    auto decls = parse_decls(topdecls);
    typechecker_state state;

    auto a = state.named_type_var("a");
    auto b = state.named_type_var("b");
    auto error_type = type_forall(a,type_forall(b,function_type(a,b)));

    type_environment_t env0;
    env0 = env0.insert({var("Compiler.Base.error"),error_type});

    env0 = env0.insert({var("Foreign.String.unpack_cpp_string"),function_type(type_con("String#"),list_type(type_con("Char")))});


    auto data_pair = std::make_shared<data>();
    data_pair->type = type_apply(type_apply(type_con("(,)"),a),b);
    data_pair->constructors[tuple_head(2)] = {a,b};
    state.add_data_type(data_pair);

    auto data_list = std::make_shared<data>();
    data_list->type = list_type(a);
    constructor cons(":",2);
    data_list->constructors[cons] = {a,list_type(a)};
    constructor nil("[]",0);
    data_list->constructors[nil] = {};
    state.add_data_type(data_list);

    auto [s,env] = state.infer_type_for_decls(env0, decls);
    for(auto& [x,e]: decls)
    {
        auto t = env[x];
        std::cerr<<x<<" :: "<<t<<"\n";
        std::cerr<<x<<" = "<<e<<"\n\n\n";
    }
    return {};
}


