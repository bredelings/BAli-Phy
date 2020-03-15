#include <vector>
#include <utility>
#include "typecheck.H"
#include "immer/map.hpp" // for immer::map
#include "expression/var.H"
#include "expression/types.H"
#include "expression/let.H"
#include "operation.H"
#include "expression/apply.H"
#include "expression/lambda.H"
#include "expression/constructor.H"

using std::pair;
using std::vector;

typedef immer::map<var,expression_ref> type_environment_t;

typedef immer::map<type_var,expression_ref> substitution_t;

struct typechecker_state
{
    int next_var_index = 0;

    type_var fresh_type_var() {return type_var(next_var_index++);}

    expression_ref instantiate(const expression_ref& t);

    pair<substitution_t,expression_ref>
    infer_type(const type_environment_t& env, const expression_ref& E);

    pair<substitution_t,expression_ref>
    typecheck_decls(const type_environment_t& env, const CDecls& E);
};


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

type_environment_t apply_subst(const substitution_t& s, const type_environment_t& env1)
{
    type_environment_t env2;
    for(auto& [x,type]: env1)
        env2 = env2.insert({x, apply_subst(s,type)});
    return env2;
}

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

// This should yield a substitution that is equivalent to apply FIRST s1 and THEN s2,
// like f . g
substitution_t compose(substitution_t s2, substitution_t s1)
{
    auto s3 = s2;
    for(auto& [tv,e]: s1)
        s3 = s3.insert({tv,apply_subst(s2,e)});
    return s3;
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
        assert(sigma);

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
        auto decls = let_decls(E);

        // let x[i] = e[i] in e
        // auto env2 = env;
        // for(auto& [x,e]: decls)
        // {
        //    let t = fresh_type_var();
        //    env2 = envs.insert({x,t});
        // }

        // let x = e1 in e2
        auto& [x,e1] = decls[0];
        auto& e2 = E.sub()[1];

        // (s1, t1) <- infer env e1
        auto [s1,t1] = infer_type(env, e1);

        // let env' = apply s1 env
        auto env2 = apply_subst(s1,env);

        // t'   = generalize env' t1
        auto t_ = generalize(env2, t1);

        // (s2, t2) <- infer (env' `extend` (x, t')) e2
        auto [s2, t2] = infer_type(env2.insert({x,t_}), e2);

        // return (s1 `compose` s2, t2)
        return {compose(s1,s2), t2};
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

            s = unify (type_apply(type_con("[]"),t1), t2);

            return {s, apply_subst(s,t2) };
            // (:) :: a -> [a] -> [a]
        }

        auto t = fresh_type_var();
        return {s,t};
    }
    else if (is_non_apply_op_exp(E))
    {
        std::abort();
        // what is an example of this?
    }
    std::abort();
}



expression_ref typecheck_topdecls(const expression_ref& topdecls)
{
    auto decls = parse_decls(topdecls);
    for(auto& [x,e]: decls)
    {
        typechecker_state state;
        auto [s,t] = state.infer_type({},e);
        std::cerr<<x<<" :: "<<t<<"\n\n\n";
    }
    return {};
}


