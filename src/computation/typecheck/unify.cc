#include "unify.H"
#include <utility>
#include "immer/set.hpp"

#include "computation/haskell/ids.H"
#include "kind.H"

using std::vector;
using std::string;
using std::optional;

string print(const substitution_t& s)
{
    std::ostringstream oss;
    for(auto& [var,type]: s)
    {
        oss<<var.print()<<" :: "<<type.print()<<"\n";
    }
    return oss.str();
}

string print(const u_substitution_t& s)
{
    std::ostringstream oss;
    for(auto& [var,type]: s)
    {
        oss<<var.print()<<" :: "<<type.print()<<"\n";
    }
    return oss.str();
}

Hs::Context apply_subst(const u_substitution_t& s, Hs::Context C)
{
    for(auto& constraint: C.constraints)
        constraint = apply_subst(s, constraint);
    return C;
}

Hs::Type apply_subst(const u_substitution_t& s, const Hs::Type& t)
{
    if (s.empty()) return t;

    if (auto tv = t.to<Hs::MetaTypeVar>())
    {
        if (auto t2 = s.find(*tv))
            return apply_subst(s,*t2);
        else
            return t;
    }
    else if (t.is_a<Hs::TypeVar>())
        return t;
    else if (t.is_a<Hs::TypeCon>())
        return t;
    else if (auto tup = t.to<Hs::TupleType>())
    {
        auto T = *tup;
        for(auto& type: T.element_types)
            type = apply_subst(s, type);
        return T;
    }
    else if (auto l = t.to<Hs::ListType>())
    {
        auto L = *l;
        L.element_type = apply_subst(s, L.element_type);
        return L;
    }
    else if (auto p_app = t.to<Hs::TypeApp>())
    {
        auto app = *p_app;
        app.head = apply_subst(s, app.head);
        app.arg  = apply_subst(s, app.arg);
        return app;
    }
    else if (auto p_forall = t.to<Hs::ForallType>())
    {
        auto forall = *p_forall;
        forall.type =  apply_subst(s, forall.type);
        return forall;
    }
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        auto C = *c;
        C.context = apply_subst(s, C.context);
        C.type = apply_subst(s, C.type);
        return C;
    }
    else if (auto sl = t.to<Hs::StrictLazyType>())
    {
        auto SL = *sl;
        SL.type = apply_subst(s, SL.type);
        return SL;
    }
    else
        std::abort();
}

Hs::Context apply_subst(const substitution_t& s, Hs::Context C)
{
    for(auto& constraint: C.constraints)
        constraint = apply_subst(s, constraint);
    return C;
}

Hs::Type apply_subst(const substitution_t& s, const Hs::Type& t)
{
    if (s.empty()) return t;

    else if (auto tt = filled_meta_type_var(t))
        return apply_subst(s, *tt);
    else if (t.is_a<Hs::MetaTypeVar>())
        return t;
    else if (auto tv = t.to<Hs::TypeVar>())
    {
        if (auto t2 = s.find(*tv))
            return apply_subst(s,*t2);
        else
            return t;
    }
    else if (t.is_a<Hs::TypeCon>())
        return t;
    else if (auto tup = t.to<Hs::TupleType>())
    {
        auto T = *tup;
        for(auto& type: T.element_types)
            type = apply_subst(s, type);
        return T;
    }
    else if (auto l = t.to<Hs::ListType>())
    {
        auto L = *l;
        L.element_type = apply_subst(s, L.element_type);
        return L;
    }
    else if (auto p_app = t.to<Hs::TypeApp>())
    {
        auto app = *p_app;
        app.head = apply_subst(s, app.head);
        app.arg  = apply_subst(s, app.arg);
        return app;
    }
    else if (auto p_forall = t.to<Hs::ForallType>())
    {
        auto forall = *p_forall;

        auto s2 = s;
        for(auto& tv: forall.type_var_binders)
            s2 = s2.erase(tv);

        forall.type =  apply_subst(s2, forall.type);
        return forall;
    }
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        auto C = *c;
        C.context = apply_subst(s, C.context);
        C.type = apply_subst(s, C.type);
        return C;
    }
    else if (auto sl = t.to<Hs::StrictLazyType>())
    {
        auto SL = *sl;
        SL.type = apply_subst(s, SL.type);
        return SL;
    }
    else
        std::abort();
}

// This should yield a substitution that is equivalent to apply FIRST s1 and THEN s2,
// like f . g
u_substitution_t compose(u_substitution_t s2, u_substitution_t s1)
{
    if (s2.empty()) return s1;

    auto s3 = s2;
    for(auto& [tv,e]: s1)
        s3 = s3.insert({tv,apply_subst(s2,e)});
    return s3;
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

bool occurs_check(const Hs::MetaTypeVar& tv, const Hs::Type& t)
{
    assert(not tv.filled());

    if (auto tt = filled_meta_type_var(t))
        return occurs_check(tv, *tt);
    else if (auto x = t.to<Hs::MetaTypeVar>())
        return tv == *x;
    else if (t.is_a<Hs::TypeVar>())
        return false;
    else if (t.is_a<Hs::TypeCon>())
        return false;
    else if (auto tup = t.to<Hs::TupleType>())
    {
        for(auto& type: tup->element_types)
            if (occurs_check(tv, type))
                return true;
        return false;
    }
    else if (auto l = t.to<Hs::ListType>())
        return occurs_check(tv, l->element_type);
    else if (auto p_app = t.to<Hs::TypeApp>())
        return occurs_check(tv, p_app->head) or occurs_check(tv, p_app->arg);
    else if (auto f = t.to<Hs::ForallType>())
        return occurs_check(tv, f->type);
    else if (auto c = t.to<Hs::ConstrainedType>())
    {
        // The context may not contain vars that don't occur in the head;
        for(auto& constraint: c->context.constraints)
            if (occurs_check(tv, constraint))
                return true;

        return occurs_check(tv, c->type);
    }
    else if (auto sl = t.to<Hs::StrictLazyType>())
        return occurs_check(tv, sl->type);
    else
        std::abort();
}

// PROOF: This cannot add substitution loops.
//  `safe_type` can't reference variables in s, since it has been substituted.
//  `safe_type` can't reference `tv` because of the occurs check.
//  Therefore, substituting into any phrase with `tv` will have the same termination properties as before.
std::optional<u_substitution_t> try_insert(const u_substitution_t& s, const Hs::MetaTypeVar& tv, Hs::Type type)
{
    // 1. We can't insert tv ~ type if we already have a substitution for tv.
    assert(not s.count(tv));

    // 2. We can only bind meta type vars to tau types.
    assert(Hs::is_tau_type(type));

    // 3. Eliminate existing variables in type.
    //    FIXME: This is expensive - GHC might try to avoid this by delaying 'zonking'.
    auto safe_type = apply_subst(s, type);

    // 4. Trying insert tv ~ tv is redundant, so return an empty substitution.
    if (auto tv2 = safe_type.to<Hs::MetaTypeVar>(); tv2 and *tv2 == tv)
        return s;

    // 5. If safe_type contains tv, then we have a substitution loop for tv.
    //    Therefore return failure.  (This rules out infinite types.)
    if (occurs_check(tv, safe_type)) return {};

    // 6. It is safe to add tv -> safe_type
    return s.insert({tv, safe_type});
}

// The kind-inference paper treats substitutions as a list of terms of the form
// * a     (declaration)
// * b ~ a (definition)
// I think that you have a sequence like {a, b ~ a} then you can't later define a,
// since it is already "declared".



// QUESTION: How should we handle levels, if we reintroduce them?
// This finds
// * the final variable in the chain of variables -- that points to either that term, or nothing.
// We can use the final variable to determine if two variables are in the same equivalence set.
// QUESTION: Can we use compare the variables by POINTER equality?  Not sure...
std::pair<const Hs::MetaTypeVar*,const Hs::Type*> follow_var_chain(const u_substitution_t& s, const Hs::MetaTypeVar* tv)
{
    const Hs::Type* type;
    // While there is a substitution for tv...
    while ( (type = s.find(*tv)) )
    {
        // If we have tv -> tv2, then continue
        if (auto tv_next = type->to<Hs::MetaTypeVar>())
            tv = tv_next;
        // Otherwise, return the type variable and the (non-type-variable) thing it points to.
        else
            break;
    }

    assert(tv);
    return {tv, type};
}

struct unification_env
{
    immer::map<Hs::TypeVar, Hs::TypeVar> mapping1;
    immer::map<Hs::TypeVar, Hs::TypeVar> mapping2;
    Hs::TypeVar fresh_tyvar() const;
    Hs::TypeVar not_in_scope_tyvar(const Hs::TypeVar& tv1, const Hs::TypeVar& tv2);
};

std::optional<u_substitution_t> combine_(bool both_ways, const unification_env&, u_substitution_t s1, u_substitution_t s2);

std::optional<u_substitution_t> combine_(bool both_ways, const unification_env& env, const std::optional<u_substitution_t>& s1, const std::optional<u_substitution_t>& s2)
{
    if (not s1) return {};
    if (not s2) return {};
    return combine_(both_ways, env, *s1, *s2);
}

std::optional<u_substitution_t> combine_(bool both_ways, const unification_env& env, const std::optional<u_substitution_t>& s1, const u_substitution_t& s2)
{
    if (not s1) return {};
    return combine_(both_ways, env, *s1, s2);
}

std::optional<u_substitution_t> combine_(bool both_ways, const unification_env& env, const u_substitution_t& s1, const optional<u_substitution_t>& s2)
{
    if (not s2) return {};
    return combine_(both_ways, env, s1, *s2);
}

optional<u_substitution_t> maybe_unify_(bool both_ways, const unification_env& env, const Hs::Type& t1, const Hs::Type& t2);

// The order of s1 and s2 should not matter.
std::optional<u_substitution_t> combine_(bool both_ways, const unification_env& env, u_substitution_t s1, u_substitution_t s2)
{
    // While we store u_substitutions as [(TypeVar,Type)], the conceptual model is
    //   really [([TypeVar], Maybe Type)].
    // That is, each typevar with a definition is equivalent to a collection of other type
    //   variables, and possible one "term" that is not a type variable.
    // Therefore, when we set a type variable equivalent to a new term, we must either
    // * unify the new term with the old term for the type variable, if there is one.
    // * set the equivalence class of variables equal to the new term, if there is no existing term.

    // If either u_substitution is empty, we won't waste any time iterating through the loop below.
    if (s2.size() > s1.size()) std::swap(s1,s2);

    // For each u_substitution x ~> e in s2:
    for(auto& [x,e]: s2)
    {
        optional<u_substitution_t> s3;

        // 1. Find the last type variable in the var chain from tv, and maybe the term x_value that it points.
        auto [x_exemplar, x_value] = follow_var_chain(s1, &x);

        // 2. If (x isn't in s1) OR (x has no value in s1), then we can just add a definition for x_exemplar.
        if (not x_value)
            s3 = try_insert(s1, *x_exemplar, e);

        // 3. If e is a type var, then..
        else if (auto y = e.to<Hs::MetaTypeVar>())
        {
            // 3a. Find the last type variable in the var chain from e, and maybe the term y_value that it points to.
            auto [y_exemplar, y_value] = follow_var_chain(s1, y);

            // 3b. If x and y both_ways point to the same exemplar, then x ~ y is already satisfied.
            if (*x_exemplar == *y_exemplar)
                s3 = s1;
            // 3c. If y has no value in s1, then we can add y_exemplar ~> x_exemplar
            else if (not y_value)
                s3 = try_insert(s1, *y_exemplar, *x_exemplar);
            // 3d. Otherwise, x_exemplar and tv2 are both equal to different terms that must be equal.
            else
                s3 = combine_(both_ways, env, s1, maybe_unify_(both_ways, env, *x_value, *y_value));
        }
        else
            s3 = combine_(both_ways, env, s1, maybe_unify_(both_ways, env, *x_value, e));

        if (not s3)
            return {};
        else
            s1 = *s3;
    }

    return s1;
}

Hs::TypeCon tuple_tycon(int n)
{
    auto kind = make_n_args_kind(n);
    return Hs::TypeCon( {noloc, tuple_name(n)}, kind );
}

Hs::TypeCon list_tycon()
{
    auto kind = make_n_args_kind(1);
    return Hs::TypeCon( {noloc,"[]"}, kind );
}

Hs::Type canonicalize_type(const Hs::TupleType& type1)
{
    int n = type1.element_types.size();
    Hs::Type type2 = tuple_tycon(n);
    return Hs::make_tyapps(type2, type1.element_types);
}

Hs::Type canonicalize_type(const Hs::ListType& type1)
{
    Hs::Type type2 = list_tycon();
    return Hs::TypeApp(type2, type1.element_type);
}

Hs::TypeVar unification_env::fresh_tyvar() const
{
    Hs::TypeVar ftv({noloc,"utv"});

    int index = 0;
    for(auto& [name,tv]: mapping1)
    {
        if (tv.index)
            index = std::max(index, *tv.index);
    }
    for(auto& [name,tv]: mapping2)
    {
        if (tv.index)
            index = std::max(index, *tv.index);
    }
    ftv.index = index + 1;
    ftv.info = Hs::typevar_info::rigid;
    return ftv;
}

// We need a typevar that isn't in scope in either term
Hs::TypeVar unification_env::not_in_scope_tyvar(const Hs::TypeVar& tv1, const Hs::TypeVar& tv2)
{
    if (not mapping1.count(tv1) and not mapping2.count(tv1))
        return tv1;
    if (not mapping1.count(tv2) and not mapping2.count(tv2))
        return tv2;
    auto ftv = fresh_tyvar();
    ftv.kind = tv1.kind;
    return ftv;
}


// Is there a better way to implement this?
optional<u_substitution_t> maybe_unify_(bool both_ways, const unification_env& env, const Hs::Type& t1, const Hs::Type& t2)
{
    // Translate rigid type variables
    if (auto tv1 = t1.to<Hs::TypeVar>(); tv1 and (tv1->info == Hs::typevar_info::rigid) and env.mapping1.count(*tv1))
    {
        auto tv1_ = env.mapping1.at(*tv1);
        return maybe_unify_(both_ways, env, tv1_, t2);
    }
    else if (auto tv2 = t2.to<Hs::TypeVar>(); tv2 and (tv2->info == Hs::typevar_info::rigid) and env.mapping2.count(*tv2))
    {
        auto tv2_ = env.mapping2.at(*tv2);
        return maybe_unify_(both_ways, env, t1, tv2_);
    }
    else if (auto tt1 = filled_meta_type_var(t1))
        return maybe_unify_(both_ways, env, *tt1, t2);
    else if (auto tt2 = filled_meta_type_var(t2))
        return maybe_unify_(both_ways, env, t1, *tt2);
    else if (auto tv1 = t1.to<Hs::MetaTypeVar>())
    {
        u_substitution_t s;
        return try_insert(s, *tv1, t2);
    }
    else if (auto tv2 = t2.to<Hs::MetaTypeVar>(); tv2 and both_ways)
    {
        u_substitution_t s;
        return try_insert(s, *tv2, t1);
    }
    else if (auto tv1 = t1.to<Hs::TypeVar>())
    {
        u_substitution_t empty;
        if (auto tv2 = t2.to<Hs::TypeVar>(); tv2 and *tv1 == *tv2)
            return empty;
        else
            return {};
    }
    else if (t1.is_a<Hs::TypeApp>() and t2.is_a<Hs::TypeApp>())
    {
        auto& app1 = t1.as_<Hs::TypeApp>();
        auto& app2 = t2.as_<Hs::TypeApp>();

        return combine_( both_ways, env,
                         maybe_unify_(both_ways, env, app1.head, app2.head),
                         maybe_unify_(both_ways, env, app1.arg , app2.arg ) );
    }
    else if (t1.is_a<Hs::TypeCon>() and
             t2.is_a<Hs::TypeCon>() and
             t1.as_<Hs::TypeCon>() == t2.as_<Hs::TypeCon>())
    {
        u_substitution_t empty;
        return empty;
    }
    else if (auto tup1 = t1.to<Hs::TupleType>())
    {
        return maybe_unify_(both_ways, env, canonicalize_type(*tup1), t2);
    }
    else if (auto tup2 = t2.to<Hs::TupleType>())
    {
        return maybe_unify_(both_ways, env, t1, canonicalize_type(*tup2));
    }
    else if (auto l1 = t1.to<Hs::ListType>())
    {
        return maybe_unify_(both_ways, env, canonicalize_type(*l1), t2);
    }
    else if (auto l2 = t2.to<Hs::ListType>())
    {
        return maybe_unify_(both_ways, env, t1, canonicalize_type(*l2));
    }
    else if (t1.is_a<Hs::ConstrainedType>() and t2.is_a<Hs::ConstrainedType>())
    {
        auto c1 = t1.to<Hs::ConstrainedType>();
        auto c2 = t2.to<Hs::ConstrainedType>();
        if (c1->context.constraints.size() != c2->context.constraints.size())
            return {};
        for(int i=0;i< c1->context.constraints.size();i++)
            if (not maybe_unify_(both_ways, env, c1->context.constraints[i], c2->context.constraints[i]))
                return {};
        return maybe_unify_(both_ways, env, c1->type, c2->type);
    }
    else if (t1.is_a<Hs::ForallType>() and t2.is_a<Hs::ForallType>())
    {
        auto fa1 = t1.to<Hs::ForallType>();
        auto fa2 = t2.to<Hs::ForallType>();

        if (fa1->type_var_binders.size() != fa2->type_var_binders.size())
            return {};

        auto env2 = env;
        for(int i=0;i < fa1->type_var_binders.size(); i++)
        {
            auto tv1 = fa1->type_var_binders[i];
            auto tv2 = fa2->type_var_binders[i];

            auto v = env2.not_in_scope_tyvar(tv1, tv2);
            env2.mapping1 = env2.mapping1.insert({tv1,v});
            env2.mapping2 = env2.mapping2.insert({tv2,v});
        }

        return maybe_unify_(both_ways, env2, fa1->type, fa2->type);
    }
    else if (t1.is_a<Hs::StrictLazyType>() or t2.is_a<Hs::StrictLazyType>())
    {
        throw myexception()<<"maybe_unify "<<t1.print()<<" ~ "<<t2.print()<<": How should we handle unification for strict/lazy types?";
    }
    else
        return {};
}

optional<u_substitution_t> maybe_unify(const Hs::Type& t1, const Hs::Type& t2)
{
    unification_env env;
    return maybe_unify_(true, env, t1, t2);
}

u_substitution_t unify(const Hs::Type& t1, const Hs::Type& t2)
{
    auto s = maybe_unify(t1, t2);
    if (not s)
        throw myexception()<<"Unification failed: "<<t1<<" !~ "<<t2;
    return *s;
}

optional<u_substitution_t> maybe_match(const Hs::Type& t1, const Hs::Type& t2)
{
    unification_env env;
    return maybe_unify_(false, env, t1, t2);
}

u_substitution_t match(const Hs::Type& t1, const Hs::Type& t2)
{
    auto s = maybe_match(t1, t2);
    if (not s)
        throw myexception()<<"Match failed: "<<t1<<" !~ "<<t2;
    return *s;
}

std::optional<u_substitution_t> combine(u_substitution_t s1, u_substitution_t s2)
{
    unification_env env;
    return combine_(true, env, s1, s2);
}

bool same_type(const Hs::Type& t1, const Hs::Type& t2)
{
    if (auto type1 = filled_meta_type_var(t1))
        return same_type(*type1, t2);
    else if (auto type2 = filled_meta_type_var(t2))
        return same_type(t1, *type2);
    else if (t1.is_a<Hs::MetaTypeVar>())
        return (t1 == t2);
    else if (t1.is_a<Hs::TypeVar>())
        return (t1 == t2);
    else if (t1.is_a<Hs::TypeCon>())
        return (t1 == t2);
    else if (t1.is_a<Hs::TypeApp>() and t2.is_a<Hs::TypeApp>())
    {
        auto& app1 = t1.as_<Hs::TypeApp>();
        auto& app2 = t2.as_<Hs::TypeApp>();

        return same_type(app1.head, app2.head) and same_type(app1.arg, app2.arg);
    }
    else if (t1.is_a<Hs::TupleType>() and t2.is_a<Hs::TupleType>())
    {
        auto& tup1 = t1.as_<Hs::TupleType>();
        auto& tup2 = t2.as_<Hs::TupleType>();
        if (tup1.element_types.size() != tup2.element_types.size())
            return false;

        for(int i=0;i<tup1.element_types.size();i++)
            if (not same_type(tup1.element_types[i], tup2.element_types[i])) return false;

        return true;
    }
    else if (t1.is_a<Hs::ListType>() and t2.is_a<Hs::ListType>())
    {
        auto& L1 = t1.as_<Hs::ListType>();
        auto& L2 = t2.as_<Hs::ListType>();

        return same_type(L1.element_type, L2.element_type);
    }
    else if (t1.is_a<Hs::ForallType>() or t2.is_a<Hs::ForallType>())
    {
        throw myexception()<<"same_type "<<t1.print()<<" "<<t2.print()<<": How should we handle forall types?";
    }
    else if (t1.is_a<Hs::ConstrainedType>() or t2.is_a<Hs::ConstrainedType>())
    {
        throw myexception()<<"same_type "<<t1.print()<<" "<<t2.print()<<": How should we handle unification for constrained types?";
    }
    else if (t1.is_a<Hs::StrictLazyType>() or t2.is_a<Hs::StrictLazyType>())
    {
        throw myexception()<<"same_type "<<t1.print()<<" "<<t2.print()<<": How should we handle unification for strict/lazy types?";
    }
    else
        return false;
}


Hs::Constructor apply_subst(const substitution_t& s, Hs::Constructor con)
{
    if (con.context)
        con.context = apply_subst(s, *con.context);

    if (con.fields.index() == 0)
    {
        for(auto& t: std::get<0>(con.fields))
            t = apply_subst(s, t);
    }
    else
    {
        for(auto& f: std::get<1>(con.fields).field_decls)
            f.type = apply_subst(s, f.type);
    }
    return con;
}

Hs::DataOrNewtypeDecl apply_subst(const substitution_t& s, Hs::DataOrNewtypeDecl d)
{
    d.context = apply_subst(s, d.context);
    for(auto& con: d.constructors)
        con = apply_subst(s, con);

    return d;
}

