#include "unify.H"
#include <utility>

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

Hs::Context apply_subst(const substitution_t& s, Hs::Context C)
{
    for(auto& constraint: C.constraints)
        constraint = apply_subst(s, constraint);
    return C;
}

expression_ref apply_subst(const substitution_t& s, const Hs::Type& t)
{
    if (s.empty()) return t;

    if (auto tv = t.to<Haskell::TypeVar>())
    {
        if (auto t2 = s.find(*tv))
            return apply_subst(s,*t2);
        else
            return t;
    }
    else if (t.is_a<Haskell::TypeCon>())
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
    else if (auto p_app = t.to<Haskell::TypeApp>())
    {
        auto app = *p_app;
        app.head = apply_subst(s, app.head);
        app.arg  = apply_subst(s, app.arg);
        return app;
    }
    else if (auto p_forall = t.to<Haskell::ForallType>())
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
substitution_t compose(substitution_t s2, substitution_t s1)
{
    if (s2.empty()) return s1;

    auto s3 = s2;
    for(auto& [tv,e]: s1)
        s3 = s3.insert({tv,apply_subst(s2,e)});
    return s3;
}

// PROOF: This cannot add substitution loops.
//  `safe_type` can't reference variables in s, since it has been substituted.
//  `safe_type` can't reference `tv` because of the occurs check.
//  Therefore, substituting into any phrase with `tv` will have the same termination properties as before.
std::optional<substitution_t> try_insert(const substitution_t& s, const Hs::TypeVar& tv, Hs::Type type)
{
    // 1. We can't insert tv ~ type if we already have a substitution for tv.
    assert(not s.count(tv));

    // 2. Eliminate existing variables in type.
    auto safe_type = apply_subst(s, type);

    // 3. Trying insert tv ~ tv is redundant, so return an empty substitution.
    if (auto tv2 = safe_type.to<Hs::TypeVar>(); tv2 and *tv2 == tv)
        return s;

    // 4. If safe_type contains tv, then we have a substitution loop for tv.
    //    Therefore return failure.  (This rules out infinite types.)
    if (occurs_check(tv, safe_type)) return {};

    // 5. It is safe to add tv -> safe_type
    return s.insert({tv, safe_type});
}

// The kind-inference paper treats substitutions as a list of terms of the form
// * a     (declaration)
// * b ~ a (definition)
// I think that you have a sequence like {a, b ~ a} then you can't later define a,
// since it is already "declared".


// This finds
// * a non-variable type that the type variable is equivalent to, if there is one, and
// * the final variable in the chain of variables -- that points to either that term, or nothing.
// We can use the final variable to determine if two variables are in the same equivalence set.
// QUESTION: Can we use compare the variables by POINTER equality?  Not sure...
std::pair<const Hs::TypeVar*,const Hs::Type*> follow_var_chain(const substitution_t& s, const Hs::TypeVar* tv)
{
    const Hs::Type* type;
    // While there is a substitution for tv...
    while ( (type = s.find(*tv)) )
    {
        // If we have tv -> tv2, then continue
        if (auto tv_next = type->to<Hs::TypeVar>())
            tv = tv_next;
        // Otherwise, return the type variable and the (non-type-variable) thing it points to.
        else
            break;
    }

    assert(tv);
    assert(not type or not type->is_a<Hs::TypeVar>());
    return {tv, type};
}

std::optional<substitution_t> combine(const std::optional<substitution_t>& s1, const std::optional<substitution_t>& s2)
{
    if (not s1) return {};
    if (not s2) return {};
    return combine(*s1, *s2);
}

std::optional<substitution_t> combine(const std::optional<substitution_t>& s1, const substitution_t& s2)
{
    if (not s1) return {};
    return combine(*s1, s2);
}

std::optional<substitution_t> combine(const substitution_t& s1, const optional<substitution_t>& s2)
{
    if (not s2) return {};
    return combine(s1, *s2);
}

// The order of s1 and s2 should not matter.
std::optional<substitution_t> combine(substitution_t s1, substitution_t s2)
{
    // While we store substitutions as [(TypeVar,Type)], the conceptual model is
    //   really [([TypeVar], Maybe Type)].
    // That is, each typevar with a definition is equivalent to a collection of other type
    //   variables, and possible one "term" that is not a type variable.
    // Therefore, when we set a type variable equivalent to a new term, we must either
    // * unify the new term with the old term for the type variable, if there is one.
    // * set the equivalence class of variables equal to the new term, if there is no existing term.

    // If either substitution is empty, we won't waste any time iterating through the loop below.
    if (s2.size() > s1.size()) std::swap(s1,s2);

    auto s3 = s1;
    for(auto& [tv,e]: s2)
    {
        optional<substitution_t> s4;

        // 1. Find the last type variable in the var chain from tv, and maybe the term term1 that it points.
        auto [tv1, term1] = follow_var_chain(s3, &tv);

        // 2. If tv is equivalent to other type variables, but not to a term, then
        //    we can add just a definition for tv1.
        if (not term1)
        {
            s4 = try_insert(s3, *tv1, e);
        }
        // 3. If e is a type var, then..
        else if (auto maybe_tv2 = e.to<Hs::TypeVar>())
        {
            // 3a. Find the last type variable in the var chain from e, and maybe the term term2 that it points to.
            auto [tv2, term2] = follow_var_chain(s3, maybe_tv2);

            // 3b. If tv and e both point to the same var (*tv1 == *tv2), then tv ~ e is already satisfied.
            if (*tv1 == *tv2)
                s4 = substitution_t();
            // 3c. If tv2 and tv2 are NOT equivalent, but tv2 has no definition, then
            //     we can add a definition for tv2.
            else if (not term2)
            {
                s4 = try_insert(s3, *tv2, *tv1);
            }
            // 2c. Otherwise, tv1 and tv2 are both equal to different terms that must be equal.
            else
            {
                assert(not term1->is_a<Hs::TypeVar>());
                assert(not term2->is_a<Hs::TypeVar>());
                s4 = combine(s3, maybe_unify(*term1, *term2));
            }
        }
        else
            s4 = combine(s3, maybe_unify(*term1, e));

        if (not s4)
            return {};
        else
            s3 = *s4;
    }
            
    return s3;
}

std::optional<substitution_t> combine_match(const std::optional<substitution_t>& s1, const std::optional<substitution_t>& s2)
{
    if (not s1) return {};
    if (not s2) return {};
    return combine_match(*s1, *s2);
}

std::optional<substitution_t> combine_match(const std::optional<substitution_t>& s1, const substitution_t& s2)
{
    if (not s1) return {};
    return combine_match(*s1, s2);
}

std::optional<substitution_t> combine_match(const substitution_t& s1, const optional<substitution_t>& s2)
{
    if (not s2) return {};
    return combine_match(s1, *s2);
}

// The order of s1 and s2 should not matter.
std::optional<substitution_t> combine_match(substitution_t s1, substitution_t s2)
{
    if (s2.size() > s1.size()) std::swap(s1,s2);

    auto s3 = s1;
    for(auto& [tv,e]: s2)
    {
        optional<substitution_t> s4;

        auto it = s3.find(tv);
        // 1. If s3 doesn't have anything of the first tv ~ *, then we can add one.
        if (not it)
        {
            if (auto s4 = try_insert(s3, tv, e))
                s3 = *s4;
            else
                return {};
        }
        // 2. Otherwise, we have tv ~ e and tv ~ *it, so e and *it have to be the same.
        else if (same_type(e, *it))
            continue;
    }

    return s3;
}

bool occurs_check(const Haskell::TypeVar& tv, const Hs::Type& t)
{
    if (auto x = t.to<Haskell::TypeVar>())
        return tv == *x;
    else if (t.is_a<Haskell::TypeCon>())
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
    else if (auto p_app = t.to<Haskell::TypeApp>())
        return occurs_check(tv, p_app->head) or occurs_check(tv, p_app->arg);
    else if (auto f = t.to<Haskell::ForallType>())
    {
        for(auto& x: f->type_var_binders)
            if (x == tv)
                return false;
        return occurs_check(tv, f->type);
    }
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

// Is there a better way to implement this?
optional<substitution_t> maybe_unify(const Hs::Type& t1, const Hs::Type& t2)
{
    if (auto tv1 = t1.to<Haskell::TypeVar>())
    {
        substitution_t s;
        return try_insert(s, *tv1, t2);
    }
    else if (auto tv2 = t2.to<Haskell::TypeVar>())
    {
        substitution_t s;
        return try_insert(s, *tv2, t1);
    }
    else if (t1.is_a<Haskell::TypeApp>() and t2.is_a<Haskell::TypeApp>())
    {
        auto& app1 = t1.as_<Haskell::TypeApp>();
        auto& app2 = t2.as_<Haskell::TypeApp>();

        return combine( maybe_unify(app1.head, app2.head),
                        maybe_unify(app1.arg , app2.arg ) );
    }
    else if (t1.is_a<Haskell::TypeCon>() and
             t2.is_a<Haskell::TypeCon>() and
             t1.as_<Haskell::TypeCon>() == t2.as_<Haskell::TypeCon>())
    {
        substitution_t s;
        return s;
    }
    else if (t1.is_a<Hs::TupleType>() and t2.is_a<Hs::TupleType>())
    {
        auto& tup1 = t1.as_<Hs::TupleType>();
        auto& tup2 = t2.as_<Hs::TupleType>();
        if (tup1.element_types.size() != tup2.element_types.size())
            return {};

        optional<substitution_t> s = substitution_t();
        for(int i=0;i<tup1.element_types.size();i++)
        {
            s = combine(s, maybe_unify(tup1.element_types[i], tup2.element_types[i]) );
            if (not s) return {};
        }
        return s;
    }
    else if (t1.is_a<Hs::ListType>() and t2.is_a<Hs::ListType>())
    {
        auto& L1 = t1.as_<Hs::ListType>();
        auto& L2 = t2.as_<Hs::ListType>();
        return maybe_unify(L1.element_type, L2.element_type);
    }
    else if (t1.is_a<Hs::ConstrainedType>() or t2.is_a<Hs::ConstrainedType>())
    {
        throw myexception()<<"maybe_unify "<<t1.print()<<" ~ "<<t2.print()<<": How should we handle unification for constrained types?";
    }
    else if (t1.is_a<Hs::StrictLazyType>() or t2.is_a<Hs::StrictLazyType>())
    {
        throw myexception()<<"maybe_unify "<<t1.print()<<" ~ "<<t2.print()<<": How should we handle unification for strict/lazy types?";
    }
    else
        return {};
}

substitution_t unify(const Hs::Type& t1, const Hs::Type& t2)
{
    auto s = maybe_unify(t1,t2);
    if (not s)
        throw myexception()<<"Unification failed: "<<t1<<" !~ "<<t2;
    return *s;
}

optional<substitution_t> match(const Hs::Type& t1, const Hs::Type& t2)
{
    if (auto tv1 = t1.to<Haskell::TypeVar>())
    {
        substitution_t s;
        if (t1 == t2) return s;
        return try_insert(s, *tv1, t2);
    }
    else if (t1.is_a<Haskell::TypeApp>() and t2.is_a<Haskell::TypeApp>())
    {
        auto& app1 = t1.as_<Haskell::TypeApp>();
        auto& app2 = t2.as_<Haskell::TypeApp>();

        return combine_match( match(app1.head, app2.head),
                              match(app1.arg , app2.arg ) );
    }
    else if (t1.is_a<Haskell::TypeCon>() and
             t2.is_a<Haskell::TypeCon>() and
             t1.as_<Haskell::TypeCon>() == t2.as_<Haskell::TypeCon>())
    {
        substitution_t s;
        return s;
    }
    else if (t1.is_a<Hs::TupleType>() and t2.is_a<Hs::TupleType>())
    {
        auto& tup1 = t1.as_<Hs::TupleType>();
        auto& tup2 = t2.as_<Hs::TupleType>();
        if (tup1.element_types.size() != tup2.element_types.size())
            return {};

        optional<substitution_t> s = substitution_t();

        for(int i=0;i<tup1.element_types.size();i++)
        {
            s = combine_match(s, match( tup1.element_types[i], tup2.element_types[i] ));
            if (not s) return {};
        }
        return s;
    }
    else if (t1.is_a<Hs::ListType>() and t2.is_a<Hs::ListType>())
    {
        auto& L1 = t1.as_<Hs::ListType>();
        auto& L2 = t2.as_<Hs::ListType>();
        return match(L1.element_type, L2.element_type);
    }
    else if (t1.is_a<Hs::ForallType>() or t2.is_a<Hs::ForallType>())
    {
        throw myexception()<<"match "<<t1.print()<<" ~ "<<t2.print()<<": How should we handle forall types?";
    }
    else if (t1.is_a<Hs::ConstrainedType>() or t2.is_a<Hs::ConstrainedType>())
    {
        throw myexception()<<"match "<<t1.print()<<" ~ "<<t2.print()<<": How should we handle unification for constrained types?";
    }
    else if (t1.is_a<Hs::StrictLazyType>() or t2.is_a<Hs::StrictLazyType>())
    {
        throw myexception()<<"match "<<t1.print()<<" ~ "<<t2.print()<<": How should we handle unification for strict/lazy types?";
    }
    else
        return {};
}


bool same_type(const Hs::Type& t1, const Hs::Type& t2)
{
    if (t1.is_a<Haskell::TypeVar>())
        return (t1 == t2);
    else if (t1.is_a<Haskell::TypeCon>())
        return (t1 == t2);
    else if (t1.is_a<Haskell::TypeApp>() and t2.is_a<Haskell::TypeApp>())
    {
        auto& app1 = t1.as_<Haskell::TypeApp>();
        auto& app2 = t2.as_<Haskell::TypeApp>();

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
        return {};
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

