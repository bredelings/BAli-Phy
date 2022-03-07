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

    // FIXME: Check that if we insert x -> y, the y->level >= x->level

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

const Hs::TypeVar* is_skolem_type_var(const Hs::TypeVar& tv)
{
    if (tv.level)
        return nullptr;
    else
        return &tv;
}

const Hs::TypeVar* is_skolem_type_var(const Hs::Type& type)
{
    auto tv = type.to<Hs::TypeVar>();

    if (not tv) return nullptr;

    return is_skolem_type_var(*tv);
}


const Hs::TypeVar* is_meta_type_var_on_level(const Hs::TypeVar& tv, int level)
{
    if (not tv.level) return nullptr;

    int tv_level = *(tv.level);
    if (tv_level >= level)
    {
        assert(tv_level == level);
        return &tv;
    }
    else
        return nullptr;
}

const Hs::TypeVar* is_meta_type_var_on_level(const Hs::Type& type, int level)
{
    auto tv = type.to<Hs::TypeVar>();

    if (not tv) return nullptr;

    return is_meta_type_var_on_level(*tv, level);
}

// This finds
// * a non-<level>-variable type that the type variable is equivalent to, if there is one, and
// * the final variable in the chain of variables -- that points to either that term, or nothing.
// We can use the final variable to determine if two variables are in the same equivalence set.
// QUESTION: Can we use compare the variables by POINTER equality?  Not sure...
std::pair<const Hs::TypeVar*,const Hs::Type*> follow_var_chain(int level, const substitution_t& s, const Hs::TypeVar* tv)
{
    assert(is_meta_type_var_on_level(*tv, level));

    const Hs::Type* type;
    // While there is a substitution for tv...
    while ( (type = s.find(*tv)) )
    {
        // If we have tv -> tv2, then continue
        if (auto tv_next = is_meta_type_var_on_level(type, level))
        {
            tv = tv_next;
        }
        // Otherwise, return the type variable and the (non-type-variable) thing it points to.
        else
            break;
    }

    assert(tv);
    assert(not type or not is_meta_type_var_on_level(type, level));
    return {tv, type};
}

// This finds
// * a non-variable type that the type variable is equivalent to, if there is one, and
// * the final variable in the chain of variables -- that points to either that term, or nothing.
// We can use the final variable to determine if two variables are in the same equivalence set.
// QUESTION: Can we use compare the variables by POINTER equality?  Not sure...
std::pair<const Hs::TypeVar*,const Hs::Type*> follow_var_chain_match(const substitution_t& s, const Hs::TypeVar* tv)
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

std::optional<substitution_t> combine(int level, const std::optional<substitution_t>& s1, const std::optional<substitution_t>& s2)
{
    if (not s1) return {};
    if (not s2) return {};
    return combine(level, *s1, *s2);
}

std::optional<substitution_t> combine(int level, const std::optional<substitution_t>& s1, const substitution_t& s2)
{
    if (not s1) return {};
    return combine(level, *s1, s2);
}

std::optional<substitution_t> combine(int level, const substitution_t& s1, const optional<substitution_t>& s2)
{
    if (not s2) return {};
    return combine(level, s1, *s2);
}

// What does it mean to combine substitutions when their are multiple levels?
// If we have x_3 -> y_3 -> z_2, then this means that {x_3 = y_3} -> z_2
// Also, we can't add new definitions except on the given level.  So, we can only call unify(level, ...)
// However, we can reverse equalities like x_3 -> y_3 to y_3 -> x_3 on other levels.
// And we can add transitive equalities on other levels: if x_3 -> y3 and x_3 -> z3, then we can add y3 -> z3.


// The order of s1 and s2 should not matter.
std::optional<substitution_t> combine(int level, substitution_t s1, substitution_t s2)
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

    // For each substitution x ~> e in s2:
    for(auto& [x,e]: s2)
    {
        optional<substitution_t> s3;

        assert(x.level);
        int x_level = *x.level;
        assert(x_level >= level);

        // 1. Find the last type variable in the var chain from tv, and maybe the term x_value that it points.
        auto [x_exemplar, x_value] = follow_var_chain(x_level, s1, &x);

        // 2. If (x isn't in s1) OR (x has no value in s1), then we can just add a definition for x_exemplar.
        if (not x_value)
            s3 = try_insert(s1, *x_exemplar, e);

        // 3. If e is a type var on the same level as tv, then..
        else if (auto y = is_meta_type_var_on_level(e, x_level))
        {
            // 3a. Find the last type variable in the var chain from e, and maybe the term y_value that it points to.
            auto [y_exemplar, y_value] = follow_var_chain(x_level, s1, y);

            // 3b. If x and y both point to the same exemplar, then x ~ y is already satisfied.
            if (*x_exemplar == *y_exemplar)
                s3 = substitution_t();
            // 3c. If y has no value in s1, then we can add y_exemplar ~> x_exemplar
            else if (not y_value)
                s3 = try_insert(s1, *y_exemplar, *x_exemplar);
            // 3d. Otherwise, x_exemplar and tv2 are both equal to different terms that must be equal.
            else
                s3 = combine(level, s1, maybe_unify(level, *x_value, *y_value));
        }
        else
            s3 = combine(level, s1, maybe_unify(level, *x_value, e));

        if (not s3)
            return {};
        else
            s1 = *s3;
    }

    return s1;
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

        // 1. Find the last type variable in the var chain from tv, and maybe the term term1 that it points.
        auto [tv1, term1] = follow_var_chain_match(s3, &tv);

        // 2. If tv is equivalent to other type variables, but not to a term, then
        //    we can add just a definition for tv1.
        if (not term1)
        {
            s4 = try_insert(s3, *tv1, e);
        }
        // 3. Otherwise, we have tv ~ term1 and tv ~ e, so *term1 and e have to be the same.
        else if (same_type(*term1, e))
            continue;
    }

    return s3;
}

// Is there a better way to implement this?
optional<substitution_t> maybe_unify(int level, const Hs::Type& t1, const Hs::Type& t2)
{
    if (auto tv1 = is_meta_type_var_on_level(t1, level))
    {
        substitution_t s;
        return try_insert(s, *tv1, t2);
    }
    else if (auto tv2 = is_meta_type_var_on_level(t2, level))
    {
        substitution_t s;
        return try_insert(s, *tv2, t1);
    }
    else if (auto tv1 = is_skolem_type_var(t1))
    {
        substitution_t empty;
        if (auto tv2 = is_skolem_type_var(t2); tv2 and *tv1 == *tv2)
            return empty;
        else
            return {};
    }
    else if (t1.is_a<Haskell::TypeApp>() and t2.is_a<Haskell::TypeApp>())
    {
        auto& app1 = t1.as_<Haskell::TypeApp>();
        auto& app2 = t2.as_<Haskell::TypeApp>();

        return combine( level, maybe_unify(level, app1.head, app2.head),
                               maybe_unify(level, app1.arg , app2.arg ) );
    }
    else if (t1.is_a<Haskell::TypeCon>() and
             t2.is_a<Haskell::TypeCon>() and
             t1.as_<Haskell::TypeCon>() == t2.as_<Haskell::TypeCon>())
    {
        substitution_t empty;
        return empty;
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
            s = combine(level, s, maybe_unify(level, tup1.element_types[i], tup2.element_types[i]) );
            if (not s) return {};
        }
        return s;
    }
    else if (t1.is_a<Hs::ListType>() and t2.is_a<Hs::ListType>())
    {
        auto& L1 = t1.as_<Hs::ListType>();
        auto& L2 = t2.as_<Hs::ListType>();
        return maybe_unify(level, L1.element_type, L2.element_type);
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

substitution_t unify(int level, const Hs::Type& t1, const Hs::Type& t2)
{
    auto s = maybe_unify(level, t1, t2);
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

