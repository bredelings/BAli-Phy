#include "typecheck.H"
#include "kindcheck.H"

#include "util/text.H"        // for ANSI colors
#include "util/set.H"         // for add( , )
#include "util/variant.H"     // for to< >()
#include "util/string/join.H" // for join( )
#include "haskell/ids.H"

#include <range/v3/all.hpp>

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::shared_ptr;

void TypeChecker::record_error(cow_ptr<TypeCheckerContext> context, const Note& e)
{
    context.modify()->push_note(e);

    messages().push_back({ErrorMsg, context->source_span(), context->notes});
}

void TypeChecker::record_error(const Note& e)
{
    return record_error(context(), e);
}

void TypeChecker::record_warning(cow_ptr<TypeCheckerContext> context, const Note& e)
{
    context.modify()->push_note(e);

    messages().push_back({WarningMsg, context->source_span(), context->notes});
}

void TypeChecker::record_warning(const Note& e)
{
    return record_warning(context(), e);
}

bool TypeChecker::has_errors() const
{
    return ::has_errors(messages());
}

myexception TypeChecker::note_exception() const
{
    return myexception(print_note());
}

string print_unqualified_id(const string& s)
{
    auto s2 = get_unqualified_name(s);
    if (is_haskell_sym(s2))
        s2 = "("+s2+")";
    return s2;
}

string print_unqualified_id(const Located<string>& ls)
{
    return print_unqualified_id(unloc(ls));
}

Note make_mismatch_message(const Constraint& wanted, const Type& t1, const Type& t2)
{
    Note mismatch;
    if (auto uorig = to<UnifyOrigin>(wanted.origin))
    {
        mismatch<<"Expected `"<<print_unqualified(uorig->t2)<<"` but got `"<<print_unqualified(uorig->t1)<<"`";
    }
    else if (auto app = to<AppOrigin>(wanted.origin))
    {
        mismatch<<"Applying "<<(app->arg_index+1)<<" arguments to function "<<app->head.print()<<", but it only takes "<<app->arg_index<<"!";
    }
    else if (auto lsec = to<LeftSectionOrigin>(wanted.origin))
        mismatch<<"In left section, "<<lsec->op<<" is not a function!";
    else
    {
        mismatch<<"Couldn't match `"<<print_unqualified(t1)<<"` with `"<<print_unqualified(t2)<<"`";
    }
    return mismatch;
}

Note TypeChecker::check_eq_tv_constraint(vector<shared_ptr<Implication>>& implic_scopes, const Constraint& wanted, const Type& t1, const Type& t2) const
{
    const Implication * implic = nullptr;
    if (not implic_scopes.empty()) implic = implic_scopes.back().get();

    auto tv1 = t1.to<TypeVar>();
    auto mtv1 = t1.to<MetaTypeVar>();
    assert(tv1 or mtv1);

    Note mismatch = make_mismatch_message(wanted, t1, t2);

    Note cannot_unify;
    cannot_unify<<"Cannot unify `"<<print_unqualified(t1)<<"` with `"<<print_unqualified(t2)<<"`";

    auto problems = check_type_equality(t1,t2);

    // 1. Unification with polytype
    if (problems.test(impredicative_bit))
    {
        cannot_unify<<" because it is a polytype";
        return cannot_unify;
    }
    // 2. Occurs check
    else if (has_occurs_check(problems))
    {
        cannot_unify<<" because of occurs check";
        return cannot_unify;
    }
    // 3. tv is blocked from escaping too
    else if (tv1 and implic and includes(implic->tvs, *tv1))
    {
        return mismatch;
    }
    // 4. tv is NOT blocked from escaping, but t2 IS blocked.
    else if (implic and intersects(free_type_variables(t2), implic->tvs | ranges::to<set>()))
    {
        auto escaped = intersection(free_type_variables(t2), implic->tvs | ranges::to<set>());
        vector<string> escaped_names;
        for(auto& tv: escaped)
            escaped_names.push_back(tv.print());
        cannot_unify<<" because the quantified variable `"<<join(escaped_names," ")<<"` would escape its scope";
        return cannot_unify;
    }
    else if (mtv1)
    {
        // This is presumably an untouchable meta-type variable.
        return mismatch;
    }
    else
        return mismatch;
}


Note TypeChecker::check_eq_constraint(vector<shared_ptr<Implication>>& implic_scopes, const Constraint& wanted, const Type& t1, const Type& t2) const
{
    // Need to pass in list of nested implications?
    // what about tidying?
    // what about relevant bindings?
    Note note;

    auto v1 = look_thru(t1);
    auto v2 = look_thru(t2);

    if (v1.to<TypeVar>() or v1.to<MetaTypeVar>())
        note = check_eq_tv_constraint(implic_scopes, wanted, v1, t2);
    else if (v2.to<TypeVar>() or v2.to<MetaTypeVar>())
        note = check_eq_tv_constraint(implic_scopes, wanted, v2, t1);
    else
        note = make_mismatch_message(wanted, t1, t2);
    return note;
}

void TypeChecker::check_wanteds(vector<shared_ptr<Implication>>& implic_scopes, const WantedConstraints& wanteds)
{
    for(auto& wanted: wanteds.simple)
    {
        Note e;
        if (auto eq = is_equality_pred(wanted.pred))
        {
            auto& [t1,t2] = *eq;
            e = check_eq_constraint(implic_scopes, wanted, t1, t2);
        }
        else
        {
            e<<"Could not derive `"<<bold_green(print_unqualified(wanted.pred))<<ANSI::bold<<"`";
            if (auto occ = to<OccurrenceOrigin>(wanted.origin))
                e<<" arising from a use of `"<<cyan(print_unqualified_id(occ->name))<<ANSI::bold<<"`";
            if (wanted.tc_state->source_span())
                e<<" at "<<(*wanted.tc_state->source_span());
        }

        record_error(wanted.tc_state, e);
    }

    for(auto& implic: wanteds.implications)
    {
        implic_scopes.push_back(implic);
        check_wanteds(implic_scopes, implic->wanteds);
        implic_scopes.pop_back();
    }
}

void TypeChecker::check_wanteds(const WantedConstraints& wanteds)
{
    vector<shared_ptr<Implication>> implic_scopes;
    check_wanteds(implic_scopes, wanteds);
}

