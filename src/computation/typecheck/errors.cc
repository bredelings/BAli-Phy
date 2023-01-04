#include "typecheck.H"
#include "kindcheck.H"

#include "util/text.H"        // for ANSI colors
#include "util/set.H"         // for add( , )
#include "util/variant.H"     // for to< >()
#include "util/string/join.H" // for join( )
#include "haskell/ids.H"
#include "tidy.H"

#include <range/v3/all.hpp>

namespace views = ranges::views;

using std::string;
using std::vector;
using std::map;
using std::set;
using std::tuple;
using std::optional;
using std::shared_ptr;

void TypeChecker::record_error(cow_ptr<TypeCheckerContext> context, const Notes& notes)
{
    auto c = context.modify();
    for(auto& note: notes)
        c->push_note(note);

    messages().push_back({ErrorMsg, context->source_span(), context->notes});
}

void TypeChecker::record_error(cow_ptr<TypeCheckerContext> context, const Note& e)
{
    record_error(context, Notes{e});
}

void TypeChecker::record_error(const Note& e)
{
    return record_error(context(), e);
}

void TypeChecker::record_warning(cow_ptr<TypeCheckerContext> context, const Notes& notes)
{
    auto c = context.modify();
    for(auto& note: notes)
        c->push_note(note);

    messages().push_back({WarningMsg, context->source_span(), context->notes});
}

void TypeChecker::record_warning(cow_ptr<TypeCheckerContext> context, const Note& e)
{
    record_warning(context, Notes{e});
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

Note make_mismatch_message(TidyState& tidy_state, const Constraint& wanted, const Type& t1, const Type& t2)
{
    Note mismatch;
    if (auto uorig = to<UnifyOrigin>(wanted.origin))
    {
        mismatch<<"Expected `"<<tidy_print(tidy_state, uorig->t2)<<"` but got `"<<tidy_print(tidy_state, uorig->t1)<<"`";
    }
    else if (auto app = to<AppOrigin>(wanted.origin))
    {
        mismatch<<"Applying "<<(app->arg_index+1)<<" arguments to function "<<app->head.print()<<", but it only takes "<<app->arg_index<<"!";
    }
    else if (auto lsec = to<LeftSectionOrigin>(wanted.origin))
        mismatch<<"In left section, "<<lsec->op<<" is not a function!";
    else
    {
        mismatch<<"Couldn't match `"<<tidy_print(tidy_state, t1)<<"` with `"<<tidy_print(tidy_state, t2)<<"`";
    }
    return mismatch;
}

vector<tuple<Hs::Var,Type>> get_relevant_bindings(const TypeCheckerContext& tc_state, const set<TypeVar>& ftvs, const set<MetaTypeVar>& fmtvs)
{
    vector<tuple<Hs::Var,Type>> relevant_bindings;

    auto maybe_add = [&](const Hs::Var& var, const Type& type) -> bool
    {
        // auto& name = unloc(var.name);
        // if (is_qualified_symbol(name)) return true;

        if (intersects(free_type_variables(type), ftvs) or intersects(free_meta_type_variables(type), fmtvs))
        {
            relevant_bindings.push_back({var,type});

            // Don't return too many relevant bindings.
            if (relevant_bindings.size() > 5) return false;
        }
        return true;
    };

    for(auto& var_type: tc_state.binder_stack | views::reverse)
    {
        if (auto id_type = to<IDType>(var_type))
        {
            auto& [var,type] = *id_type;
            if (not maybe_add(var,type)) break;
        }
        else if (auto id_type = to<IDExpType>(var_type))
        {
            auto& [var,exp_type] = *id_type;
            if (not maybe_add(var, exp_type.read_type())) break;
        }
        else
            std::abort();
    }

    return relevant_bindings;
}

[[nodiscard]] Notes add_relevant_bindings(TidyState& tidy_state, Notes notes, const vector<tuple<Hs::Var,Type>>& bindings)
{
    if (not bindings.empty())
    {
        auto note = Note()<<"Relevant bindings:";
        for(auto& [var,type]: bindings)
            note<<"\n  "<<print_unqualified_id(var.print())<<" :: "<<tidy_print(tidy_state, type);
        notes.push_back(note);
    }
    return notes;
}



Notes TypeChecker::check_eq_tv_constraint(TidyState& tidy_state, vector<shared_ptr<Implication>>& implic_scopes, const Constraint& wanted, const Type& t1, const Type& t2) const
{
    const Implication * implic = nullptr;
    if (not implic_scopes.empty()) implic = implic_scopes.back().get();

    auto tv1 = t1.to<TypeVar>();
    auto mtv1 = t1.to<MetaTypeVar>();
    assert(tv1 or mtv1);

    Note mismatch = make_mismatch_message(tidy_state, wanted, t1, t2);

    Note cannot_unify;
    cannot_unify<<"Cannot unify `"<<tidy_print(tidy_state, t1)<<"` with `"<<tidy_print(tidy_state, t2)<<"`";

    auto problems = check_type_equality(t1,t2);

    auto ftvs = free_type_variables(wanted.pred);
    auto fmtvs = free_meta_type_variables(wanted.pred);

    auto relevant_bindings = get_relevant_bindings(*wanted.tc_state, ftvs, fmtvs);

    // 1. Unification with polytype
    Notes notes;
    if (problems.test(impredicative_bit))
    {
        cannot_unify<<" because it is a polytype";
        notes.push_back(cannot_unify);
    }
    // 2. Occurs check
    else if (has_occurs_check(problems))
    {
        cannot_unify<<" because of occurs check";
        notes.push_back(cannot_unify);
    }
    // 3. tv is blocked from escaping too
    else if (tv1 and implic and includes(implic->tvs, *tv1))
    {
        notes.push_back(mismatch);
    }
    // 4. tv is NOT blocked from escaping, but t2 IS blocked.
    else if (implic and intersects(free_type_variables(t2), implic->tvs | ranges::to<set>()))
    {
        auto escaped = intersection(free_type_variables(t2), implic->tvs | ranges::to<set>());
        vector<string> escaped_names;
        for(auto& tv: escaped)
            escaped_names.push_back(tidy_print(tidy_state,tv));
        cannot_unify<<" because the quantified variable `"<<join(escaped_names," ")<<"` would escape its scope";

        notes.push_back(cannot_unify);
    }
    else if (mtv1)
    {
        // This is presumably an untouchable meta-type variable.
        notes.push_back(mismatch);
    }
    else
        notes.push_back(mismatch);

    notes = add_relevant_bindings(tidy_state, notes, relevant_bindings);

    // The "top" ones are supposed to be at the end...
    std::reverse(notes.begin(), notes.end());

    return notes;
}


Notes TypeChecker::check_eq_constraint(TidyState& tidy_state, vector<shared_ptr<Implication>>& implic_scopes, const Constraint& wanted, const Type& t1, const Type& t2) const
{
    // Need to pass in list of nested implications?
    // what about tidying?
    // what about relevant bindings?
    Notes notes;

    auto v1 = look_thru(t1);
    auto v2 = look_thru(t2);

    if (v1.to<TypeVar>() or v1.to<MetaTypeVar>())
        notes = check_eq_tv_constraint(tidy_state, implic_scopes, wanted, v1, t2);
    else if (v2.to<TypeVar>() or v2.to<MetaTypeVar>())
        notes = check_eq_tv_constraint(tidy_state, implic_scopes, wanted, v2, t1);
    else
        notes.push_back( make_mismatch_message(tidy_state, wanted, t1, t2) );
    return notes;
}

void TypeChecker::check_wanteds(TidyState& tidy_state, vector<shared_ptr<Implication>>& implic_scopes, const WantedConstraints& wanteds)
{
    for(auto& wanted: wanteds.simple)
    {
        Notes notes;
        if (auto eq = is_equality_pred(wanted.pred))
        {
            auto& [t1,t2] = *eq;
            notes = check_eq_constraint(tidy_state, implic_scopes, wanted, t1, t2);
        }
        else
        {
            Note e;
            e<<"Could not derive `"<<bold_green(tidy_print(tidy_state, wanted.pred))<<ANSI::bold<<"`";
            if (auto occ = to<OccurrenceOrigin>(wanted.origin))
                e<<" arising from a use of `"<<cyan(print_unqualified_id(occ->name))<<ANSI::bold<<"`";
            if (wanted.tc_state->source_span())
                e<<" at "<<(*wanted.tc_state->source_span());
            notes.push_back(e);
        }

        record_error(wanted.tc_state, notes);
    }

    for(auto& implic: wanteds.implications)
    {
        implic_scopes.push_back(implic);
        check_wanteds(tidy_state, implic_scopes, implic->wanteds);
        implic_scopes.pop_back();
    }
}

void TypeChecker::check_wanteds(const WantedConstraints& wanteds)
{
    vector<shared_ptr<Implication>> implic_scopes;

    // Get names for problematic variables before names in "relevant bindings"
    TidyState tidy_state;
    for(auto& tv: free_type_variables(wanteds))
        tidy_print(tidy_state, tv);
    for(auto& mtv: free_meta_type_variables(wanteds))
        tidy_print(tidy_state, mtv);

    check_wanteds(tidy_state, implic_scopes, wanteds);
}

