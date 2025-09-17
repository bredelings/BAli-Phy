#include "typecheck.H"
#include "kindcheck.H"
#include "solver.H"

#include "util/set.H"
#include "util/variant.H"
#include "util/string/join.H"

#include "computation/core/func.H"

#include <range/v3/all.hpp>

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;
using std::shared_ptr;

namespace views = ranges::views;

string NonCanonical::print() const
{
    return constraint.print();
}

string CanonicalDict::print() const
{
    return constraint.print();
}

string CanonicalEquality::print() const
{
    return constraint.print();
}

string Predicate::print() const
{
    if (std::holds_alternative<NonCanonical>(*this))
        return std::get<NonCanonical>(*this).print();
    else if (std::holds_alternative<CanonicalDict>(*this))
        return std::get<CanonicalDict>(*this).print();
    else if (std::holds_alternative<CanonicalEquality>(*this))
        return std::get<CanonicalEquality>(*this).print();
    else
        std::abort();
}

const Constraint& Predicate::constraint() const
{
    if (auto nc = to<NonCanonical>(*this))
        return nc->constraint;
    else if (auto dict = to<CanonicalDict>(*this))
        return dict->constraint;
    else if (auto eq = to<CanonicalEquality>(*this))
        return eq->constraint;
    else
        std::abort();
}

const ConstraintOrigin& Predicate::origin() const
{
    return constraint().origin;
}

ConstraintFlavor Predicate::flavor() const
{
    return constraint().flavor;
}

int Predicate::level() const
{
    return constraint().level();
}

// FIXME: there should be a `const` way of getting these.
// FIXME: instantiate is not constant though.
// FIXME: we shouldn't need fresh type vars if the type is unambiguous though.
vector<pair<Core2::Var<>, Type>> TypeChecker::superclass_constraints(const Type& constraint)
{
    vector<pair<Core2::Var<>, Type>> constraints;

    auto class_name = get_full_class_name_from_constraint(constraint);

    if (class_name == "~") return {};

    // Fixme... since we know the class name, can we simplify superclass_extractors???
    auto info = info_for_class(class_name);
    assert(info);
    for(auto& [dvar, type]: info->superclass_extractors)
    {
        // forall a.Klass a => Superklass a
        auto [tvs, preds, superclass_constraint] = peel_top_gen(type);
        auto s = fresh_tv_binders(tvs);
        preds = apply_subst(s, preds);
        superclass_constraint = apply_subst(s, superclass_constraint);

        // Constraints like a ~ (Arg a -> Result a) violate this:
        // assert(constraint_is_hnf(superclass_constraint));

        assert(preds.size() == 1);
        auto class_constraint = preds[0];

        // The premise matches the current class;
        if (auto subst = maybe_match(class_constraint, constraint))
        {
            superclass_constraint = apply_subst(*subst, superclass_constraint);
            constraints.push_back( { dvar, superclass_constraint } );
        }
    }

    return constraints;
}

// We are trying to eliminate the *first* argument.
optional<vector<Core2::Var<>>> TypeChecker::is_superclass_of(const Type& constraint1, const Type& constraint2)
{
    vector<Core2::Var<>> extractors;
    if (same_type(constraint1, constraint2))
        return extractors;
    else
    {
        // dvar1 :: constraint1 => dvar3 :: constraint3 => dvar2 :: constraint2
        for(auto& [dvar, constraint3]: superclass_constraints(constraint2))
        {
            if (auto extractors2 = is_superclass_of(constraint1, constraint3))
            {
                extractors = std::move(*extractors2);
                extractors.push_back(dvar);
                return extractors;
            }
        }
        return {};
    }
}

optional<Core2::Decls<>> TypeChecker::entails_by_superclass(const Constraint& given, const Constraint& wanted)
{
    if (auto extractors = is_superclass_of(wanted.pred, given.pred))
    {
        Core2::Exp<> dict_exp = given.ev_var;
        for(auto& extractor: *extractors | views::reverse)
            dict_exp = Core2::Apply<>{extractor, dict_exp};

        // dvar_wanted = extractor[n] extractor[n-1] ... extractor[0] dvar_given
        return Core2::Decls<>( { {wanted.ev_var, dict_exp} } );
    }
    else
        return {};
}

// 1. constraint solving loop: first process simple constraints, then go under implications.
//    1a. Run the constraint solving loop on the simpled Wanted constraints.
//        - This can create new implication constraints. (how?)
//    1b. Process each implication constraint
//        - Enter an implication.
//        - Save the outer inert set.
//        - Simplify the givens - add them to the inert set when this makes sense.
//        - Solve the inner simple wanteds - also add them to the inert set?
//          - does this have the effect of making simple wanteds affect implication constraints?
//        - Recurse into nested implications.
//        - Restore the saved inert set to what it was before entering the implication.
//        - Then continue processing the next implication constraint.

// 2. The interaction pipeline.
//    2a. Pick an item from the work list.
//    2b. If its not canonical, canonicalize it and goto 2a.
//    2c. If we have an inert reaction, do it and goto 2a.
//    2d. If we have a top-level reaction, do it and goto 2a.
//    2e. If its canonical and there are no possible reactions left, add it to the inert set.

CanonicalEquality CanonicalEquality::flip() const
{
    CanonicalEquality E = *this;
    E.constraint.pred = make_equality_pred(t2,t1);
    std::swap(E.t1, E.t2);
    return E;
}

void Solver::unbreak_type_equality_cycles()
{
    for(auto& [mtv,type]: inerts.cycle_breakers)
    {
        mtv.fill(type);
    }
}

bool is_canonical(const Predicate& p)
{
    return std::holds_alternative<CanonicalDict>(p) or
        std::holds_alternative<CanonicalEquality>(p);
}


Change Solver::interact(const Predicate& P1, const Predicate& P2)
{
    assert(is_canonical(P1));
    assert(is_canonical(P2));

    // Don't allow wanteds to rewrite givens
    if (P1.flavor() == Wanted and P2.flavor() == Given) return Unchanged();

    auto dict1 = to<CanonicalDict>(P1);
    auto dict2 = to<CanonicalDict>(P2);

    if (dict1 and dict2)
    {
        // DDICT:  (D xs)     + (D xs)     -> (D xs)
        if (same_type(dict1->constraint.pred, dict2->constraint.pred))
        {
            decls.push_back({dict2->constraint.ev_var, dict1->constraint.ev_var});
            return Solved();
        }
        // SUPER - not in the paper.
        else if (auto sdecls = entails_by_superclass(dict1->constraint, dict2->constraint))
        {
            decls += *sdecls;
            return Solved();
        }
    }
    // EQFEQ:  (tv1 ~ X1) + (F xs ~ x) -> (tv1 ~ X1) && (F [tv1->X1]xs ~ [tv1 -> X1]x) if tv1 in ftv(xs,x)
    // FEQFEQ: (F xs ~ X1) + (F xs ~ X2) -> (F xs ~ X1) && (X1 ~ X2)

    return Unchanged();
}

// we need to have three results: No react, React<vector<Predicates>>, ReactFail
std::optional<Reaction> Solver::top_react(const Predicate& P)
{
    assert(is_canonical(P));

    if (auto dict = to<CanonicalDict>(P))
    {
        // We DO get givens like Eq Ordering that have instances.
        // Should we be preventing such things from becoming givens, since we could
        //   derive them from an instance instead?

        // We don't use instances for givens.
        if (P.flavor() == Given) return {};

        auto loc = dict->constraint.tc_state->source_span();
        if (loc) push_source_span( *loc );
        auto inst = lookup_instance(dict->constraint.pred);        
        if (loc) pop_source_span();

        if (inst)
        {
            auto [dfun_exp, super_wanteds] = *inst;

            decls.push_back( { dict->constraint.ev_var, dfun_exp } );
            for(auto& pred: super_wanteds)
            {
                // Say where in the source code we got this thing from
                pred.tc_state = dict->constraint.tc_state;

                work_list.push_back( pred );
            }

            return ReactSuccess();
        }
    }
    else if (auto eq = to<CanonicalEquality>(P))
    {
        // We don't use instances for givens.
        if (P.flavor() == Given) return {};

        if (find_type_eq_instance(eq->t1, eq->t2))
            return ReactSuccess();
    }
    
    return {};
}

bool Solver::is_touchable(const MetaTypeVar& mtv, const Type& rhs) const
{
    // We need to have done follow_meta_type_var( ) already.
    assert(not mtv.filled());

    assert(mtv.level() <= level());

    // why do we think this?
    assert(not inerts.given_eq_level or *inerts.given_eq_level < level());

    if (mtv.cycle_breaker) return false;

    // 1. Check for intervening given equalities
    bool intervening_given_eqs = inerts.given_eq_level and mtv.level() <= *inerts.given_eq_level;
    if (intervening_given_eqs) return false;

    // 2. Check for skolem escapes
    for(auto& tv: free_type_variables(rhs))
    {
        if (mtv.level() < tv.level())
            return false;
    }

    return true;
}

bool affected_by_mtv(const Predicate& P, const MetaTypeVar& mtv)
{
    return affected_by_mtv(P.constraint().pred, mtv);
}

void Solver::add_to_work_list(const std::vector<Predicate>& ps)
{
    for(auto& p: ps)
        if (p.flavor() == Wanted)
            work_list.push_back(p);

    for(auto& p: ps)
        if (p.flavor() == Given)
            work_list.push_back(p);
}

vector<Predicate> kickout_after_unification2(const MetaTypeVar& mtv, std::vector<Predicate>& preds)
{
    vector<Predicate> work_list;
    // kick-out for inerts that are rewritten by p
    for(int i=0;i<preds.size();i++)
    {
        if (affected_by_mtv(preds[i], mtv))
        {
            // FIXME: put givens last, so that we process them first
            work_list.push_back(preds[i]);
            if (i+1 < preds.size())
                std::swap(preds[i], preds.back());

            preds.pop_back();
            i--;
        }
    }

    return work_list;
}

void Solver::kickout_after_unification(const MetaTypeVar& mtv)
{
    // kick-out for inerts containing mtv
    kickout_after_unification2(mtv, inerts.tv_eqs);
    kickout_after_unification2(mtv, inerts.mtv_eqs);
    kickout_after_unification2(mtv, inerts.tyfam_eqs);
    kickout_after_unification2(mtv, inerts.dicts);
    kickout_after_unification2(mtv, inerts.irreducible);
}

void Solver::add_inert(const Predicate& p)
{
    if (auto E = to<CanonicalEquality>(p))
    {
        int eq_level = std::max( max_level(E->t1), max_level(E->t2));
        if (eq_level < level() and p.flavor() == Given)
        {
            inerts.given_eq_level = std::max( inerts.given_eq_level.value_or(0), eq_level );
        }
    }

    if (auto E = to<CanonicalEquality>(p))
    {
        auto t1 = follow_meta_type_var(E->t1);
        if (t1.is_a<TypeVar>())
            inerts.tv_eqs.push_back(p);
        else if (t1.is_a<MetaTypeVar>())
            inerts.mtv_eqs.push_back(p);
        else if (is_type_fam_app(t1))
            inerts.tyfam_eqs.push_back(p);
        else
            inerts.irreducible.push_back(p);
    }
    else if (to<CanonicalDict>(p))
    {
        inerts.dicts.push_back(p);
    }
    else
        std::abort();
}

    // FIXME: GHC has removed flattening on Mar 19 2025.
    //        The fix claims that flattening "doesn't really work" and is subsumed by
    //        the ability of the unifier to return (MaybeApart subst) in addition to SurelyApart
    //        and (Unifiable subst).
    //
    //        The modification is in the *pure* unifier.  When we try to unify e.g. (G Float) ~ Int,
    //        the result is not MaybeApart, but we record the conditions under which its unifiable.
    //        Then if we later see (G Float) ~ Double, we can return SurelyApart.
    //
    //        But it doesn't remove cycle-breaker variables!
    //        What is it removing?
    //
    //        Apparently its removing the replacement of type family applications w/ fresh type variables.
    //        If we do that then if we see
    //            a a ~ (Var A) (Var B)   where Var is injective.
    //        we end up with
    //            a a ~ b c
    //        which (I think) unifies under (b->a) and (c->a).  But if we track that Var A -> a, then
    //        Var B -> a violates the injectivity constraint.
    //        
    //        Q: Recording the (G Float) ~ Int is equivalent to extending the type family with G Float -> Int.
    //        But if we record (G Float) ~ (G Int), then do we record G Float -> G Int, or the other way?


    // The simplifier corresponds to the relation |->[simp] from Figure 19 of the OutsideIn(X) paper:
    //    \mathcal{Q}; Q[given] ; alpha[touchable] |->[simp] Q[wanted] ~~> Q[residual]; theta
    // where theta is the substitution -- which we should actually perform here instead of returning as an object.

    // So, we need to to know the set of touchable variables, either through the TypeChecker, or
    // as a function argument.

    // This has a few steps (rule SIMPLES from Figure 19):
    // 1. Run the the rewrite rules until no more are applicable.
    //    We start out with no flattening substitution.
    // 2. Apply the flattening transformation on the wanteds -> Q[rr].
    // 3. \mathcal{E} = the set of (beta ~ tau) or (tau ~ beta) in Q[rr] where
    //    * beta is a touchable unification variable
    //    * beta is not in the free unification variables of tau
    // 4. theta = substitutions (beta -> theta(tau)) from \mathcal{E}, where
    //    * we choose only one substitution for beta if we have multiple equations on beta
    //    * for example, if we have (beta ~ tau1) and (beta ~ tau2), we pick one.
    //    * presumably, we can pick either one -> they would need to agree!
    //    * if they don't, do we do Irresolvable (beta ~ tau1) and Irresolvable (beta ~ tau2)?
    //    * we need to substitute into the taus using the theta that contains the taus!
    //      - that probably works fine if we use meta-vars.
    // 5. Q[r] = Q[rr] - \mathcal{E}
    // 6. Return theta(Q[r])
    //    * we can do this by just performing theta and returning Q[r].

    // The rewrite rules (also Figure 19) include:
    // a. replace a given/wanted by a canonicalized given/wanted (and maybe add a flattening substitution)
    // b. interact a given/wanted Q1 with a given/wanted Q2 and to produce a given/wanted Q3.
    //    - do we REPLACE Q1 and Q2 by Q3, or ADD Q3 to the set?
    // c. react a given Q1 and a wanted Q2 to produce a new wanted Q3 = simplifies(Q1,Q2)
    //    - replace the wanted Q2 by Q3.
    // d. react a given Q1 with axioms in \mathcal{Q} to produce Q2.
    //    - replace Q1 by Q2.
    // e. react a wanted Q1 with axioms in \mathcal{Q} to produce Q2 and new touchable variables beta.
    //    - replace Q1 by Q2 and add in the new touchable variables beta.


bool Solver::contains_type(const Type& t1_, const Type& t2) const
{
    assert(is_rewritable_lhs(t2));

    Type t1 = follow_meta_type_var(t1_);

    if (t1.is_a<MetaTypeVar>() or t1.is_a<TypeVar>())
        return t1 == t2;
    else if (auto con = t1.to<ConstrainedType>())
        return contains_type(con->context, t2) or contains_type(con->type, t2);
    else if (auto forall = t1.to<ForallType>())
        return contains_type(forall->type, t2);
    else if (auto syn = expand_type_synonym(t1))
        return contains_type(*syn, t2);
    else if (auto app = t1.to<TypeApp>())
        return contains_type(app->head, t2) or contains_type(app->arg, t2);
    else if (t1.is_a<TypeCon>())
        return false;
    else
        std::abort();
}

bool Solver::contains_type(const vector<Type>& ts1, const Type& t2) const
{
    for(auto& t1: ts1)
        if (contains_type(t1, t2)) return true;
    return false;
}

bool Solver::can_rewrite(const Predicate& p1, const Predicate& p2) const
{
    // 1. Check the flavor
    if (p1.flavor() == Wanted and p2.flavor() == Given) return false;

    // 2. Check if p1 can be used for rewriting.
    auto eq1 = to<CanonicalEquality>(p1);
    if (not eq1 or not is_rewritable_lhs(eq1->t1)) return false;

    auto lhs = follow_meta_type_var(eq1->t1);

    if (auto dict2 = to<CanonicalDict>(p2))
        return contains_type(dict2->args, lhs);
    else if (auto eq2 = to<CanonicalEquality>(p2))
        return contains_type(eq2->t1, lhs) or contains_type(eq2->t2, lhs);
    else
        std::abort();

    return false;
}

void Solver::kickout_rewritten(const Predicate& p, std::vector<Predicate>& ps)
{
    // kick-out for inerts that are rewritten by p
    for(int i=0;i<ps.size();i++)
    {
        if (can_rewrite(p, ps[i]))
        {
            work_list.push_back(ps[i]);

            if (i+1 < ps.size())
                std::swap(ps[i], ps.back());

            ps.pop_back();
            i--;
            continue;
        }
    }
}

Core2::Decls<> Solver::simplify(const LIE& givens, LIE& wanteds)
{
    if (wanteds.empty()) return {};

    for(auto& wanted: wanteds)
    {
        assert(wanted.flavor == Wanted);
        work_list.push_back( NonCanonical(wanted) );
    }
    // Givens must be processed first!
    for(auto& given: givens)
    {
        assert(given.flavor == Given);
        work_list.push_back( NonCanonical(given) );
    }

//   std::cerr<<"---------------\n";
//   for(auto& w: work_list)
//       std::cerr<<"  "<<w.print()<<"\n";

    while(not work_list.empty())
    {
        auto p = work_list.back(); work_list.pop_back();

        // canonicalize
        if (auto cp = canonicalize(p))
            p = *cp;
        else
            continue;

        // interact
        bool done = false;
        for(auto& inert: views::concat(inerts.tv_eqs, inerts.mtv_eqs, inerts.tyfam_eqs, inerts.dicts, inerts.irreducible, inerts.failed))
        {
            auto I = interact(inert, p);
            if (auto C = to<Changed>(I))
            {
                p = C->P;
            }
            else if (to<Solved>(I) or to<NonCanon>(I))
            {
                done = true;
                break;
            }
        }
        if (done) continue;

        // kick-out for inerts that are rewritten by p
        kickout_rewritten(p, inerts.tv_eqs);
        kickout_rewritten(p, inerts.mtv_eqs);
        kickout_rewritten(p, inerts.tyfam_eqs);
        kickout_rewritten(p, inerts.dicts);
        kickout_rewritten(p, inerts.irreducible);
        kickout_rewritten(p, inerts.failed);

        // top-level reactions
        if (top_react(p))
            continue;


        // perform same-level substitutions
        if (auto E = to<CanonicalEquality>(p); E and p.flavor() == Wanted)
        {
            auto t1 = follow_meta_type_var(E->t1);
            if (auto mtv = t1.to<MetaTypeVar>())
            {
                if (mtv->level() == level())
                {
                    mtv->fill(E->t2);
                    kickout_after_unification(*mtv);
                    continue;
                }
                else if (mtv->level() < level() and is_touchable(*mtv, E->t2))
                {
                    promote(E->t2, mtv->level());
                    set_unification_level(mtv->level());
                    mtv->fill(E->t2);
                    kickout_after_unification(*mtv);
                    // Iterating at the level of mtv reconstructs the inert set from scratch, so ...
                    // we don't need to do kick-out at that level?
                    continue;

                    // 3. maybe track which level the constraints are added on?  that way wanteds on a higher
                    //    level become givens automatically...

                    // 4. create a CtLoc object and set it when we first create constraints...

                    // 5. create a TcLclEnv object, and give one to implications also

                    // 6. what to do with ic_given_eqs on implications?
                }
            }
        }

        // we should only get this far if p is closed under rewriting, and unsolved.
        add_inert(p);
    }

    // Split inert into substitution and remaining constraints
    wanteds.clear();
    for(auto& P: views::concat(inerts.tv_eqs, inerts.mtv_eqs, inerts.tyfam_eqs, inerts.dicts, inerts.irreducible, inerts.failed))
    {
        assert(is_canonical(P));

        if (P.flavor() == Wanted)
            wanteds.push_back(P.constraint());
    }

//    if (not wanteds.empty())
//	std::cerr<<" residual wanteds = \n";
//    for(auto& c: wanteds)
//        std::cerr<<"  "<<c.print()<<"\n";

    return decls;
}

Core2::Decls<> TypeChecker::entails(const LIE& givens, WantedConstraints& wanteds)
{
    Core2::Decls<> decls;
    bool update = false;
    do
    {
        // 1. Simplify the simple wanteds.
        Solver solver(*this);
        decls += solver.simplify(givens, wanteds.simple);
        update = false;

        // 2. Handle implications
        vector<shared_ptr<Implication>> wanted_implics;
        std::swap(wanted_implics, wanteds.implications);
        for(auto& implic: wanted_implics)
        {
            // 3. If there was a unification that affected this level
            //    then save any remaining implications and iterate.
            if (unification_level() and *unification_level() <= level())
            {
                wanteds.implications.push_back( implic );
                continue;
            }

            // 4. construct sub-givens
            LIE sub_givens = implic->givens;
            sub_givens += givens;
            auto given_wanteds = wanteds.simple;
            for(auto& c: given_wanteds)
                c.flavor = Given;
            sub_givens += given_wanteds;

            // 5. try and sub-wanteds
            auto tcs2 = copy_clear_wanteds();
            tcs2.set_level( implic->level );
            *implic->evidence_binds += tcs2.entails(sub_givens, implic->wanteds);

            // 6. Keep implication if not empty.
            if (not implic->wanteds.empty())
                wanteds.implications.push_back( implic );
        }

        // 7. If there was a unification, that affected this level, then we have to iterate.
        if (unification_level() and *unification_level() == level())
        {
            update = true;
            clear_unification_level();
        }

        // Does this work?
        solver.unbreak_type_equality_cycles();
    }
    while(update);

    // This should implement |->[solv] from Figure 14 of the OutsideIn(X) paper:
    //   \mathcal{Q}; Q[given]; alpha[touchable] |->[solv] C[wanted] ~~> Q[residual]; theta
    // where theta is a substitution.

    // This has a few steps (rule SOLVE from Figure 14):
    // 1. Run the simplifier (|->[simp]) on the non-implication constraints to produce Q[r]; theta
    // 2. For each implication constraint alpha[i];Q[i] => C[i]
    //    - apply theta to Q[i] and C[i] (which would happen automatically if we use metavars)
    //    - run solve(\mathcal{Q}, Q[given] + Q[r] + Q[i]; alpha[i], C[i]) |->[solv] empty;theta[i]
    //    - the theta[i] shouldn't affect anything outside the implication we are working on.
    // 3. we return Q[r];theta
    //    - interestingly, the implication constraints don't contribute here.

    return decls;
}

Solver::Solver(const TypeChecker& tc)
    :TypeChecker(tc)
{
}

string InertSet::print() const
{
    vector<string> ps;
    for(auto& pred: views::concat(tv_eqs, mtv_eqs, tyfam_eqs, dicts, irreducible, failed))
    {
        ps.push_back(pred.print());
    }
    return join(ps, "\n");
}
