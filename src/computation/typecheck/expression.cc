#include "typecheck.H"
#include "kindcheck.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;


// OK, so this returns something of type exp_sigma
void typechecker_state::checkSigma(Hs::Expression& E, const Hs::SigmaType& sigma_type)
{
    // 1. skolemize the type
    auto [tvs, givens, rho_type] = skolemize(sigma_type, true);

    // 2. typecheck
    auto tcs2 = copy_clear_lie();
    tcs2.tcRho(E, Check(rho_type));
    auto lie_wanted = apply_current_subst( tcs2.current_lie() );

    // 3.  default ambiguous constraints.
    auto fixed_mtvs = free_meta_type_variables( apply_current_subst(gve) );
    auto [s1, binds1, lie_wanted_unambiguous] = default_preds( fixed_mtvs, {}, lie_wanted );
    auto ev_binds = binds1;

    // 4. check that the remaining constraints are satisfied by the constraints in the type signature
    auto [ev_binds2, lie_failed] = entails( unordered_lie(givens), lie_wanted_unambiguous);
    if (not ev_binds2)
        throw myexception()<<"Can't derive constraints '"<<print(lie_failed)<<"' from specified constraints '"<<print(givens)<<"'";
    ev_binds = *ev_binds2 + ev_binds;

    // 5. modify E, which is of type rho_type, to be of type sigma_type
    if (ev_binds.size())
        E = Hs::LetExp({noloc, ev_binds}, {noloc, E});

    if (givens.size())
    {
        auto dict_pats = vars_from_lie<Hs::Pattern>(givens);
        E = Hs::LambdaExp(dict_pats, E);
    }
}

Hs::Expression typechecker_state::tcRho(const Hs::Var& x, const Expected& exp_type)
{
    if (exp_type.check())
    {
        Hs::Type type;
        auto E = tcRho(x, Infer(type));
        unify(type, exp_type.check_type());
        return E;
    }

    auto& x_name = unloc(x.name);
    if (auto it = mono_local_env.find(x_name))
    {
        auto& [v,type] = *it;

        exp_type.infer_type(type);

        return v;
    }

    auto sigma = gve.find( x_name );

    // x should be in the type environment
    if (not sigma)
        throw myexception()<<"infer_type: can't find type of variable '"<<x.print()<<"'";

    auto [_, wanteds, type] = instantiate(*sigma);

    Hs::Expression E = x;
    if (wanteds.size())
    {
        vector<Hs::Expression> d_args;
        for(auto& [dvar,constraint]: wanteds)
        {
            lie = lie.insert( {unloc(dvar.name), constraint} );
            d_args.push_back( dvar );
        }
        E = Hs::ApplyExp(x, d_args);
    }

    exp_type.infer_type(type);

    return E;
}

void typechecker_state::tcRho(const Hs::Con& con, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(con, exp_type.check_type());
        return;
    }

    auto sigma = constructor_type(con);
    auto [tvs, wanteds, result_type] = instantiate( sigma );

    if (not wanteds.empty())
    {
        myexception e;
        e<<"Constructor "<<unloc(con.name)<<" has constraints!  Type is:\n\n";
        e<<"    "<<sigma<<"\n\n";
        e<<"Constructor constraints are not implemented yet.\n\n";

        throw e;
    }

    exp_type.infer_type(result_type);
}

void typechecker_state::tcRho(Hs::ApplyExp& App, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(App, exp_type.check_type());
        return;
    }

    // If we're checking, the expected type maybe be a rho type sigma_arg -> sigma_result
    // If so, then we can do checkSigma(arg, sigma_arg)
    // Eventually this may lead to checkSigma(var, sigma), which affects how much instantiation we do.

    auto t1 = inferRho(App.head);

    for(int i=0;i<App.args.size();i++)
    {
        auto e = myexception()<<"Applying "<<App.args.size()<<" arguments to function "<<App.head.print()<<", but it only takes "<<i<<"!";

        auto [expected_arg_type, result_type] = unify_function(t1, e);

        checkRho(App.args[i], expected_arg_type);

        t1 = result_type;
    }

    exp_type.infer_type(t1);
}

void typechecker_state::tcRho(Hs::LetExp& Let, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(Let, exp_type.check_type());
        return;
    }

    auto state2 = copy_clear_lie();

    state2.infer_type_for_binds(unloc(Let.binds));

    // 2. Compute type of let body
    state2.tcRho(unloc(Let.body), exp_type);

    current_lie() += state2.current_lie();
}

void typechecker_state::tcRho(Hs::LambdaExp& Lam, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(Lam, exp_type.check_type());
        return;
    }

    auto rule = Hs::MRule{Lam.args, Lam.body};
    tcRho(rule, exp_type);
    Lam.args = rule.patterns;
    Lam.body = rule.rhs;
}

Hs::Expression typechecker_state::tcRho(const Hs::TypedExp& TExp, const Expected& exp_type)
{
    if (exp_type.check())
    {
        Hs::Type type;
        auto E = tcRho(TExp, Infer(type));
        unify(type, exp_type.check_type());
        return E;
    }

    // 1. So, ( e :: tau ) should be equivalent to ( let x :: tau ; x = e in x )
    // according to the 2010 report.

    // Example: (\x -> x) :: Num a => a -> a
    // In this example, we should rewrite this to \dNum -> \x -> x

    // TExp.exp;
    // TExp.type

    // FIXME: For better error messages, we should inline the code for inferring types of LetExp.
    //        We will know we will call infer_type_for_single_fundecl_with_sig

    // 2. I think that we end up typechecking TExp.exp the condition that it has type sigma.
    //    When then in the let body when we see the x, we would need to instantiate the type.

    auto x = get_fresh_Var("tmp", false);
    Hs::Decls decls;
    decls.push_back(simple_decl(x,TExp.exp));
    Hs::Binds binds;
    // By making a LetExp, we rely on the Let code to handle the type here.
    binds.signatures.insert({unloc(x.name), TExp.type});
    binds.push_back(decls);
    expression_ref E2 = Hs::LetExp({noloc,binds},{noloc,x});

    tcRho(E2, exp_type);
    return E2;
}

void typechecker_state::tcRho(Hs::CaseExp& Case, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(Case, exp_type.check_type());
        return;
    }

    // 1. Determine object type
    auto object_type = inferRho(Case.object);

    // 2. Determine data type for object from patterns.
    Hs::Match match;
    for(auto& alt: Case.alts)
    {
        auto& [pattern, body] = unloc(alt);
        match.rules.push_back(Hs::MRule{{pattern},body});
    }

    auto match_type = inferRho(match);

    for(int i=0;i<Case.alts.size();i++)
    {
        unloc(Case.alts[i]) = {match.rules[i].patterns[0], match.rules[i].rhs};
    }

    Hs::Type result_type = fresh_meta_type_var( kind_star() );

    unify( Hs::make_arrow_type(object_type,result_type), match_type );

    exp_type.infer_type(result_type);
}

void typechecker_state::tcRho(Hs::List& L, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(L, exp_type.check_type());
        return;
    }

    Hs::Type element_type = fresh_meta_type_var( kind_star() );

    for(auto& element: L.elements)
        checkRho(element, element_type);

    exp_type.infer_type( Hs::ListType(element_type) );
}

void typechecker_state::tcRho(Hs::Tuple& T, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(T, exp_type.check_type());
        return;
    }

    vector<Hs::Type> element_types;
    for(auto& element: T.elements)
    {
        auto element_type = inferRho(element);
        element_types.push_back( element_type );
    }
    Hs::Type result_type = Hs::TupleType(element_types);
    exp_type.infer_type( result_type );
}

void typechecker_state::tcRho(Hs::Literal& Lit, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(Lit, exp_type.check_type());
        return;
    }

    if (Lit.is_Char())
        exp_type.infer_type( char_type() );
    else if (Lit.is_String())
        exp_type.infer_type( Hs::ListType( char_type() ) );
    else if (Lit.is_BoxedInteger())
        exp_type.infer_type( int_type() );
    else if (auto i = Lit.is_Integer())
    {
        // 1. Typecheck fromInteger
        expression_ref fromInteger = Hs::Var({noloc,"Compiler.Num.fromInteger"});
        auto fromInteger_type = inferRho(fromInteger);

        // 2. Determine result type
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(fromInteger_type, Hs::make_arrow_type(int_type(), result_type));

        Lit = Hs::Literal(Hs::Integer{*i, fromInteger});

        exp_type.infer_type( result_type );
    }
    else if (auto d = Lit.is_Double())
    {
        // 1. Typecheck fromRational
        expression_ref fromRational = Hs::Var({noloc,"Compiler.Num.fromRational"});
        auto fromRational_type = inferRho(fromRational);

        // 2. Determine result type
        Hs::Type result_type = fresh_meta_type_var( kind_star() );
        unify(fromRational_type, Hs::make_arrow_type(double_type(), result_type));

        Lit = Hs::Literal(Hs::Double{*d, fromRational});
        exp_type.infer_type( result_type );
    }
    else
        std::abort();
}

void typechecker_state::tcRho(Hs::IfExp& If, const Expected& exp_type)
{
    checkRho(unloc(If.condition), bool_type());

    auto result_type = expTypeToType(exp_type);
    checkRho(unloc(If.true_branch), result_type);
    checkRho(unloc(If.false_branch), result_type);
}


void typechecker_state::tcRho(Hs::LeftSection& LSec, const Expected& exp_type)
{
    if (exp_type.check())
        checkRho(LSec, exp_type.check_type());

    // 1. Typecheck the op
    auto op_type = inferRho(LSec.op);

    // 2. Check that the op is a function
    auto e = myexception()<<"In left section, "<<LSec.op<<" is not a function!";
    auto [left_arg_type, result_type] = unify_function(op_type, e);

    // 3. Typecheck the left argument
    checkRho(LSec.l_arg, left_arg_type);

    exp_type.infer_type( result_type );
}

void typechecker_state::tcRho(Hs::RightSection& RSec, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(RSec, exp_type.check_type());
        return;
    }

    // 1. Typecheck the op
    auto op_type = inferRho(RSec.op);

    // 2. Typecheck the right argument
    auto right_arg_type = inferRho(RSec.r_arg);

    // 3. Typecheck the function application:  op left_arg right_arg
    auto left_arg_type = fresh_meta_type_var( kind_star() );
    auto result_type = fresh_meta_type_var( kind_star() );
    unify(op_type, Hs::function_type({left_arg_type, right_arg_type}, result_type));

    // 4. Compute the section type;
    Hs::Type section_type = Hs::function_type({left_arg_type}, result_type);

    exp_type.infer_type( section_type );
}

void typechecker_state::tcRho(Hs::Do& DoExp, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(DoExp, exp_type.check_type());
        return;
    }

    tcRhoStmts(0, DoExp.stmts.stmts, exp_type);
}

void typechecker_state::tcRho(Hs::ListComprehension& LComp, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(LComp, exp_type.check_type());
        return;
    }

    auto state2 = copy_clear_lie();
    state2.infer_quals_type(LComp.quals);
    auto body_type = state2.inferRho(LComp.body);

    current_lie() += state2.current_lie();

    exp_type.infer_type( Hs::ListType(body_type) );
}

void typechecker_state::tcRho(Hs::ListFrom& L, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(L, exp_type.check_type());
        return;
    }

    // 1. Typecheck enumFrom
    L.enumFromOp = Hs::Var({noloc,"Compiler.Enum.enumFrom"});
    auto enumFrom_type = inferRho(L.enumFromOp);

    // 2. Typecheck from argument
    auto from_type = inferRho(L.from);

    // 3. enumFrom_type ~ from_type -> result_type
    auto result_type = fresh_meta_type_var( kind_star() );
    unify(enumFrom_type, Hs::make_arrow_type(from_type, result_type));

    exp_type.infer_type( result_type );
}

void typechecker_state::tcRho(Hs::ListFromThen& L, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(L, exp_type.check_type());
        return;
    }

    // 1. Typecheck enumFrom
    L.enumFromThenOp = Hs::Var({noloc,"Compiler.Enum.enumFromThen"});
    auto enumFromThen_type = inferRho(L.enumFromThenOp);

    // 2. Typecheck from argument
    auto from_type = inferRho(L.from);

    // 3. enumFromThen_type ~ from_type -> a
    auto a = fresh_meta_type_var( kind_star() );
    unify(enumFromThen_type, Hs::make_arrow_type(from_type, a));

    // 4. Typecheck then argument
    auto then_type = inferRho(L.then);

    // 5. a ~ then_type -> result_type
    auto result_type = fresh_meta_type_var( kind_star() );
    unify(a, Hs::make_arrow_type(then_type, result_type));
        
    exp_type.infer_type( result_type );
}

void typechecker_state::tcRho(Hs::ListFromTo& L, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(L, exp_type.check_type());
        return;
    }

    // 1. Typecheck enumFrom
    L.enumFromToOp = Hs::Var({noloc,"Compiler.Enum.enumFromTo"});
    auto enumFromTo_type = inferRho(L.enumFromToOp);

    // 2. Typecheck from argument
    auto from_type = inferRho(L.from);

    // 3. enumFromTo_type ~ from_type -> a
    auto a = fresh_meta_type_var( kind_star() );
    unify(enumFromTo_type, Hs::make_arrow_type(from_type, a));

    // 4. Typecheck to argument
    auto to_type = inferRho(L.to);

    // 5. a ~ to_type -> result_type
    auto result_type = fresh_meta_type_var( kind_star() );
    unify(a, Hs::make_arrow_type(to_type, result_type));
        
    exp_type.infer_type( result_type );
}

void typechecker_state::tcRho(Hs::ListFromThenTo& L, const Expected& exp_type)
{
    if (exp_type.check())
    {
        checkRho(L, exp_type.check_type());
        return;
    }

    // 1. Typecheck enumFromThenTo
    L.enumFromThenToOp = Hs::Var({noloc,"Compiler.Enum.enumFromThenTo"});
    auto enumFromThenTo_type = inferRho(L.enumFromThenToOp);

    // 2. Typecheck from argument
    auto from_type = inferRho(L.from);

    // 3. enumFromThenTo_type ~ from_type -> a
    auto a = fresh_meta_type_var( kind_star() );
    unify(enumFromThenTo_type, Hs::make_arrow_type(from_type, a));

    // 4. Typecheck then argument
    auto then_type = inferRho(L.then);

    // 5. a ~ then_type -> b
    auto b = fresh_meta_type_var( kind_star() );
    unify(a, Hs::make_arrow_type(then_type, b));

    // 6. Typecheck to argument
    auto to_type = inferRho(L.to);

    // 7. b ~ to_type -> result_type
    auto result_type = fresh_meta_type_var( kind_star() );
    unify(b, Hs::make_arrow_type(to_type, result_type));

    exp_type.infer_type( result_type );
}

void typechecker_state::tcRho(expression_ref& E, const Expected& exp_type)
{
    // VAR
    if (auto x = E.to<Hs::Var>())
    {
        Hs::Type type;
        E = tcRho(*x, exp_type);
    }
    // CON
    else if (auto con = E.to<Hs::Con>())
    {
        tcRho(*con, exp_type);
    }
    // APP
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        auto App = *app;
        tcRho(App, exp_type);
        E = App;
    }
    // LAMBDA
    else if (auto lam = E.to<Hs::LambdaExp>())
    {
        auto Lam = *lam;
        tcRho(Lam, exp_type);
        E = Lam;
    }
    // LET
    else if (auto let = E.to<Hs::LetExp>())
    {
        auto Let = *let;
        tcRho(Let, exp_type);
        E = Let;
    }
    // CASE
    else if (auto case_exp = E.to<Hs::CaseExp>())
    {
        auto Case = *case_exp;
        tcRho(Case, exp_type);
        E = Case;
    }
    // EXP :: sigma
    else if (auto texp = E.to<Hs::TypedExp>())
    {
        auto TExp = *texp;
        E = tcRho(TExp, exp_type);
    }
    // LITERAL
    else if (auto L = E.to<Hs::Literal>())
    {
        auto Lit = *L;
        tcRho(Lit, exp_type);
        E = Lit;
    }
    // LIST
    else if (auto l = E.to<Hs::List>())
    {
        auto L = *l;
        tcRho(L, exp_type);
        E = L;
    }
    // TUPLE
    else if (auto tup = E.to<Hs::Tuple>())
    {
        auto T = *tup;
        tcRho(T, exp_type);
        E = T;
    }
    else if (is_non_apply_op_exp(E))
    {
        std::abort();
        // this includes builtins like Prelude::add
    }
    // IF
    else if (auto if_exp = E.to<Hs::IfExp>())
    {
        auto If = *if_exp;
        tcRho(If, exp_type);
        E = If;
    }
    // LEFT section
    else if (auto lsec = E.to<Hs::LeftSection>())
    {
        auto LSec = *lsec;
        tcRho(LSec, exp_type);
        E = LSec;
    }
    // Right section
    else if (auto rsec = E.to<Hs::RightSection>())
    {
        auto RSec = *rsec;
        tcRho(RSec, exp_type);
        E = RSec;
    }
    // DO expression
    else if (auto do_exp = E.to<Hs::Do>())
    {
        auto DoExp = *do_exp;
        tcRho(DoExp, exp_type);
        E = DoExp;
    }
    // LISTCOMP
    else if (auto lcomp = E.to<Hs::ListComprehension>())
    {
        auto LComp = *lcomp;
        tcRho(LComp, exp_type);
        E = LComp;
    }
    // ENUM-FROM
    else if (auto l = E.to<Hs::ListFrom>())
    {
        auto L = *l;
        tcRho(L, exp_type);
        E = L;
    }
    // ENUM-FROM-THEN
    else if (auto l = E.to<Hs::ListFromThen>())
    {
        auto L = *l;
        tcRho(L, exp_type);
        E = L;
    }
    // ENUM-FROM-TO
    else if (auto l = E.to<Hs::ListFromTo>())
    {
        auto L = *l;
        tcRho(L, exp_type);
        E = L;
    }
    // ENUM-FROM-THEN-TO
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
        auto L = *l;
        tcRho(L, exp_type);
        E = L;
    }
    else
        throw myexception()<<"type check expression: I don't recognize expression '"<<E<<"'";
}
