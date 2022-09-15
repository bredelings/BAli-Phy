#include "typecheck.H"
#include "kindcheck.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;


void typechecker_state::tcRho(Hs::Var& x, const Expected& exp_type)
{
    Hs::Type sigma;
    // First look for x in the local type environment
    if (auto& x_name = unloc(x.name); auto it = mono_local_env.find(x_name))
    {
        auto& [v,type] = *it;
        // translate to the monomorpic id;
        x = v;
        sigma = type;
    }
    // x should be in the global type environment
    else if (auto sigma_ptr = gve.find( x_name ))
        sigma = *sigma_ptr;
    // x should be in the global type environment
    else if (auto sigma_ptr = imported_gve.find( x_name ))
        sigma = *sigma_ptr;
    else
        throw myexception()<<"infer_type: can't find type of variable '"<<x.print()<<"'";

    try
    {
        auto w = instantiateSigma(sigma, exp_type);
        x.wrap = w;
    }
    catch (myexception& ex)
    {
        std::ostringstream header;
        if (x.name.loc)
            header<<"At location "<<*x.name.loc<<"\n";
        header<<"In expression\n\n";
        header<<"   "<<unloc(x.name)<<"\n\n";
        ex.prepend(header.str());
        throw;
    }
}

void typechecker_state::tcRho(const Hs::Con& con, const Expected& exp_type)
{
    auto sigma = constructor_type(con);

    try
    {
        auto w = instantiateSigma(sigma, exp_type);
    }
    catch (myexception& ex)
    {
        std::ostringstream header;
        if (con.name.loc)
            header<<"At location "<<*con.name.loc<<"\n";
        header<<"In expression\n\n";
        header<<"   "<<unloc(con.name)<<"\n\n";
        ex.prepend(header.str());
        throw;
    }


    // We should actually return w(con).
    // However, since (i) there are no wanteds and (ii) we aren't applying type variables,
    //     the wrapper has to be an identity for the moment.
}

void typechecker_state::tcRho(Hs::ApplyExp& App, const Expected& exp_type, int i)
{
    int arg_index = int(App.args.size())-1-i;

    Hs::Type fun_type;
    if (arg_index > 0)
        tcRho(App, Infer(fun_type), i + 1);
    else
        tcRho(App.head, Infer(fun_type));

    auto e = myexception()<<"Applying "<<(arg_index+1)<<" arguments to function "<<App.head.print()<<", but it only takes "<<i<<"!";
    auto [arg_type, result_type] = unify_function(fun_type, e);

    // Check the argument according to its required type
    auto wrap_arg = checkSigma(App.args[arg_index], arg_type);
    App.arg_wrappers.push_back(wrap_arg);

    // Convert the result to the expected time for the term
    try {
        auto wrap_res = instantiateSigma(result_type, exp_type);
        App.res_wrappers.push_back(wrap_res);
    }
    catch (myexception& ex)
    {
        std::ostringstream header;
//        header<<"At location "<<con.name.loc<<"\n";
        header<<"In expression\n\n";
        header<<"   "<<App.print()<<"\n\n";
        ex.prepend(header.str());
        throw;
    }
}

void typechecker_state::tcRho(Hs::LetExp& Let, const Expected& exp_type)
{
    auto state2 = copy_clear_lie();

    state2.infer_type_for_binds(unloc(Let.binds));

    // 2. Compute type of let body
    state2.tcRho(unloc(Let.body), exp_type);

    current_lie() += state2.current_lie();
}

void typechecker_state::tcRho(Hs::LambdaExp& Lam, const Expected& exp_type)
{
    auto rule = Hs::MRule{Lam.args, Lam.body};
    tcRho(rule, exp_type);
    Lam.args = rule.patterns;
    Lam.body = rule.rhs;
}

void typechecker_state::tcRho(Hs::TypedExp& TExp, const Expected& exp_type)
{
    TExp.type = check_type(TExp.type);
    Core::wrapper w1 = checkSigma( TExp.exp, TExp.type );
    Core::wrapper w2 = instantiateSigma(TExp.type, exp_type);
    TExp.wrap = w1 * w2;
}

void typechecker_state::tcRho(Hs::CaseExp& Case, const Expected& exp_type)
{
    // 1. Determine the match type
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

    // 2. Check the expected result type
    auto [object_type, result_type] = unify_function(match_type);
    set_expected_type( exp_type, result_type );

    // 3. Check the object type
    checkRho(Case.object, object_type);
}

void typechecker_state::tcRho(Hs::List& L, const Expected& exp_type)
{
    Hs::Type element_type = fresh_meta_type_var( kind_star() );
    set_expected_type( exp_type, Hs::ListType(element_type) );

    for(auto& element: L.elements)
        checkRho(element, element_type);
}

void typechecker_state::tcRho(Hs::Tuple& T, const Expected& exp_type)
{
    vector<Hs::Type> element_types;
    for(auto& element: T.elements)
    {
        auto element_type = inferRho(element);
        element_types.push_back( element_type );
    }

    set_expected_type( exp_type, Hs::TupleType(element_types) );
}

void typechecker_state::tcRho(Hs::Literal& Lit, const Expected& exp_type)
{
    if (Lit.is_Char())
        set_expected_type( exp_type, char_type() );
    else if (Lit.is_String())
        set_expected_type( exp_type, Hs::ListType( char_type() ) );
    else if (Lit.is_BoxedInteger())
        set_expected_type( exp_type, int_type() );
    else if (auto i = Lit.is_Integer())
    {
        // 1. Typecheck fromInteger
        expression_ref fromInteger = Hs::Var({noloc,"Compiler.Num.fromInteger"});
        auto fromInteger_type = inferRho(fromInteger);

        // 2. Check result type
        auto [arg_type, result_type] = unify_function(fromInteger_type);
        set_expected_type( exp_type, result_type );

        // 3. The argument type should be Int
        unify(arg_type, int_type());

        Lit = Hs::Literal(Hs::Integer{*i, fromInteger});
    }
    else if (auto d = Lit.is_Double())
    {
        // 1. Typecheck fromRational
        expression_ref fromRational = Hs::Var({noloc,"Compiler.Real.fromRational"});
        auto fromRational_type = inferRho(fromRational);

        // 2. Check result type
        auto [arg_type, result_type] = unify_function(fromRational_type);
        set_expected_type( exp_type, result_type );

        // 3. The argument type should be Double (well, not not really, but for now....)
        unify(arg_type, double_type());

        Lit = Hs::Literal(Hs::Double{*d, fromRational});
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
    // 1. Typecheck the op
    auto op_type = inferRho(LSec.op);

    // 2. Check that the op is a function
    auto e = myexception()<<"In left section, "<<LSec.op<<" is not a function!";
    auto [left_arg_type, result_type] = unify_function(op_type, e);

    // 3. Check expected type
    set_expected_type(exp_type, result_type);

    // 4. Typecheck the left argument
    checkRho(LSec.l_arg, left_arg_type);
}

void typechecker_state::tcRho(Hs::RightSection& RSec, const Expected& exp_type)
{
    // 1. Typecheck the op
    auto op_type = inferRho(RSec.op);

    // 2. Check the op is a two-argument function
    auto [left_arg_type, tmp1] = unify_function(op_type);
    auto [right_arg_type, result_type] = unify_function(tmp1);

    // 3. Check the expected type
    Hs::Type section_type = Hs::function_type({left_arg_type}, result_type);
    set_expected_type( exp_type, section_type );

    // 4. Check the right arg type.
    checkRho(RSec.r_arg, right_arg_type);
}

void typechecker_state::tcRho(Hs::Do& DoExp, const Expected& exp_type)
{
    tcRhoStmts(0, DoExp.stmts.stmts, exp_type);
}

void typechecker_state::tcRho(Hs::ListComprehension& LComp, const Expected& exp_type)
{
    auto state2 = copy_clear_lie();
    state2.infer_quals_type(LComp.quals);
    auto body_type = state2.inferRho(LComp.body);

    current_lie() += state2.current_lie();

    set_expected_type( exp_type, Hs::ListType(body_type) );
}

void typechecker_state::tcRho(Hs::ListFrom& L, const Expected& exp_type)
{
    // 1. Typecheck enumFrom
    L.enumFromOp = Hs::Var({noloc,"Compiler.Enum.enumFrom"});
    auto enumFrom_type = inferRho(L.enumFromOp);

    // 2. Check the result type
    auto [from_type, result_type] = unify_function(enumFrom_type);

    set_expected_type( exp_type, result_type );

    // 3. Typecheck from argument
    checkRho(L.from, from_type);
}

void typechecker_state::tcRho(Hs::ListFromThen& L, const Expected& exp_type)
{
    // 1. Typecheck enumFrom
    L.enumFromThenOp = Hs::Var({noloc,"Compiler.Enum.enumFromThen"});
    auto enumFromThen_type = inferRho(L.enumFromThenOp);

    // 2. Check the result type
    auto [from_type, tmp1] = unify_function(enumFromThen_type);
    auto [then_type, result_type] = unify_function(tmp1);

    set_expected_type( exp_type, result_type );

    // 3. Typecheck from argument
    checkRho(L.from, from_type);

    // 4. Typecheck then argument
    checkRho(L.then, then_type);
}

void typechecker_state::tcRho(Hs::ListFromTo& L, const Expected& exp_type)
{
    // 1. Typecheck enumFrom
    L.enumFromToOp = Hs::Var({noloc,"Compiler.Enum.enumFromTo"});
    auto enumFromTo_type = inferRho(L.enumFromToOp);

    // 2. Check the result type
    auto [from_type, tmp1] = unify_function(enumFromTo_type);
    auto [to_type, result_type] = unify_function(tmp1);

    set_expected_type( exp_type, result_type );

    // 3. Typecheck from argument
    checkRho(L.from, from_type);

    // 4. Typecheck to argument
    checkRho(L.to, to_type);
}

void typechecker_state::tcRho(Hs::ListFromThenTo& L, const Expected& exp_type)
{
    // 1. Typecheck enumFromThenTo
    L.enumFromThenToOp = Hs::Var({noloc,"Compiler.Enum.enumFromThenTo"});
    auto enumFromThenTo_type = inferRho(L.enumFromThenToOp);

    // 2. Check the result type
    auto [from_type, tmp1] = unify_function(enumFromThenTo_type);
    auto [then_type,  tmp2] = unify_function(tmp1);
    auto [to_type, result_type] = unify_function(tmp2);

    set_expected_type( exp_type, result_type );

    // 3. Typecheck from argument
    checkRho(L.from, from_type);

    // 4. Typecheck then argument
    checkRho(L.then, then_type);

    // 5. Typecheck to argument
    checkRho(L.to, to_type);
}

void typechecker_state::tcRho(expression_ref& E, const Expected& exp_type)
{
    // VAR
    if (auto x = E.to<Hs::Var>())
    {
        auto X = *x;
        tcRho(X, exp_type);
        E = X;
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
        tcRho(TExp, exp_type);
        E = TExp;
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
