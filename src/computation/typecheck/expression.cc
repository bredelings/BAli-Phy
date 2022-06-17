#include "typecheck.H"
#include "kindcheck.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

Hs::Expression typechecker_state::tcRho(const Hs::Var& x, const Expected& exp_rho)
{
    auto& x_name = unloc(x.name);
    if (auto it = mono_local_env.find(x_name))
    {
        auto& [v,type] = *it;

        exp_rho.infer_type() = type;

        return v;
    }

    auto sigma = gve.find( x_name );

    // x should be in the type environment
    if (not sigma)
        throw myexception()<<"infer_type: can't find type of variable '"<<x.print()<<"'";

    auto [_, constraints, type] = instantiate(*sigma);

    Hs::Expression E = x;
    if (constraints.size())
    {
        vector<Hs::Expression> d_args;
        for(auto& constraint: constraints)
        {
            auto dvar = add_dvar(constraint);
            d_args.push_back( dvar );
        }
        E = Hs::ApplyExp(x, d_args);
    }

    exp_rho.infer_type() = type;

    return E;
}

void typechecker_state::tcRho(const Hs::Con& con, const Expected& exp_type)
{
    auto sigma = constructor_type(con);
    auto [tvs, constraints, result_type] = instantiate( sigma );

    if (not constraints.empty())
    {
        myexception e;
        e<<"Constructor "<<unloc(con.name)<<" has constraints!  Type is:\n\n";
        e<<"    "<<sigma<<"\n\n";
        e<<"Constructor constraints are not implemented yet.\n\n";

        throw e;
    }

    exp_type.infer_type() = result_type;
}

void typechecker_state::tcRho(Hs::ApplyExp& App, const Expected& exp_type)
{
    // If we're checking, the expected type maybe be a rho type sigma_arg -> sigma_result
    // If so, then we can do checkSigma(arg, sigma_arg)
    // Eventually this may lead to checkSigma(var, sigma), which affects how much instantiation we do.

    auto t1 = inferRho(App.head);

    for(int i=0;i<App.args.size();i++)
    {
        // Fixme -- throwing an error from inside unify_function( ) would simply this a lot.
        // The paper calls unify_function on an EXPRESSION, not a type.
        auto arg_result_types = unify_function(t1);
        if (not arg_result_types)
            throw myexception()<<"Applying "<<App.args.size()<<" arguments to function "<<App.head.print()<<", but it only takes "<<i<<"!";

        auto [expected_arg_type, result_type] = *arg_result_types;

        checkRho(App.args[i], expected_arg_type);
        t1 = result_type;
    }

    exp_type.infer_type() = t1;
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
    auto t = inferRho(rule);
    Lam.args = rule.patterns;
    Lam.body = rule.rhs;
    exp_type.infer_type() = t;
}

Hs::Expression typechecker_state::tcRho(const Hs::TypedExp& TExp, const Expected& exp_type)
{
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

    exp_type.infer_type() = result_type;
}

void typechecker_state::tcRho(Hs::List& L, const Expected& exp_type)
{
    Hs::Type element_type = fresh_meta_type_var( kind_star() );

    for(auto& element: L.elements)
    {
        auto t1 = inferRho(element);

        try {
            unify(t1, element_type);
        }
        catch (myexception& e)
        {
            std::ostringstream header;
            header<<"List element "<<element<<" has type "<<apply_current_subst(t1)<<" but expected type "<<apply_current_subst(element_type)<<"\n ";
            e.prepend(header.str());
            throw;
        }
    }

    exp_type.infer_type() = Hs::ListType(element_type);
}

void typechecker_state::tcRho(Hs::Tuple& T, const Expected& exp_type)
{
    vector<Hs::Type> element_types;
    for(auto& element: T.elements)
    {
        auto element_type = inferRho(element);
        element_types.push_back( element_type );
    }
    Hs::Type result_type = Hs::TupleType(element_types);
    exp_type.infer_type() = result_type;
}

void typechecker_state::tcRho(Hs::Literal& Lit, const Expected& exp_type)
{
    if (Lit.is_Char())
        exp_type.infer_type() = char_type();
    else if (Lit.is_String())
        exp_type.infer_type() = Hs::ListType( char_type() );
    else if (Lit.is_BoxedInteger())
        exp_type.infer_type() = int_type();
    else if (auto i = Lit.is_Integer())
    {
        // 1. Typecheck fromInteger
        expression_ref fromInteger = Hs::Var({noloc,"Compiler.Num.fromInteger"});
        auto fromInteger_type = inferRho(fromInteger);

        // 2. Determine result type
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(fromInteger_type, Hs::make_arrow_type(int_type(), result_type));

        Lit = Hs::Literal(Hs::Integer{*i, fromInteger});

        exp_type.infer_type() = result_type;
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
        exp_type.infer_type() = result_type;
    }
    else
        std::abort();
}

void typechecker_state::tcRho(Hs::IfExp& If, const Expected& exp_type)
{
    auto cond_type = inferRho(unloc(If.condition));
    auto tbranch_type = inferRho(unloc(If.true_branch));
    auto fbranch_type = inferRho(unloc(If.false_branch));

    unify(cond_type, bool_type());
    unify(tbranch_type, fbranch_type);
    exp_type.infer_type() = tbranch_type;
}


void typechecker_state::tcRho(Hs::LeftSection& LSec, const Expected& exp_type)
{
    // 1. Typecheck the op
    auto op_type = inferRho(LSec.op);

    // 2. Typecheck the left argument
    auto left_arg_type = inferRho(LSec.l_arg);

    // 3. Typecheck the function application
    auto result_type = fresh_meta_type_var( kind_star() );
    unify(op_type, Hs::make_arrow_type(left_arg_type, result_type));

    exp_type.infer_type() = result_type;
}

void typechecker_state::tcRho(Hs::RightSection& RSec, const Expected& exp_type)
{
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

    exp_type.infer_type() = section_type;
}

void typechecker_state::tcRho(Hs::Do& DoExp, const Expected& exp_type)
{
    exp_type.infer_type() = infer_stmts_type(0, DoExp.stmts.stmts);
}

void typechecker_state::tcRho(Hs::ListComprehension& LComp, const Expected& exp_type)
{
    auto state2 = copy_clear_lie();
    state2.infer_quals_type(LComp.quals);
    auto body_type = state2.inferRho(LComp.body);

    current_lie() += state2.current_lie();

    exp_type.infer_type() = Hs::ListType(body_type);
}

void typechecker_state::tcRho(Hs::ListFrom& L, const Expected& exp_type)
{
    // 1. Typecheck enumFrom
    L.enumFromOp = Hs::Var({noloc,"Compiler.Enum.enumFrom"});
    auto enumFrom_type = inferRho(L.enumFromOp);

    // 2. Typecheck from argument
    auto from_type = inferRho(L.from);

    // 3. enumFrom_type ~ from_type -> result_type
    auto result_type = fresh_meta_type_var( kind_star() );
    unify(enumFrom_type, Hs::make_arrow_type(from_type, result_type));

    exp_type.infer_type() = result_type;
}

void typechecker_state::tcRho(Hs::ListFromThen& L, const Expected& exp_type)
{
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
        
    exp_type.infer_type() = result_type;
}

void typechecker_state::tcRho(Hs::ListFromTo& L, const Expected& exp_type)
{
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
        
    exp_type.infer_type() = result_type;
}

void typechecker_state::tcRho(Hs::ListFromThenTo& L, const Expected& exp_type)
{
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

    exp_type.infer_type() = result_type;
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

// FIXME -- we need the location!
void typechecker_state::checkRho(Hs::Expression& e, const Hs::Type& exp_rho)
{
    auto orig_e = e;
    auto type = inferRho(e);
    try
    {
        unify(type, exp_rho);
    }
    catch (myexception& ex)
    {
        std::ostringstream header;
        header<<"Expected type\n\n";
        header<<"   "<<apply_current_subst(exp_rho)<<"\n\n";
        header<<"but got type\n\n";
        header<<"   "<<apply_current_subst(type)<<"\n\n";
        header<<"for expression\n\n";
        header<<"   "<<orig_e<<"\n";
        
        ex.prepend(header.str());
        throw;
    }
}
