#include "typecheck.H"
#include "kindcheck.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

std::pair<Hs::Expression,Hs::Type> typechecker_state::inferRho(const Hs::Var& x)
{
    Hs::Type result_type;
    auto E = tcRho(x, Infer(result_type));
    return {E, result_type};
}

Hs::Expression typechecker_state::tcRho(const Hs::Var& x, const Expected& exp_rho)
{
    auto& x_name = unloc(x.name);
    if (auto it = mono_local_env.find(x_name))
    {
        auto& [v,type] = *it;

        if (exp_rho.infer())
            exp_rho.infer_type() = type;
        else
            unify(type, exp_rho.check_type());

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

    if (exp_rho.infer())
        exp_rho.infer_type() = type;
    else
        unify(type, exp_rho.check_type());

    return E;
}

Hs::Type typechecker_state::inferRho(const Hs::Con& con)
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

    return result_type;
}

void typechecker_state::tcRho(Hs::ApplyExp& App, const Expected& exp_type)
{
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

    if (exp_type.infer())
        exp_type.infer_type() = t1;
    else
        unify(t1, exp_type.check_type());
}

Hs::Type typechecker_state::inferRho(Hs::ApplyExp& App)
{
    Hs::Type result_type;
    tcRho(App, Infer(result_type));
    return result_type;
}

void typechecker_state::checkRho(Hs::ApplyExp& App, const Hs::Type& exp_type)
{
    tcRho(App, Check(exp_type));
}

Hs::Type typechecker_state::inferRho(Hs::LetExp& Let)
{
    auto state2 = copy_clear_lie();

    state2.infer_type_for_binds(unloc(Let.binds));

    // 2. Compute type of let body
    auto t_body = state2.inferRho(unloc(Let.body));

    current_lie() += state2.current_lie();

    return t_body;
}

Hs::Type typechecker_state::inferRho(Hs::LambdaExp& Lam)
{
    auto rule = Hs::MRule{Lam.args, Lam.body};
    auto t = inferRho(rule);
    Lam.args = rule.patterns;
    Lam.body = rule.rhs;
    return t;
}

std::pair<Hs::Expression,Hs::Type> typechecker_state::inferRho(const Hs::TypedExp& TExp)
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

    Hs::Type t = inferRho(E2);
    return {E2, t};
}

Hs::Type typechecker_state::inferRho(Hs::CaseExp& Case)
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

    return result_type;
}

Hs::Type typechecker_state::inferRho(Hs::List& L)
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

    return Hs::ListType(element_type);
}

Hs::Type typechecker_state::inferRho(Hs::Tuple& T)
{
    vector<Hs::Type> element_types;
    for(auto& element: T.elements)
    {
        auto element_type = inferRho(element);
        element_types.push_back( element_type );
    }
    Hs::Type result_type = Hs::TupleType(element_types);
    return result_type;
}

Hs::Type typechecker_state::inferRho(Hs::Literal& Lit)
{
    if (Lit.is_Char())
        return char_type();
    else if (Lit.is_String())
        return Hs::ListType( char_type() );
    else if (Lit.is_BoxedInteger())
        return int_type();
    else if (auto i = Lit.is_Integer())
    {
        // 1. Typecheck fromInteger
        expression_ref fromInteger = Hs::Var({noloc,"Compiler.Num.fromInteger"});
        auto fromInteger_type = inferRho(fromInteger);

        // 2. Determine result type
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(fromInteger_type, Hs::make_arrow_type(int_type(), result_type));

        Lit = Hs::Literal(Hs::Integer{*i, fromInteger});

        return result_type;
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
        return result_type;
    }
    else
        std::abort();
}

Hs::Type typechecker_state::inferRho(Hs::IfExp& If)
{
    auto cond_type = inferRho(unloc(If.condition));
    auto tbranch_type = inferRho(unloc(If.true_branch));
    auto fbranch_type = inferRho(unloc(If.false_branch));

    unify(cond_type, bool_type());
    unify(tbranch_type, fbranch_type);
    return tbranch_type;
}


Hs::Type typechecker_state::inferRho(Hs::LeftSection& LSec)
{
    // 1. Typecheck the op
    auto op_type = inferRho(LSec.op);

    // 2. Typecheck the left argument
    auto left_arg_type = inferRho(LSec.l_arg);

    // 3. Typecheck the function application
    auto result_type = fresh_meta_type_var( kind_star() );
    unify(op_type, Hs::make_arrow_type(left_arg_type, result_type));

    return result_type;
}

Hs::Type typechecker_state::inferRho(Hs::RightSection& RSec)
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

    return section_type;
}

Hs::Type typechecker_state::inferRho(Hs::Do& DoExp)
{
    return infer_stmts_type(0, DoExp.stmts.stmts);
}

Hs::Type typechecker_state::inferRho(Hs::ListComprehension& LComp)
{
    auto state2 = copy_clear_lie();
    state2.infer_quals_type(LComp.quals);
    auto exp_type = state2.inferRho(LComp.body);

    current_lie() += state2.current_lie();

    return Hs::ListType(exp_type);
}

Hs::Type typechecker_state::inferRho(Hs::ListFrom& L)
{
    // 1. Typecheck enumFrom
    L.enumFromOp = Hs::Var({noloc,"Compiler.Enum.enumFrom"});
    auto enumFrom_type = inferRho(L.enumFromOp);

    // 2. Typecheck from argument
    auto from_type = inferRho(L.from);

    // 3. enumFrom_type ~ from_type -> result_type
    auto result_type = fresh_meta_type_var( kind_star() );
    unify(enumFrom_type, Hs::make_arrow_type(from_type, result_type));

    return result_type;
}

Hs::Type typechecker_state::inferRho(Hs::ListFromThen& L)
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
        
    return result_type;
}

Hs::Type typechecker_state::inferRho(Hs::ListFromTo& L)
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
        
    return result_type;
}

Hs::Type typechecker_state::inferRho(Hs::ListFromThenTo& L)
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

    return result_type;
}

Hs::Type typechecker_state::inferRho(expression_ref& E)
{
    // VAR
    if (auto x = E.to<Hs::Var>())
    {
        auto [E2, type] = inferRho(*x);
        E = E2;
        return type;
    }
    // CON
    else if (auto con = E.to<Hs::Con>())
    {
        return inferRho(*con);
    }
    // APP
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        auto App = *app;
        auto type = inferRho(App);
        E = App;
        return type;
    }
    // LAMBDA
    else if (auto lam = E.to<Hs::LambdaExp>())
    {
        auto Lam = *lam;
        auto t = inferRho(Lam);
        E = Lam;
        return t;
    }
    // LET
    else if (auto let = E.to<Hs::LetExp>())
    {
        auto Let = *let;
        auto t = inferRho(Let);
        E = Let;
        return t;
    }
    // CASE
    else if (auto case_exp = E.to<Hs::CaseExp>())
    {
        auto Case = *case_exp;
        auto type = inferRho(Case);
        E = Case;
        return type;
    }
    // EXP :: sigma
    else if (auto texp = E.to<Hs::TypedExp>())
    {
        auto TExp = *texp;
        auto [E2,type] = inferRho(TExp);
        E = E2;
        return type;
    }
    // LITERAL
    else if (auto L = E.to<Hs::Literal>())
    {
        auto Lit = *L;
        auto type = inferRho(Lit);
        E = Lit;
        return type;
    }
    // LIST
    else if (auto l = E.to<Hs::List>())
    {
        auto L = *l;
        auto type = inferRho(L);
        E = L;
        return type;
    }
    // TUPLE
    else if (auto tup = E.to<Hs::Tuple>())
    {
        auto T = *tup;
        auto type = inferRho(T);
        E = T;
        return type;
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
        auto type = inferRho(If);
        E = If;
        return type;
    }
    // LEFT section
    else if (auto lsec = E.to<Hs::LeftSection>())
    {
        auto LSec = *lsec;
        auto type = inferRho(LSec);
        E = LSec;
        return type;
    }
    // Right section
    else if (auto rsec = E.to<Hs::RightSection>())
    {
        auto RSec = *rsec;
        auto type = inferRho(RSec);
        E = RSec;
        return type;
    }
    // DO expression
    else if (auto do_exp = E.to<Hs::Do>())
    {
        auto DoExp = *do_exp;
        auto do_type = inferRho(DoExp);
        E = DoExp;
        return do_type;
    }
    // LISTCOMP
    else if (auto lcomp = E.to<Hs::ListComprehension>())
    {
        auto LComp = *lcomp;
        auto type = inferRho(LComp);
        E = LComp;
        return type;
    }
    // ENUM-FROM
    else if (auto l = E.to<Hs::ListFrom>())
    {
        auto L = *l;
        auto type = inferRho(L);
        E = L;
        return type;
    }
    // ENUM-FROM-THEN
    else if (auto l = E.to<Hs::ListFromThen>())
    {
        auto L = *l;
        auto type = inferRho(L);
        E = L;
        return type;
    }
    // ENUM-FROM-TO
    else if (auto l = E.to<Hs::ListFromTo>())
    {
        auto L = *l;
        auto type = inferRho(L);
        E = L;
        return type;
    }
    // ENUM-FROM-THEN-TO
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
        auto L = *l;
        auto type = inferRho(L);
        E = L;
        return type;
    }

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
