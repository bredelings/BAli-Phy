#include "typecheck.H"
#include "kindcheck.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

std::pair<Hs::Expression,Hs::Type> typechecker_state::infer_type(const Hs::Var& x)
{
    auto& x_name = unloc(x.name);
    if (auto it = mono_local_env.find(x_name))
    {
        auto& [v,type] = *it;
        return {v,type};
    }

    auto sigma = gve.find( x_name );

    // x should be in the type environment
    if (not sigma)
        throw myexception()<<"infer_type: can't find type of variable '"<<x.print()<<"'";

    auto [_, constraints, type] = instantiate(*sigma);

    expression_ref E = x;
    for(auto& constraint: constraints)
    {
        auto dvar = add_dvar(constraint);
        E = {E, dvar};
    }

    return {E,type};
}

Hs::Type typechecker_state::infer_type_apply(expression_ref& E)
{
    assert(E.size() >= 2);

    expression_ref f = E.sub()[0];
    auto t1 = infer_type(f);

    vector<expression_ref> args;
    for(int i=1;i<E.size();i++)
    {
        auto arg_result_types = unify_function(t1);
        if (not arg_result_types)
            throw myexception()<<"Applying "<<E.size()<<" arguments to function "<<f.print()<<", but it only takes "<<i-1<<"!";

        auto [expected_arg_type, result_type] = *arg_result_types;

        expression_ref arg = E.sub()[i];
        auto arg_type = infer_type(arg);
        args.push_back(arg);

        try {
            unify (arg_type, expected_arg_type);
        }
        catch (myexception& ex)
        {
            std::ostringstream header;
            header<<"Argument "<<i<<" of function "<<f<<" expected type\n\n";
            header<<"   "<<apply_current_subst(expected_arg_type)<<"\n\n";
            header<<"but got type\n\n";
            header<<"   "<<apply_current_subst(arg_type)<<"\n\n";
            header<<"with argument\n\n";
            header<<"   "<<E.sub()[i]<<"\n\n";

            ex.prepend(header.str());
            throw;
        }

        t1 = result_type;
    }
    E = apply_expression(f, args);

    return t1;
}

Hs::Type typechecker_state::infer_type(Hs::LetExp& Let)
{
    auto state2 = copy_clear_lie();

    state2.infer_type_for_binds(unloc(Let.binds));

    // 2. Compute type of let body
    auto t_body = state2.infer_type(unloc(Let.body));

    current_lie() += state2.current_lie();

    return t_body;
}

Hs::Type typechecker_state::infer_type(Hs::LambdaExp& Lam)
{
    auto rule = Hs::MRule{Lam.args, Lam.body};
    auto t = infer_type(rule);
    Lam.args = rule.patterns;
    Lam.body = rule.rhs;
    return t;
}

std::pair<Hs::Expression,Hs::Type> typechecker_state::infer_type(const Hs::TypedExp& TExp)
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

    Hs::Type t = infer_type(E2);
    return {E2, t};
}

Hs::Type typechecker_state::infer_type(Hs::CaseExp& Case)
{
    // 1. Determine object type
    auto object_type = infer_type(Case.object);

    // 2. Determine data type for object from patterns.
    Hs::Match match;
    for(auto& alt: Case.alts)
    {
        auto& [pattern, body] = unloc(alt);
        match.rules.push_back(Hs::MRule{{pattern},body});
    }

    auto match_type = infer_type(match);

    for(int i=0;i<Case.alts.size();i++)
    {
        unloc(Case.alts[i]) = {match.rules[i].patterns[0], match.rules[i].rhs};
    }

    Hs::Type result_type = fresh_meta_type_var( kind_star() );

    unify( Hs::make_arrow_type(object_type,result_type), match_type );

    return result_type;
}

Hs::Type typechecker_state::infer_type(Hs::List& L)
{
    Hs::Type element_type = fresh_meta_type_var( kind_star() );

    for(auto& element: L.elements)
    {
        auto t1 = infer_type(element);

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

Hs::Type typechecker_state::infer_type(Hs::Tuple& T)
{
    vector<Hs::Type> element_types;
    for(auto& element: T.elements)
    {
        auto element_type = infer_type(element);
        element_types.push_back( element_type );
    }
    Hs::Type result_type = Hs::TupleType(element_types);
    return result_type;
}

Hs::Type typechecker_state::infer_type(Hs::Literal& Lit)
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
        auto fromInteger_type = infer_type(fromInteger);

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
        auto fromRational_type = infer_type(fromRational);

        // 2. Determine result type
        Hs::Type result_type = fresh_meta_type_var( kind_star() );
        unify(fromRational_type, Hs::make_arrow_type(double_type(), result_type));

        Lit = Hs::Literal(Hs::Double{*d, fromRational});
        return result_type;
    }
    else
        std::abort();
}

Hs::Type
typechecker_state::infer_type(expression_ref& E)
{
    // VAR
    if (auto x = E.to<Hs::Var>())
    {
        auto [E2, type] = infer_type(*x);
        E = E2;
        return type;
    }
    // APP
    else if (is_apply_exp(E))
    {
        assert(E.size() >= 2);
        auto t = infer_type_apply(E);
        return t;
    }
    // LAMBDA
    else if (auto lam = E.to<Hs::LambdaExp>())
    {
        auto Lam = *lam;
        auto t = infer_type(Lam);
        E = Lam;
        return t;
    }
    // LET
    else if (auto let = E.to<Hs::LetExp>())
    {
        auto Let = *let;
        auto t = infer_type(Let);
        E = Let;
        return t;
    }
    // CASE
    else if (auto case_exp = E.to<Hs::CaseExp>())
    {
        auto Case = *case_exp;
        auto type = infer_type(Case);
        E = Case;
        return type;
    }
    // EXP :: sigma
    else if (auto texp = E.to<Hs::TypedExp>())
    {
        auto TExp = *texp;
        auto [E2,type] = infer_type(TExp);
        E = E2;
        return type;
    }
    // LITERAL
    else if (auto L = E.to<Hs::Literal>())
    {
        auto Lit = *L;
        auto type = infer_type(Lit);
        E = Lit;
        return type;
    }
    // LIST
    else if (auto l = E.to<Hs::List>())
    {
        auto L = *l;
        auto type = infer_type(L);
        E = L;
        return type;
    }
    // TUPLE
    else if (auto tup = E.to<Hs::Tuple>())
    {
        auto T = *tup;
        auto type = infer_type(T);
        E = T;
        return type;
    }
    else if (auto con = E.to<Hs::Con>())
    {
        auto [tvs, constraints, result_type] = instantiate( constructor_type(*con) );
        return result_type;
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
        auto cond_type = infer_type(unloc(If.condition));
        auto tbranch_type = infer_type(unloc(If.true_branch));
        auto fbranch_type = infer_type(unloc(If.false_branch));

        unify(cond_type, bool_type());
        unify(tbranch_type, fbranch_type);

        E = If;
        return tbranch_type;
    }
    // LEFT section
    else if (auto lsec = E.to<Hs::LeftSection>())
    {
        auto LSec = *lsec;

        // 1. Typecheck the op
        auto op_type = infer_type(LSec.op);

        // 2. Typecheck the left argument
        auto left_arg_type = infer_type(LSec.l_arg);

        // 3. Typecheck the function application
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(op_type, Hs::make_arrow_type(left_arg_type, result_type));

        E = LSec;
        return result_type;
    }
    // Right section
    else if (auto rsec = E.to<Hs::RightSection>())
    {
        auto RSec = *rsec;

        // 1. Typecheck the op
        auto op_type = infer_type(RSec.op);

        // 2. Typecheck the right argument
        auto right_arg_type = infer_type(RSec.r_arg);

        // 3. Typecheck the function application:  op left_arg right_arg
        auto left_arg_type = fresh_meta_type_var( kind_star() );
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(op_type, Hs::function_type({left_arg_type, right_arg_type}, result_type));

        // 4. Compute the section type;
        Hs::Type section_type = Hs::function_type({left_arg_type}, result_type);

        E = RSec;
        return section_type;
    }
    // DO expression
    else if (auto do_exp = E.to<Hs::Do>())
    {
        auto DoExp = *do_exp;
        auto do_type = infer_stmts_type(0, DoExp.stmts.stmts);
        E = DoExp;
        return do_type;
    }

    // LISTCOMP
    else if (auto lcomp = E.to<Hs::ListComprehension>())
    {
        auto LComp = *lcomp;
        auto state2 = copy_clear_lie();
        state2.infer_quals_type(LComp.quals);
        auto exp_type = state2.infer_type(LComp.body);

        Hs::Type result_type = Hs::ListType(exp_type);

        current_lie() += state2.current_lie();

        E = LComp;
        return result_type;
    }
    // ENUM-FROM
    else if (auto l = E.to<Hs::ListFrom>())
    {
        auto L = *l;

        // 1. Typecheck enumFrom
        L.enumFromOp = Hs::Var({noloc,"Compiler.Enum.enumFrom"});
        auto enumFrom_type = infer_type(L.enumFromOp);

        // 2. Typecheck from argument
        auto from_type = infer_type(L.from);

        // 3. enumFrom_type ~ from_type -> result_type
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(enumFrom_type, Hs::make_arrow_type(from_type, result_type));

        E = L;
        return result_type;
    }
    // ENUM-FROM-THEN
    else if (auto l = E.to<Hs::ListFromThen>())
    {
        auto L = *l;

        // 1. Typecheck enumFrom
        L.enumFromThenOp = Hs::Var({noloc,"Compiler.Enum.enumFromThen"});
        auto enumFromThen_type = infer_type(L.enumFromThenOp);

        // 2. Typecheck from argument
        auto from_type = infer_type(L.from);

        // 3. enumFromThen_type ~ from_type -> a
        auto a = fresh_meta_type_var( kind_star() );
        unify(enumFromThen_type, Hs::make_arrow_type(from_type, a));

        // 4. Typecheck then argument
        auto then_type = infer_type(L.then);

        // 5. a ~ then_type -> result_type
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(a, Hs::make_arrow_type(then_type, result_type));
        
        E = L;
        return result_type;
    }
    // ENUM-FROM-TO
    else if (auto l = E.to<Hs::ListFromTo>())
    {
        auto L = *l;

        // 1. Typecheck enumFrom
        L.enumFromToOp = Hs::Var({noloc,"Compiler.Enum.enumFromTo"});
        auto enumFromTo_type = infer_type(L.enumFromToOp);

        // 2. Typecheck from argument
        auto from_type = infer_type(L.from);

        // 3. enumFromTo_type ~ from_type -> a
        auto a = fresh_meta_type_var( kind_star() );
        unify(enumFromTo_type, Hs::make_arrow_type(from_type, a));

        // 4. Typecheck to argument
        auto to_type = infer_type(L.to);

        // 5. a ~ to_type -> result_type
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(a, Hs::make_arrow_type(to_type, result_type));
        
        E = L;
        return result_type;
    }
    // ENUM-FROM-THEN-TO
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
        auto L = *l;

        // 1. Typecheck enumFromThenTo
        L.enumFromThenToOp = Hs::Var({noloc,"Compiler.Enum.enumFromThenTo"});
        auto enumFromThenTo_type = infer_type(L.enumFromThenToOp);

        // 2. Typecheck from argument
        auto from_type = infer_type(L.from);

        // 3. enumFromThenTo_type ~ from_type -> a
        auto a = fresh_meta_type_var( kind_star() );
        unify(enumFromThenTo_type, Hs::make_arrow_type(from_type, a));

        // 4. Typecheck then argument
        auto then_type = infer_type(L.then);

        // 5. a ~ then_type -> b
        auto b = fresh_meta_type_var( kind_star() );
        unify(a, Hs::make_arrow_type(then_type, b));

        // 6. Typecheck to argument
        auto to_type = infer_type(L.to);

        // 7. b ~ to_type -> result_type
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(b, Hs::make_arrow_type(to_type, result_type));

        E = L;
        return result_type;
    }

    throw myexception()<<"type check expression: I don't recognize expression '"<<E<<"'";
}

