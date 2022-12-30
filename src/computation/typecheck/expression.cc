#include "typecheck.H"
#include "kindcheck.H"
#include "match.H" // for tcMatchesFun

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;


void TypeChecker::tcRho(Hs::Var& x, const Expected& exp_type)
{
    Type sigma;
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
        throw note_exception()<<"infer_type: can't find type of variable '"<<x.print()<<"'";

    x.wrap = instantiateSigma(OccurrenceOrigin(x.name), sigma, exp_type);
}

void TypeChecker::tcRho(Hs::Con& con, const Expected& exp_type)
{
    auto sigma = constructor_info(con).constructor_type();

    con.wrap = instantiateSigma(OccurrenceOrigin(con.name), sigma, exp_type);
}

void TypeChecker::tcRho(Hs::ApplyExp& App, const Expected& exp_type, int i)
{
    // i         = the number of arguments we pretend we've removed from the end of App.
    // arg_index = the index of the last argument that remains after these are removed.
    // 0         = the index of the first argument.

    int arg_index = int(App.args.size())-1-i;

    Expected fun_type = newInfer();
    if (arg_index > 0)
        tcRho(App, fun_type, i + 1);
    else
        tcRho(App.head, fun_type);

    auto e = note_exception()<<"Applying "<<(arg_index+1)<<" arguments to function "<<App.head.print()<<", but it only takes "<<arg_index<<"!";
    auto [arg_type, result_type] = unify_function(fun_type.read_type(), e);

    // Check the argument according to its required type
    auto wrap_arg = checkSigma(App.args[arg_index], arg_type);
    App.arg_wrappers.push_back(wrap_arg);

    context.push_note( Note() << "In expression '"<< App.print()<<"':" );

    // Convert the result to the expected time for the term
    auto wrap_res = instantiateSigma(AppOrigin(), result_type, exp_type);
    App.res_wrappers.push_back(wrap_res);

    context.pop_note();
}

void TypeChecker::tcRho(Hs::LetExp& Let, const Expected& exp_type)
{
    auto state2 = copy_clear_wanteds();

    state2.infer_type_for_binds(unloc(Let.binds));

    // 2. Compute type of let body
    state2.tcRho(unloc(Let.body), exp_type);

    current_wanteds() += state2.current_wanteds();
}

void TypeChecker::tcRho(Hs::LambdaExp& Lam, const Expected& exp_type)
{
    tcMatchesFun( getArity(Lam.match), exp_type, [&](const std::vector<Expected>& arg_types, const Expected& result_type){
        return [&](TypeChecker& tc) { tc.tcMatch(Hs::LambdaContext(), Lam.match, arg_types, result_type); }; }
        );
}

void TypeChecker::tcRho(Hs::TypedExp& TExp, const Expected& exp_type)
{
    auto type = check_type(desugar(TExp.type));
    Core::wrapper w1 = checkSigma( TExp.exp, type );
    Core::wrapper w2 = instantiateSigma(TypeConvertOrigin(), type, exp_type);
    TExp.wrap = w1 * w2;
}

void TypeChecker::tcRho(Hs::CaseExp& Case, const Expected& exp_type)
{
    // 2. Check the object
    auto object_type = inferRho(Case.object);

    // 3. Check the alternatives
    tcMatches(Hs::CaseContext(), Case.alts, {Check(object_type)}, exp_type);
}

void TypeChecker::tcRho(Hs::List& L, const Expected& exp_type)
{
    Type element_type = fresh_meta_type_var( kind_type() );
    set_expected_type( exp_type, list_type(element_type) );

    for(auto& element: L.elements)
        checkRho(element, element_type);
}

void TypeChecker::tcRho(Hs::Tuple& T, const Expected& exp_type)
{
    vector<Type> element_types;
    for(auto& element: T.elements)
    {
        auto element_type = inferRho(element);
        element_types.push_back( element_type );
    }

    set_expected_type( exp_type, tuple_type(element_types) );
}

void TypeChecker::tcRho(Hs::Literal& Lit, const Expected& exp_type)
{
    if (Lit.is_Char())
        set_expected_type( exp_type, char_type() );
    else if (Lit.is_String())
        set_expected_type( exp_type, list_type( char_type() ) );
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
        unify(arg_type, integer_type());

        Lit = Hs::Literal(Hs::Integer{*i, fromInteger});
    }
    else if (auto d = Lit.is_Double())
    {
        // 1. Typecheck fromRational
        expression_ref fromRational = Hs::Var({noloc,"Compiler.Fractional.fromRational"});
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

void TypeChecker::tcRho(Hs::IfExp& If, const Expected& exp_type)
{
    checkRho(unloc(If.condition), bool_type());

    auto result_type = expTypeToType(exp_type);
    checkRho(unloc(If.true_branch), result_type);
    checkRho(unloc(If.false_branch), result_type);
}


void TypeChecker::tcRho(Hs::LeftSection& LSec, const Expected& exp_type)
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

void TypeChecker::tcRho(Hs::RightSection& RSec, const Expected& exp_type)
{
    // 1. Typecheck the op
    auto op_type = inferRho(RSec.op);

    // 2. Check the op is a two-argument function
    auto [left_arg_type, tmp1] = unify_function(op_type);
    auto [right_arg_type, result_type] = unify_function(tmp1);

    // 3. Check the expected type
    Type section_type = function_type({left_arg_type}, result_type);
    set_expected_type( exp_type, section_type );

    // 4. Check the right arg type.
    checkRho(RSec.r_arg, right_arg_type);
}

void TypeChecker::tcRho(Hs::Do& DoExp, const Expected& exp_type)
{
    tcRhoStmts(0, DoExp.stmts.stmts, exp_type);
}

void TypeChecker::tcRho(Hs::ListComprehension& LComp, const Expected& exp_type)
{
    auto state2 = copy_clear_wanteds();
    state2.infer_quals_type(LComp.quals);
    auto body_type = state2.inferRho(LComp.body);

    current_wanteds() += state2.current_wanteds();

    set_expected_type( exp_type, list_type(body_type) );
}

void TypeChecker::tcRho(Hs::ListFrom& L, const Expected& exp_type)
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

void TypeChecker::tcRho(Hs::ListFromThen& L, const Expected& exp_type)
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

void TypeChecker::tcRho(Hs::ListFromTo& L, const Expected& exp_type)
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

void TypeChecker::tcRho(Hs::ListFromThenTo& L, const Expected& exp_type)
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

void TypeChecker::tcRho(expression_ref& E, const Expected& exp_type)
{
    // VAR
    if (auto v = E.to<Hs::Var>())
    {
        auto V = *v;
        tcRho(V, exp_type);
        E = V;
    }
    // CON
    else if (auto con = E.to<Hs::Con>())
    {
        auto C = *con;
        tcRho(C, exp_type);
        E = C;
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
        throw note_exception()<<"type check expression: I don't recognize expression '"<<E<<"'";
}

