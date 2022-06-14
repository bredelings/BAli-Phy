#include "typecheck.H"
#include "kindcheck.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

tuple<expression_ref, Hs::Type>
typechecker_state::infer_type(expression_ref E)
{
    if (auto x = E.to<Hs::Var>())
    {
        auto& x_name = unloc(x->name);
        if (auto it = mono_local_env.find(x_name))
        {
            auto& [v,type] = *it;
            return {v, type};
        }

        auto sigma = gve.find( x_name );

        // x should be in the type environment
        if (not sigma)
            throw myexception()<<"infer_type: can't find type of variable '"<<x->print()<<"'";

        auto [_, constraints, type] = instantiate(*sigma);

        for(auto& constraint: constraints)
        {
            auto dvar = add_dvar(constraint);
            E = {E, dvar};
        }

        return {E, type};
    }
    else if (auto L = E.to<Hs::Literal>())
    {
        if (auto c = L->is_Char())
        {
            return { E, char_type() };
        }
        else if (auto i = L->is_Integer())
        {
            // 1. Typecheck fromInteger
            auto [fromInteger, fromInteger_type] = infer_type(Hs::Var({noloc,"Compiler.Num.fromInteger"}));

            // 2. Determine result type
            auto result_type = fresh_meta_type_var( kind_star() );
            unify(fromInteger_type, Hs::make_arrow_type(int_type(), result_type));

            return { Hs::Literal(Hs::Integer{*i, fromInteger}), result_type };
        }
        else if (auto s = L->is_String())
        {
            return { E, Hs::ListType( char_type() ) };
        }
        else if (auto d = L->is_Double())
        {
            // 1. Typecheck fromRational
            auto [fromRational, fromRational_type] = infer_type(Hs::Var({noloc,"Compiler.Num.fromRational"}));

            // 2. Determine result type
            auto result_type = fresh_meta_type_var( kind_star() );
            unify(fromRational_type, Hs::make_arrow_type(double_type(), result_type));

            return { Hs::Literal(Hs::Double{*d, fromRational}), result_type };
        }
        else if (auto i = L->is_BoxedInteger())
        {
            return { E, int_type() };
        }
    }
    else if (auto texp = E.to<Hs::TypedExp>())
    {
        // So, ( e :: tau ) should be equivalent to ( let x :: tau ; x = e in x )
        // according to the 2010 report.

        // Example: (\x -> x) :: Num a => a -> a
        // In this example, we should rewrite this to \dNum -> \x -> x

        // texp->exp;
        // texp->type

        // FIXME: For better error messages, we should inline the code for inferring types of LetExp.
        //        We will know we will call infer_type_for_single_fundecl_with_sig

        auto x = get_fresh_Var("tmp", false);
        Hs::Decls decls;
        decls.push_back(simple_decl(x,texp->exp));
        Hs::Binds binds;
        // By making a LetExp, we rely on the Let code to handle the type here.
        binds.signatures.insert({unloc(x.name), texp->type});
        binds.push_back(decls);
        expression_ref E2 = Hs::LetExp({noloc,binds},{noloc,x});

        return infer_type(E2);
    }
    else if (auto l = E.to<Hs::List>())
    {
        Hs::Type element_type = fresh_meta_type_var( kind_star() );
        auto L = *l;
        for(auto& element: L.elements)
        {
            auto [element1, t1] = infer_type(element);
            element = element1;
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
        return { L, Hs::ListType(element_type) };
    }
    else if (auto tup = E.to<Hs::Tuple>())
    {
        auto T = *tup;

        vector<Hs::Type> element_types;
        for(auto& element: T.elements)
        {
            auto [element1, element_type] = infer_type(element);
            element = element1;
            element_types.push_back( element_type );
        }
        Hs::Type result_type = Hs::TupleType(element_types);
        return {T, result_type};
    }
    // COMB
    else if (is_apply_exp(E))
    {
        assert(E.size() >= 2);

        auto e1 = E.sub()[0];

        auto [f, t1] = infer_type(e1);

        vector<expression_ref> args;
        for(int i=1;i<E.size();i++)
        {
            auto arg_result_types = unify_function(t1);
            if (not arg_result_types)
                throw myexception()<<"Applying "<<E.size()<<" arguments to function "<<e1.print()<<", but it only takes "<<i-1<<"!";

            auto [expected_arg_type, result_type] = *arg_result_types;

            auto e2 = E.sub()[i];
            auto [arg, arg_type] = infer_type(e2);
            args.push_back(arg);

            try {
                unify (arg_type, expected_arg_type);
            }
            catch (myexception& ex)
            {
                std::ostringstream header;
                header<<"Argument "<<i<<" of function "<<e1<<" expected type\n\n";
                header<<"   "<<apply_current_subst(expected_arg_type)<<"\n\n";
                header<<"but got type\n\n";
                header<<"   "<<apply_current_subst(arg_type)<<"\n\n";
                header<<"with argument\n\n";
                header<<"   "<<e2<<"\n\n";

                ex.prepend(header.str());
                throw;
            }

            t1 = result_type;
        }
        E = apply_expression(f, args);

        return {E, t1};
    }
    // LAMBDA
    else if (auto lam = E.to<Hs::LambdaExp>())
    {
        auto Lam = *lam;
        auto rule = Hs::MRule{Lam.args, Lam.body};
        auto t = infer_type(rule);
        Lam.args = rule.patterns;
        Lam.body = rule.rhs;
        return {Lam, t};
    }
    // LET
    else if (auto let = E.to<Hs::LetExp>())
    {
        auto Let = *let;

        // 1. Extend environment with types for decls, get any substitutions
        auto state2 = copy_clear_lie();
        unloc(Let.binds) = state2.infer_type_for_binds(unloc(Let.binds));

        // 2. Compute type of let body
        auto [body, t_body] = state2.infer_type(unloc(Let.body));
        unloc(Let.body) = body;

        current_lie() += state2.current_lie();

        return {Let, t_body};
    }
    else if (auto con = E.to<Hs::Con>())
    {
        auto [tvs, constraints, result_type] = instantiate( constructor_type(*con) );
        return { E, result_type };
    }
    else if (auto con = E.head().to<Hs::Con>())
    {
        // See note in rename/expressio.cc about rewriting (@ con args) to (con args)

        vector<Hs::Exp> args = E.copy_sub();

        auto [type, field_types] = constructor_pattern_types(*con);
        if (args.size() < field_types.size())
            return infer_type(apply_expression(*con, args));

        vector<Hs::Type> arg_types;
        for(int i=0; i < args.size(); i++)
        {
            auto& arg = args[i];
            auto [arg_i, t_i] = infer_type(arg);
            arg = arg_i;
            arg_types.push_back(t_i);

            // REQUIRE that i-th argument matches the type for the i-th field.
            try{
                unify( field_types[i], t_i);
            }
            catch (myexception& e)
            {
                std::ostringstream header;
                header<<"Argument "<<i+1<<" // "<<arg<<" of constructor "<<con->print()<<" has type "<<apply_current_subst(t_i)<<" but expected type "<<apply_current_subst(field_types[i])<<"\n ";
                e.prepend(header.str());
                throw;
            }
        }
        E = expression_ref(*con, args);
        
        return { E, type };
    }
    else if (is_non_apply_op_exp(E))
    {
        std::abort();
        // this includes builtins like Prelude::add
    }
    // CASE
    else if (auto case_exp = E.to<Hs::CaseExp>())
    {
        auto Case = *case_exp;

        // 1. Determine object type
        auto [object, object_type] = infer_type(Case.object);
        Case.object = object;
        
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

        return { Case, result_type };
    }
    // IF
    else if (auto if_exp = E.to<Hs::IfExp>())
    {
        auto If = *if_exp;
        auto [cond, cond_type ] = infer_type(unloc(If.condition));
        auto [tbranch, tbranch_type] = infer_type(unloc(If.true_branch));
        auto [fbranch, fbranch_type] = infer_type(unloc(If.false_branch));
        unloc(If.condition) = If;
        unloc(If.true_branch) = tbranch;
        unloc(If.false_branch) = fbranch;

        unify(cond_type, bool_type());
        unify(tbranch_type, fbranch_type);

        return {If, tbranch_type};
    }
    // LEFT section
    else if (auto lsec = E.to<Hs::LeftSection>())
    {
        auto LSec = *lsec;

        // 1. Typecheck the op
        auto [op, op_type] = infer_type(LSec.op);
        LSec.op = op;

        // 2. Typecheck the left argument
        auto [left_arg, left_arg_type] = infer_type(LSec.l_arg);
        LSec.l_arg = left_arg;

        // 3. Typecheck the function application
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(op_type, Hs::make_arrow_type(left_arg_type, result_type));

        return {LSec, result_type};
    }
    // Right section
    else if (auto rsec = E.to<Hs::RightSection>())
    {
        auto RSec = *rsec;

        // 1. Typecheck the op
        auto [op, op_type] = infer_type(RSec.op);
        RSec.op = op;

        // 2. Typecheck the right argument
        auto [right_arg, right_arg_type] = infer_type(RSec.r_arg);
        RSec.r_arg = right_arg;

        // 3. Typecheck the function application:  op left_arg right_arg
        auto left_arg_type = fresh_meta_type_var( kind_star() );
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(op_type, Hs::function_type({left_arg_type, right_arg_type}, result_type));

        // 4. Compute the section type;
        Hs::Type section_type = Hs::function_type({left_arg_type}, result_type);

        return {RSec, section_type};
    }
    // DO expression
    else if (auto do_exp = E.to<Hs::Do>())
    {
        auto DoExp = *do_exp;
        auto do_type = infer_stmts_type(0, DoExp.stmts.stmts);
        return {DoExp, do_type};
    }

    // LISTCOMP
    else if (auto lcomp = E.to<Hs::ListComprehension>())
    {
        auto LComp = *lcomp;
        auto state2 = copy_clear_lie();
        LComp.quals = state2.infer_quals_type(LComp.quals);
        auto [body, exp_type] = state2.infer_type(LComp.body);
        LComp.body = body;

        Hs::Type result_type = Hs::ListType(exp_type);

        current_lie() += state2.current_lie();

        return { LComp, result_type };
    }
    // ENUM-FROM
    else if (auto l = E.to<Hs::ListFrom>())
    {
        auto L = *l;

        // 1. Typecheck enumFrom
        auto [enumFrom, enumFrom_type] = infer_type(Hs::Var({noloc,"Compiler.Enum.enumFrom"}));
        L.enumFromOp = enumFrom;

        // 2. Typecheck from argument
        auto [from, from_type] = infer_type(L.from);
        L.from = from;

        // 3. enumFrom_type ~ from_type -> result_type
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(enumFrom_type, Hs::make_arrow_type(from_type, result_type));

        return { L, result_type };
    }
    // ENUM-FROM-THEN
    else if (auto l = E.to<Hs::ListFromThen>())
    {
        auto L = *l;

        // 1. Typecheck enumFrom
        auto [enumFromThen, enumFromThen_type] = infer_type(Hs::Var({noloc,"Compiler.Enum.enumFromThen"}));
        L.enumFromThenOp = enumFromThen;

        // 2. Typecheck from argument
        auto [from, from_type] = infer_type(L.from);
        L.from = from;

        // 3. enumFromThen_type ~ from_type -> a
        auto a = fresh_meta_type_var( kind_star() );
        unify(enumFromThen_type, Hs::make_arrow_type(from_type, a));

        // 4. Typecheck then argument
        auto [then, then_type] = infer_type(L.then);
        L.then = then;

        // 5. a ~ then_type -> result_type
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(a, Hs::make_arrow_type(then_type, result_type));
        
        return { L, result_type };
    }
    // ENUM-FROM-TO
    else if (auto l = E.to<Hs::ListFromTo>())
    {
        auto L = *l;

        // 1. Typecheck enumFrom
        auto [enumFromTo, enumFromTo_type] = infer_type(Hs::Var({noloc,"Compiler.Enum.enumFromTo"}));
        L.enumFromToOp = enumFromTo;

        // 2. Typecheck from argument
        auto [from, from_type] = infer_type(L.from);
        L.from = from;

        // 3. enumFromTo_type ~ from_type -> a
        auto a = fresh_meta_type_var( kind_star() );
        unify(enumFromTo_type, Hs::make_arrow_type(from_type, a));

        // 4. Typecheck to argument
        auto [to, to_type] = infer_type(L.to);
        L.to = to;

        // 5. a ~ to_type -> result_type
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(a, Hs::make_arrow_type(to_type, result_type));
        
        return { L, result_type };
    }
    // ENUM-FROM-THEN-TO
    else if (auto l = E.to<Hs::ListFromThenTo>())
    {
        auto L = *l;

        // 1. Typecheck enumFromThenTo
        auto [enumFromThenTo, enumFromThenTo_type] = infer_type(Hs::Var({noloc,"Compiler.Enum.enumFromThenTo"}));
        L.enumFromThenToOp = enumFromThenTo;

        // 2. Typecheck from argument
        auto [from, from_type] = infer_type(L.from);
        L.from = from;

        // 3. enumFromThenTo_type ~ from_type -> a
        auto a = fresh_meta_type_var( kind_star() );
        unify(enumFromThenTo_type, Hs::make_arrow_type(from_type, a));

        // 4. Typecheck then argument
        auto [then, then_type] = infer_type(L.then);
        L.then = then;

        // 5. a ~ then_type -> b
        auto b = fresh_meta_type_var( kind_star() );
        unify(a, Hs::make_arrow_type(then_type, b));

        // 6. Typecheck to argument
        auto [to, to_type] = infer_type(L.to);
        L.to = to;

        // 7. b ~ to_type -> result_type
        auto result_type = fresh_meta_type_var( kind_star() );
        unify(b, Hs::make_arrow_type(to_type, result_type));

        return { L, result_type };
    }

    throw myexception()<<"type check expression: I don't recognize expression '"<<E<<"'";
}

