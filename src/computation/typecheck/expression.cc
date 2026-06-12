#include "typecheck.H"
#include "kindcheck.H"
#include "match.H" // for tcMatchesFun
#include "haskell/ids.H"
#include "computation/record_utils.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

namespace
{
    using record_utils::record_field_positions;

    struct RecordFieldUpdate
    {
        std::vector<FieldInfo> candidates;
        Hs::LExp value;
    };

    Hs::LExp missing_record_field_exp(const std::optional<yy::location>& loc, const std::string& con_name, int field_index)
    {
        auto message = "Missing field " + std::to_string(field_index + 1) + " in record construction for constructor '" + get_unqualified_name(con_name) + "'";
        Hs::LExp error = {loc, Hs::Var("Compiler.Error.error")};
        Hs::LExp msg = {loc, Hs::Literal(Hs::String(message))};
        return Hs::apply(error, {msg});
    }

    Hs::LExp record_update_binder(const std::optional<yy::location>& loc, const std::string& con_name, int index)
    {
        auto name = "v$record_update$" + get_unqualified_name(con_name) + "$" + std::to_string(index);
        return {loc, Hs::Var(name)};
    }

    std::set<std::string> intersect_constructors(const std::set<std::string>& constructors1, const std::set<std::string>& constructors2)
    {
        std::set<std::string> constructors;
        for(const auto& con: constructors1)
            if (constructors2.count(con))
                constructors.insert(con);
        return constructors;
    }

    std::optional<std::set<std::string>> intersect_constructors(
        const std::optional<std::set<std::string>>& constructors1,
        const std::set<std::string>& constructors2)
    {
        if (constructors1)
            return intersect_constructors(*constructors1, constructors2);
        else
            return constructors2;
    }

    std::set<std::string> constructors_for_record_field_candidates(const std::vector<FieldInfo>& candidates)
    {
        std::set<std::string> constructors;
        for(const auto& field: candidates)
            constructors.insert(field.constructors.begin(), field.constructors.end());
        return constructors;
    }

    std::optional<int> record_field_position_for_constructor(const std::vector<FieldInfo>& candidates, const std::string& con_name)
    {
        for(const auto& field: candidates)
            for(int i=0; i<field.constructors.size(); i++)
                if (field.constructors[i] == con_name)
                    return field.positions[i];
        return {};
    }

    // Return constructors for the concrete record type when the scrutinee type is already known.
    std::optional<std::set<std::string>> constructors_for_record_type(TypeChecker& tc, const Type& type)
    {
        auto expanded_type = tc.expand_all_type_synonyms(follow_meta_type_var(type));
        auto tcapp = is_type_con_app(expanded_type);
        if (not tcapp)
            return {};

        auto& [tycon, _] = *tcapp;
        auto type_info = tc.this_mod().lookup_resolved_type(tycon.name);
        if (not type_info)
            return std::set<std::string>{};

        std::set<std::string> constructors;
        if (auto data = type_info->is_data())
            constructors.insert(data->constructors.begin(), data->constructors.end());
        else if (auto data_fam = type_info->is_data_fam())
            constructors.insert(data_fam->constructors.begin(), data_fam->constructors.end());
        else
            return std::set<std::string>{};

        for(auto iter = constructors.begin(); iter != constructors.end(); )
        {
            auto con_info = tc.this_mod().constructor_info(*iter);
            if (not con_info or not tc.maybe_unify(con_info->result_type(), expanded_type))
                iter = constructors.erase(iter);
            else
                ++iter;
        }

        return constructors;
    }

    // Validate record update fields and expand the update into constructor-specific alternatives.
    Hs::Matches record_update_alts(TypeChecker& tc, Hs::RecordUpdate& Rec, const Type& object_type, const std::optional<yy::location>& record_loc)
    {
        std::optional<std::set<std::string>> constructors;
        set<string> used_field_names;
        vector<RecordFieldUpdate> updates;

        for(auto& field: unloc(Rec.fbinds).fields)
        {
            auto& f = unloc(field);
            auto field_name = unloc(f.field).name;
            auto field_key = get_unqualified_name(field_name);
            if (used_field_names.count(field_key))
                tc.record_error(field.loc, Note()<<"Field '"<<field_name<<"' appears more than once in record update.");
            used_field_names.insert(field_key);

            std::vector<FieldInfo> field_candidates;
            bool lookup_failed = false;
            try
            {
                field_candidates = tc.this_mod().lookup_record_field_candidates(field_name);
            }
            catch (myexception& e)
            {
                lookup_failed = true;
                auto lookup_message = std::string(e.what());
                if (lookup_message.find("ambiguous") != std::string::npos)
                    tc.record_error(field.loc, Note()<<lookup_message);
                else
                    tc.record_error(field.loc, Note()<<"Record field '"<<field_name<<"' not in scope for update.");
            }

            auto field_constructors = constructors_for_record_field_candidates(field_candidates);
            if (field_constructors.empty() and not lookup_failed)
                tc.record_error(field.loc, Note()<<"Record field '"<<field_name<<"' not in scope for update.");
            constructors = intersect_constructors(constructors, field_constructors);

            if (not f.value)
                tc.record_error(field.loc, Note()<<"Field pun '"<<field_name<<"' was not expanded before typechecking.");
            else if (not field_candidates.empty())
                updates.push_back({field_candidates, *f.value});
        }

        if (auto type_constructors = constructors_for_record_type(tc, object_type))
            constructors = intersect_constructors(constructors, *type_constructors);

        vector<Located<Hs::Alt>> alts;
        if (constructors)
        {
            for(const auto& con_name: *constructors)
            {
                auto con_info = tc.this_mod().constructor_info(con_name);
                if (not con_info or not con_info->field_names)
                    continue;

                vector<Hs::LExp> args;
                Hs::LPats patterns;
                set<int> updated_positions;
                bool constructor_has_all_fields = true;

                for(int i=0; i<con_info->arity(); i++)
                {
                    auto var = record_update_binder(record_loc, con_info->name, i);
                    Hs::LVar lvar = {record_loc, unloc(var).as_<Hs::Var>()};
                    patterns.push_back({record_loc, Hs::VarPattern(lvar)});
                    args.push_back(var);
                }

                for(const auto& update: updates)
                {
                    auto pos = record_field_position_for_constructor(update.candidates, con_info->name);
                    if (not pos)
                    {
                        constructor_has_all_fields = false;
                        break;
                    }
                    args[*pos] = update.value;
                    updated_positions.insert(*pos);
                }

                if (unloc(Rec.fbinds).dotdot)
                {
                    for(int i=0; i<con_info->field_names->size(); i++)
                    {
                        const auto& field_name = (*con_info->field_names)[i];
                        if (not updated_positions.count(i))
                            args[i] = {record_loc, Hs::Var(get_unqualified_name(field_name))};
                    }
                }

                if (not constructor_has_all_fields)
                    continue;

                Hs::LCon head = {record_loc, Hs::Con(con_info->name, con_info->arity())};
                Hs::LPat pattern = {record_loc, Hs::ConPattern(head, patterns)};
                auto rhs = Hs::SimpleRHS(Hs::apply({record_loc, unloc(head)}, args));
                alts.push_back({record_loc, Hs::Alt(pattern, rhs)});
            }
        }

        if (alts.empty())
            tc.record_error(record_loc, Note()<<"No record constructor has all fields used in this update.");

        return Hs::CaseExp(Rec.object, Hs::Alts(alts)).alts;
    }

    // Validate record construction fields and place them in constructor order.
    Hs::LExp record_con_to_positional_app(TypeChecker& tc, Hs::RecordCon& Rec, const std::optional<yy::location>& record_loc)
    {
        auto con = unloc(Rec.con);
        auto con_info = tc.constructor_info(con);
        con.name = con_info.name;
        con.arity = con_info.arity();

        if (not con_info.field_names)
        {
            tc.record_error(Rec.con.loc, Note()<<"Constructor '"<<get_unqualified_name(con.name)<<"' is not a record constructor.");
            return {Rec.con.loc, con};
        }

        auto positions = record_field_positions(*con_info.field_names);
        vector<optional<Hs::LExp>> fields(con_info.field_names->size());
        set<int> used_fields;

        for(auto& field: unloc(Rec.fbinds).fields)
        {
            auto& f = unloc(field);
            auto field_name = unloc(f.field).name;
            auto pos = positions.find(field_name);
            if (pos == positions.end())
                pos = positions.find(get_unqualified_name(field_name));

            if (pos == positions.end())
                tc.record_error(field.loc, Note()<<"Constructor '"<<get_unqualified_name(con.name)<<"' does not have field '"<<field_name<<"'.");
            else if (used_fields.count(pos->second))
                tc.record_error(field.loc, Note()<<"Field '"<<field_name<<"' appears more than once in record construction.");
            else if (not f.value)
                tc.record_error(field.loc, Note()<<"Field pun '"<<field_name<<"' was not expanded before typechecking.");
            else
            {
                used_fields.insert(pos->second);
                fields[pos->second] = *f.value;
            }
        }

        vector<Hs::LExp> args;
        for(int i=0; i<fields.size(); i++)
        {
            if (fields[i])
                args.push_back(*fields[i]);
            else if (unloc(Rec.fbinds).dotdot)
                args.push_back({record_loc, Hs::Var(get_unqualified_name((*con_info.field_names)[i]))});
            else
            {
                tc.record_error(record_loc, Note()<<"Missing field "<<i+1<<" in record construction for constructor '"<<get_unqualified_name(con.name)<<"'.");
                args.push_back(missing_record_field_exp(record_loc, con.name, i));
            }
        }

        return Hs::apply({Rec.con.loc, con}, args);
    }
}

void TypeChecker::tcRho(Hs::Var& x, const Expected& exp_type)
{
    Type sigma;
    // First look for x in the local type environment
    if (auto it = mono_local_env().find(x))
    {
        auto& [v,type] = *it;
        // translate to the monomorpic id;
        x = v;
        sigma = type;
    }
    // x should be in the global type environment
    else if (auto sigma_ptr = poly_env().find( x ))
        sigma = *sigma_ptr;
    // x should be in the global type environment
    else if (auto S = this_mod().lookup_resolved_symbol(x.name))
        sigma = S->type;
    else
        throw note_exception()<<"infer_type: can't find type of variable '"<<x.print()<<"'";

    x.wrap = instantiateSigma(OccurrenceOrigin(x.name), sigma, exp_type);
}

void TypeChecker::tcRho(Hs::Con& con, const Expected& exp_type)
{
    auto sigma = constructor_info(con).constructor_type();

    con.wrap = instantiateSigma(OccurrenceOrigin(con.name), sigma, exp_type);
}

void TypeChecker::tcRho(Hs::ApplyExp& App, const Expected& exp_type)
{
    // 1. Infer the head type
    Expected fun_type = newInfer();
    tcRho(App.head, fun_type);

    // 2. Check the argument type
    auto result_type = [&]() {
        auto span = source_span_scope(App.arg.loc);
        auto [arg_type, result_type] = unify_function(fun_type.read_type(), AppOrigin{App});
        App.arg_wrapper = checkSigma(App.arg, arg_type);
        return result_type;
    }();

    // 3. Check the return type
    auto exp_loc = App.head.loc * App.arg.loc;

    {
        auto span = source_span_scope(exp_loc);
        auto note = note_scope( Note() << "In expression '"<< App.print()<<"':" );
        App.res_wrapper = instantiateSigma(AppOrigin(), result_type, exp_type);
    }
}

void TypeChecker::tcRho(Hs::LetExp& Let, const Expected& exp_type)
{
    auto state2 = copy_clear_wanteds();

    state2.infer_type_for_binds(Let.binds);

    // 2. Compute type of let body
    state2.tcRho(Let.body, exp_type);

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
    auto type = check_type(TExp.type);
    Core::wrapper w1 = checkSigma( TExp.exp, type );
    Core::wrapper w2 = instantiateSigma(TypeConvertOrigin(), type, exp_type);
    TExp.wrap = w1 * w2;
}

void TypeChecker::tcRho(Hs::CaseExp& Case, const Expected& exp_type)
{
    // 2. Check the object
    auto object_type = inferRho(Case.object);

    // 3. Check the alternatives
    tcCaseAlts(Case.alts, object_type, exp_type);
}

void TypeChecker::tcCaseAlts(Hs::Matches& alts, const Type& object_type, const Expected& exp_type)
{
    // The scrutinee has already been checked; this only checks alternatives against its type.
    tcMatches(Hs::CaseContext(), alts, {Check(object_type)}, exp_type);
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
    else if (auto s = Lit.is_String())
    {
        if (this_mod().language_extensions.has_extension(LangExt::OverloadedStrings))
        {
            // 1. Typecheck fromString
            auto fromString = Hs::Var("Data.String.fromString");
            auto fromString_type = inferRho(fromString);

            // 2. Check result type
            auto [arg_type, result_type] = unify_function(fromString_type);
            set_expected_type( exp_type, result_type );

            // 3. The argument type should be [Char]
            unify(arg_type, list_type(char_type()));

            Lit = Hs::Literal(Hs::String(*s, fromString));
        }
        else
            set_expected_type( exp_type, list_type( char_type() ) );
    }
    else if (Lit.is_BoxedInteger())
        set_expected_type( exp_type, int_type() );
    else if (auto i = Lit.is_Integer())
    {
        // 1. Typecheck fromInteger
        auto fromInteger = Hs::Var("Compiler.Num.fromInteger");
        auto fromInteger_type = inferRho(fromInteger);

        // 2. Check result type
        auto [arg_type, result_type] = unify_function(fromInteger_type);
        set_expected_type( exp_type, result_type );

        // 3. The argument type should be Integer
        unify(arg_type, integer_type());

        Lit = Hs::Literal(Hs::Integer{*i, fromInteger});
    }
    else if (auto r = Lit.is_Floating())
    {
        // 1. Typecheck fromRational
        auto fromRational = Hs::Var("Compiler.Fractional.fromRational");
        auto fromRational_type = inferRho(fromRational);

        // 2. Check result type
        auto [arg_type, result_type] = unify_function(fromRational_type);
        set_expected_type( exp_type, result_type );

        // 3. The argument type should be Rational
        unify(arg_type, rational_type());

        Lit = Hs::Literal(Hs::Floating{*r, fromRational});
    }
    else
        std::abort();
}

void TypeChecker::tcRho(Hs::IfExp& If, const Expected& exp_type)
{
    checkRho(If.condition, bool_type());

    auto result_type = expTypeToType(exp_type);
    checkRho(If.true_branch, result_type);
    checkRho(If.false_branch, result_type);
}


void TypeChecker::tcRho(Hs::LeftSection& LSec, const Expected& exp_type)
{
    // 1. Typecheck the op
    auto op_type = inferRho(LSec.op);

    // 2. Check that the op is a function
    auto [left_arg_type, result_type] = unify_function(op_type, LeftSectionOrigin{LSec.op});

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
    // FIXME! Rewrite "Do" to >>= and fail immediately before typechecking.
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

void TypeChecker::tcRho(Located<Hs::Expression>& E, const Expected& exp_type)
{
    auto span = source_span_scope(E.loc);
    tcRho_(unloc(E), exp_type);
}

void TypeChecker::tcRho_(Hs::Expression& E, const Expected& exp_type)
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
    // RECORD CONSTRUCTION
    else if (auto rec = E.to<Hs::RecordCon>())
    {
        auto Rec = *rec;
        auto app = record_con_to_positional_app(*this, Rec, Rec.con.loc * Rec.fbinds.loc);
        tcRho(app, exp_type);
        E = unloc(app);
    }
    // RECORD UPDATE
    else if (auto rec = E.to<Hs::RecordUpdate>())
    {
        auto Rec = *rec;
        auto object_type = inferRho(Rec.object);
        auto alts = record_update_alts(*this, Rec, object_type, Rec.object.loc * Rec.fbinds.loc);
        tcCaseAlts(alts, object_type, exp_type);
        Rec.checked_update = std::make_shared<Hs::CheckedRecordUpdate>(Rec.object, alts);
        E = Rec;
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
