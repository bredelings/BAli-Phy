#include "code-generation.H"

#include "rules.H"                         // for Rules
#include "util/set.H"                      // for add, plus, minus
#include "util/log-level.H"                // for log_verbose
#include "util/graph.H"                    // for make_graph( )
#include "computation/haskell/haskell.H"   // for Hs::LExp
#include "computation/haskell/generated.H" // for Haskell::Generated builders
#include "computation/haskell/ids.H"       // for haskell_qid
#include "util/string/join.H"              // for join( )
#include "range/v3/all.hpp"

namespace views = ranges::views;
namespace HsG = Haskell::Generated;

using std::string;
using std::vector;
using std::optional;
using std::set;
using std::multiset;
using std::map;

Hs::Binds make_generated_binds(const std::vector<std::pair<Hs::Var, expression_ref>>& decls);

bool is_generated_var_expr(const expression_ref& E)
{
    return E.is_a<Hs::Var>();
}

Hs::Var as_generated_var(const expression_ref& E)
{
    if (E.is_a<Hs::Var>())
        return E.as_<Hs::Var>();
    else
        throw myexception()<<"Expected generated variable, but got "<<E;
}

bool is_loggable_function(const Rules& R, const string& name)
{
    auto rule = R.get_rule_for_func(name);

    // A random "let" has NOT necessarily been logged.
    // * let[k=random,k+2]         # OK    : we assume that if k is random, the whole thing is random.
    // * let[k=random,hky85[k]]    # Not OK: we assume that if k is random, the whole thing is random.
    // Probably we should track whether the result is an unlogged random in translation_result_t.
    if (name == "!let") return false;

    if (name == "function") return true;

    if (not rule) return false;
    return not rule->no_log;
}

void perform_action_simplified_(generated_code_t& block, const Hs::Var& x, bool is_referenced, const generated_code_t& code, bool depends_on_lambda)
{
    if (code.perform_function)
    {
	assert(not depends_on_lambda);
        HsG::Bind(block.stmts, HsG::VarPat(x), code.E);
    }
    else if (is_referenced or code.is_action())
    {
	if (not depends_on_lambda)
	    HsG::Let(block.stmts, x, code.E);
	else
	    block.decls.push_back({x, code.E});
    }
}

void use_block(translation_result_t& block, const Hs::Var& log_x, const translation_result_t& code, const string& name)
{
    add(block.imports, code.imports);
    add(block.lambda_vars, code.lambda_vars);
    add(block.haskell_vars, code.haskell_vars);
    add(block.code.used_states, code.code.used_states);
    add(block.code.free_vars, code.code.free_vars);

    for(auto& stmt: code.code.stmts.stmts)
        block.code.stmts.stmts.push_back(stmt);

    for(auto& decl: code.code.decls)
        block.code.decls.push_back(decl);

    if (code.code.has_loggers())
        block.code.log_sub(name, log_x, code.code.loggers);
}

void perform_action_simplified(translation_result_t& block, const Hs::Var& x, const Hs::Var& log_x, bool is_referenced, const translation_result_t& code, const string& name)
{
    use_block(block, log_x, code, name);
    perform_action_simplified_(block.code, x, is_referenced, code.code, not code.lambda_vars.empty());
}

bool is_loggable_type(const type_t& type)
{
    auto [head,args] = get_type_apps(type);

    if (head == "String") return true;

    else if (head == "Int") return true;

    else if (head == "Double") return true;

    else if (head == "DiscreteDist")
    {
        if (args.size() != 1) return false;

        return is_loggable_type(args[0]);
    }

    else if (head == "List")
    {
        if (args.size() != 1) return false;

        return is_loggable_type(args[0]);
    }

    else if (head == "Tuple")
    {
        for(auto& arg: args)
            if (not is_loggable_type(arg)) return false;
        return true;
    }

    return false;
}

// Rewrites generated `intToDouble n` applications back to the integer literal
// so Haskell can resolve the numeric type from context.
expression_ref simplify_intToDouble(const expression_ref& E)
{
    auto is_int_to_double = [](const expression_ref& head)
    {
        if (head.is_a<Hs::Var>())
            return head.as_<Hs::Var>().name == "intToDouble";
        else
            return false;
    };

    if (E.is_a<Hs::ApplyExp>())
    {
        auto [head,args] = Hs::decompose_apps({noloc, E});
        if (args.size() == 1 and is_int_to_double(unloc(head)))
        {
            auto arg = unloc(args[0]);
            if (auto L = arg.to<Hs::Literal>(); L and L->is_Integer())
                return arg;
        }
    }

    return E;
}

// Extracts a Haskell-friendly function name from a typed model expression for
// submodel argument variable naming.
optional<string> get_func_name(const CM::TypedExpr& model)
{
    optional<string> func_name;
    if (auto var = model.to<CM::Var>())
        func_name = var->name;
    else if (auto call = model.to<CM::Call<CM::Ann>>())
        func_name = call->function;
    else if (auto lambda = model.to<CM::Lambda<CM::Ann>>())
        return get_func_name(lambda->body);
    else
        return {};

    if (is_qualified_symbol(*func_name))
        func_name = get_unqualified_name(*func_name);

    if (is_haskell_id(*func_name))
        return func_name;
    else
        return {};
}

expression_ref generated_code_t::add_arguments(const expression_ref& F, const std::map<std::string,expression_ref>& state_values) const
{
    vector<expression_ref> args;
    for(auto& state: used_states)
	args.push_back(state_values.at(state));
    for(auto& [_,x]: free_vars)
	args.push_back(x);
    return HsG::Apply(F, args);
}

// Detects whether a typed model expression contains a random expression,
// including let-declared random variables visible in the current scope.
bool CodeGenState::is_random(const CM::TypedExpr& model) const
{
    return model.visit(CM::overloaded{
        [](const CM::IntLiteral&) { return false; },
        [](const CM::DoubleLiteral&) { return false; },
        [](const CM::BoolLiteral&) { return false; },
        [](const CM::StringLiteral&) { return false; },
        [&](const CM::Var& var)
        {
            return identifiers.count(var.name) and identifiers.at(var.name).is_random;
        },
        [](const CM::ArgRef&) { return false; },
        [](const CM::Placeholder&) { return false; },
        [&](const CM::GetState&) { return false; },
        [&](const CM::Call<CM::Ann>& call)
        {
            if (call.function == "sample") return true;
            for(auto& arg: call.args)
                if (arg.value and is_random(*arg.value)) return true;
            return false;
        },
        [&](const CM::List<CM::Ann>& list)
        {
            for(auto& element: list.elements)
                if (is_random(element)) return true;
            return false;
        },
        [&](const CM::Tuple<CM::Ann>& tuple)
        {
            for(auto& element: tuple.elements)
                if (is_random(element)) return true;
            return false;
        },
        [&](const CM::Let<CM::Ann>& let)
        {
            for(auto& [_, expr]: let.decls)
                if (is_random(expr)) return true;
            return false;
        },
        [&](const CM::Lambda<CM::Ann>& lambda)
        {
            return is_random(lambda.body);
        },
        [](const CM::Sample<CM::Ann>&) { return true; }
    });
}

// Detects random expressions that still need parent logging; loggable function
// calls are treated as having already logged their random children.
bool CodeGenState::is_unlogged_random(const CM::TypedExpr& model) const
{
    return model.visit(CM::overloaded{
        [](const CM::IntLiteral&) { return false; },
        [](const CM::DoubleLiteral&) { return false; },
        [](const CM::BoolLiteral&) { return false; },
        [](const CM::StringLiteral&) { return false; },
        [](const CM::Var&) { return false; },
        [](const CM::ArgRef&) { return false; },
        [](const CM::Placeholder&) { return false; },
        [](const CM::GetState&) { return false; },
        [&](const CM::Call<CM::Ann>& call)
        {
            if (call.function == "sample") return true;
            if (is_loggable_function(*R, call.function)) return false;
            for(auto& arg: call.args)
                if (arg.value and is_unlogged_random(*arg.value)) return true;
            return false;
        },
        [&](const CM::List<CM::Ann>& list)
        {
            for(auto& element: list.elements)
                if (is_unlogged_random(element)) return true;
            return false;
        },
        [&](const CM::Tuple<CM::Ann>& tuple)
        {
            for(auto& element: tuple.elements)
                if (is_unlogged_random(element)) return true;
            return false;
        },
        [&](const CM::Let<CM::Ann>& let)
        {
            for(auto& [_, expr]: let.decls)
                if (is_unlogged_random(expr)) return true;
            if (is_unlogged_random(let.body)) return true;
            return false;
        },
        [](const CM::Lambda<CM::Ann>&) { return false; },
        [](const CM::Sample<CM::Ann>&) { return true; }
    });
}

CodeGenState CodeGenState::extend_scope(const string& var, const var_info_t& var_info) const
{
    auto scope = *this;
    return scope.extend_modify_scope(var, var_info);
}

CodeGenState& CodeGenState::extend_modify_scope(const string& var, const var_info_t& var_info)
{
    if (identifiers.count(var))
        identifiers.erase(var);
    identifiers.insert({var, var_info});
    haskell_vars.insert(var_info.x);
    return *this;
}

int get_index_for_arg_name(const Rule& rule, const string& arg_name)
{
    if (auto i = rule.arg_index(arg_name))
        return *i;
    throw myexception()<<"No arg named '"<<arg_name<<"'";
}

// Generates code for typed declarations without converting the declaration
// container or expression bodies through annotated ptree codegen.
translation_result_t CodeGenState::get_model_decls(const CM::TypedDecls& decls)
{
    translation_result_t result;

    for(auto& [var_name, var_exp]: decls)
    {
        auto x = get_var(var_name);
        auto log_x = get_var("log_" + var_name);
        bool x_is_random = is_random(var_exp);
        var_info_t var_info(x, x_is_random);

        // 1. Perform the variable expression
        auto arg_result = get_model_as(var_exp);

        if (arg_result.lambda_vars.size())
            var_info.depends_on_lambda = true;

        // 3. Construct code.
        add(haskell_vars, arg_result.haskell_vars);
        add(result.lambda_vars, arg_result.lambda_vars);

        // (x, log_x) <- arg_result
        perform_action_simplified(result, x, log_x, true, arg_result, var_name);
        auto type = var_exp.ann.type;
        if (x_is_random and is_loggable_type(type))
            result.code.log_value(var_name, x, type);

        // 4. Put x into the scope for the next decl.
        extend_modify_scope(var_name, var_info);
    }

    result.haskell_vars = haskell_vars;

    return result;
}

void add_generated_bound_vars(const set<Hs::Var>& vars, multiset<Hs::Var>& bound)
{
    for(const auto& x: vars)
        bound.insert(x);
}

void remove_generated_bound_vars(const set<Hs::Var>& vars, multiset<Hs::Var>& bound)
{
    for(const auto& x: vars)
    {
        auto i = bound.find(x);
        assert(i != bound.end());
        bound.erase(i);
    }
}

set<Hs::Var> get_generated_bound_vars(const expression_ref& E);

set<Hs::Var> get_generated_free_vars(const expression_ref& E);

set<Hs::Var> get_generated_bound_vars(const Hs::Binds& binds)
{
    set<Hs::Var> bound;
    for(const auto& x: Hs::vars_bound_in_binds(binds))
        bound.insert(unloc(x));
    return bound;
}

void get_generated_free_vars2(const expression_ref& E, multiset<Hs::Var>& bound, set<Hs::Var>& free);

void get_generated_free_vars2(const Hs::MultiGuardedRHS& rhs, multiset<Hs::Var>& bound, set<Hs::Var>& free)
{
    if (rhs.decls)
        throw myexception()<<"get_generated_free_vars: unsupported local declarations in RHS";

    for(auto& guarded_rhs: rhs.guarded_rhss)
    {
        for(auto& guard: guarded_rhs.guards)
            get_generated_free_vars2(unloc(guard), bound, free);
        get_generated_free_vars2(unloc(guarded_rhs.body), bound, free);
    }
}

void get_generated_free_vars2(const Hs::MRule& match, multiset<Hs::Var>& bound, set<Hs::Var>& free)
{
    auto bound_ = Hs::vars_in_patterns(match.patterns);
    for(const auto& x: bound_)
        bound.insert(unloc(x));

    get_generated_free_vars2(match.rhs, bound, free);

    for(const auto& x: bound_)
    {
        auto i = bound.find(unloc(x));
        assert(i != bound.end());
        bound.erase(i);
    }
}

void get_generated_free_vars_in_decl(const Hs::Decl& decl, multiset<Hs::Var>& bound, set<Hs::Var>& free)
{
    if (auto F = decl.to<Hs::FunDecl>())
    {
        for(auto& match: F->matches)
            get_generated_free_vars2(match, bound, free);
    }
    else if (auto P = decl.to<Hs::PatDecl>())
        get_generated_free_vars2(P->rhs, bound, free);
    else
        throw myexception()<<"get_generated_free_vars: unsupported local declaration "<<decl;
}

void get_generated_free_vars2(const Hs::Binds& binds, multiset<Hs::Var>& bound, set<Hs::Var>& free)
{
    for(auto& decls: binds)
        for(auto& [_, decl]: decls)
            get_generated_free_vars_in_decl(decl, bound, free);
}

void get_generated_free_vars2(const expression_ref& E, multiset<Hs::Var>& bound, set<Hs::Var>& free)
{
    if (E.is_a<Hs::Var>())
    {
        auto x = E.as_<Hs::Var>();
        if (not bound.count(x))
            free.insert(x);
        return;
    }

    if (auto A = E.to<Hs::ApplyExp>())
    {
        get_generated_free_vars2(unloc(A->head), bound, free);
        get_generated_free_vars2(unloc(A->arg), bound, free);
        return;
    }
    else if (auto L = E.to<Hs::List>())
    {
        for(auto& element: L->elements)
            get_generated_free_vars2(unloc(element), bound, free);
        return;
    }
    else if (auto T = E.to<Hs::Tuple>())
    {
        for(auto& element: T->elements)
            get_generated_free_vars2(unloc(element), bound, free);
        return;
    }
    else if (auto L = E.to<Hs::LambdaExp>())
    {
        auto bound_ = Hs::vars_in_patterns(L->match.patterns);
        for(const auto& x: bound_)
            bound.insert(unloc(x));

        get_generated_free_vars2(L->match.rhs, bound, free);

        for(const auto& x: bound_)
        {
            auto i = bound.find(unloc(x));
            assert(i != bound.end());
            bound.erase(i);
        }
        return;
    }
    else if (auto L = E.to<Hs::LetExp>())
    {
        auto bound_ = get_generated_bound_vars(unloc(L->binds));
        add_generated_bound_vars(bound_, bound);

        get_generated_free_vars2(unloc(L->binds), bound, free);
        get_generated_free_vars2(unloc(L->body), bound, free);

        remove_generated_bound_vars(bound_, bound);
        return;
    }

    auto bound_ = get_generated_bound_vars(E);
    add_generated_bound_vars(bound_, bound);

    if (is_gcable_type(E.type()))
    {
        // These legacy object atoms do not bind or reference variables on their own.
    }
    else if (E.is_a<String>() or E.is_a<Hs::Con>() or E.is_a<Hs::Literal>())
    {
        // Boxed strings, Haskell constructors, and Haskell literals have no free variables.
    }
    else if (E)
    {
        throw myexception()<<"get_generated_free_vars: unsupported atomic expression "<<E;
    }

    remove_generated_bound_vars(bound_, bound);
}

set<Hs::Var> get_generated_free_vars(const expression_ref& E)
{
    multiset<Hs::Var> bound;
    set<Hs::Var> free;
    get_generated_free_vars2(E, bound, free);
    return free;
}

set<Hs::Var> get_generated_bound_vars(const expression_ref& E)
{
    set<Hs::Var> bound;
    if (auto L = E.to<Hs::LetExp>())
        bound = get_generated_bound_vars(unloc(L->binds));
    return bound;
}

Hs::Binds make_generated_binds(const std::vector<std::pair<Hs::Var, expression_ref>>& decls)
{
    Hs::Decls hs_decls;
    for(auto& [x,E]: decls)
        hs_decls.push_back({noloc, Hs::simple_decl({noloc, x}, {noloc, E})});
    return Hs::Binds({hs_decls});
}

expression_ref make_generated_let(const std::vector<std::vector<std::pair<Hs::Var, expression_ref>>>& decl_groups, const expression_ref& body)
{
    expression_ref result = body;
    for(auto& decls: decl_groups | views::reverse)
    {
        if (decls.empty())
            continue;
        result = Hs::LetExp({noloc, make_generated_binds(decls)}, {noloc, result});
    }
    return result;
}

expression_ref eta_reduce(expression_ref E)
{
    while(true)
    {
        if (auto L = E.to<Hs::LambdaExp>())
        {
            if (L->match.patterns.size() != 1)
                break;
            auto pattern = unloc(L->match.patterns[0]).to<Hs::VarPattern>();
            if (not pattern)
                break;
            auto x = unloc(pattern->var);

            if (L->match.rhs.decls or L->match.rhs.guarded_rhss.size() != 1)
                break;

            auto& guarded_rhs = L->match.rhs.guarded_rhss[0];
            if (not guarded_rhs.guards.empty())
                break;

            auto& body = unloc(guarded_rhs.body);
            auto A = body.to<Hs::ApplyExp>();
            if (not A)
                break;

            auto [head,args] = Hs::decompose_apps({noloc, body});
            if (args.empty() or not unloc(args.back()).is_a<Hs::Var>() or not (unloc(args.back()).as_<Hs::Var>() == x))
                break;

            expression_ref E2;
            if (args.size() == 1)
                E2 = unloc(head);
            else
            {
                vector<expression_ref> args2;
                for(int i=0; i<args.size()-1; i++)
                    args2.push_back(unloc(args[i]));
                E2 = HsG::Apply(unloc(head), args2);
            }

            if (get_generated_free_vars(E2).count(x))
                break;
            E = E2;
            continue;
        }

        break;
    }
    return E;
}

// Finds variable binders inside a typed lambda pattern.
set<string> find_vars_in_pattern(const CM::TypedPattern& pattern)
{
    if (auto var = pattern.to<CM::VarPattern>())
        return {var->name};
    else if (auto tuple = pattern.to<CM::TuplePattern<CM::Ann>>())
    {
        set<string> vars;
        for(auto& sub_pattern: tuple->elements)
        {
            auto slot_vars = find_vars_in_pattern(sub_pattern);
            for(auto& var_name: slot_vars)
            {
                assert(not vars.count(var_name));
                vars.insert(var_name);
            }
        }
        return vars;
    }
    else
        std::abort();
}

// Converts a typed lambda pattern into Haskell source pattern syntax.
Hs::LPat get_typed_pattern(const CM::TypedPattern& pattern, const CodeGenState& scope)
{
    return pattern.visit(CM::overloaded{
        [&](const CM::VarPattern& var) -> Hs::LPat
        {
            if (not scope.identifiers.count(var.name))
                throw myexception()<<"No variable '"<<var.name<<"' in scope!";
            return {noloc, Hs::VarPattern({noloc, scope.identifiers.at(var.name).x})};
        },
        [&](const CM::TuplePattern<CM::Ann>& tuple) -> Hs::LPat
        {
            Hs::LPats elements;
            for(auto& element: tuple.elements)
                elements.push_back(get_typed_pattern(element, scope));
            return {noloc, Hs::tuple_pattern(elements)};
        }
    });
}

// Returns true for the legacy rule-template spelling of a trailing submodel
// argument that should be compiled as `submodel +> f`.
bool is_template_submodel_arg(const CM::Arg<CM::NoAnn>& arg)
{
    if (not arg.name.empty() or not arg.value)
        return false;

    auto arg_ref = arg.value->to<CM::ArgRef>();
    return arg_ref and arg_ref->name == "submodel";
}

// Converts a rule call/computed template from CmdModel to Haskell expression
// code, preserving the old ptree template codegen behavior.
expression_ref make_rule_template_expr(const CM::UntypedExpr& expr, const map<string,expression_ref>& simple_args)
{
    return expr.visit(CM::overloaded{
        [](const CM::IntLiteral& x) -> expression_ref { return Hs::Literal(Hs::Integer{integer(x.value)}); },
        [](const CM::DoubleLiteral& x) -> expression_ref { return Hs::Literal(Hs::Floating{Hs::rationalFromDouble(x.value)}); },
        [](const CM::BoolLiteral& x) -> expression_ref { return x.value ? Hs::True() : Hs::False(); },
        [](const CM::StringLiteral& x) -> expression_ref { return Hs::Literal(Hs::String{x.value}); },
        [](const CM::Var& x) -> expression_ref { return Hs::Var(x.name); },
        // Looks up a rule-template argument reference in the already generated
        // argument environment.
        [&](const CM::ArgRef& x) -> expression_ref
        {
            try
            {
                return simple_args.at(x.name);
            }
            catch(...)
            {
                throw myexception()<<"cannot find argument '"<<x.name<<"'";
            }
        },
        [](const CM::Placeholder&) -> expression_ref { throw myexception()<<"Placeholder is not allowed in rule templates."; },
        [](const CM::GetState&) -> expression_ref { throw myexception()<<"get_state is not allowed in rule templates."; },
        // Compiles a rule-template call as Haskell application, preserving the
        // legacy final-submodel rewrite.
        [&](const CM::Call<CM::NoAnn>& call) -> expression_ref
        {
            expression_ref E = Hs::Var(call.function);
            for(int i=0;i<call.args.size();i++)
            {
                auto& arg = call.args[i];
                if (not arg.name.empty())
                    throw myexception()<<"Named arguments are not allowed in rule templates.";
                if (not arg.value)
                    throw myexception()<<"Missing arguments are not allowed in rule templates.";

                auto arg_expr = make_rule_template_expr(*arg.value, simple_args);
                // Compatibility behavior: rule templates encode `submodel +> f`
                // as a final @submodel argument. Remove when bindings spell this directly.
                if (i == call.args.size()-1 and is_template_submodel_arg(arg))
                    E = HsG::Apply(Hs::Var("+>"), {arg_expr, E});
                else
                    E = HsG::Apply(E, {arg_expr});
            }
            return E;
        },
        // Compiles a rule-template list by translating each element into a
        // located Haskell expression.
        [&](const CM::List<CM::NoAnn>& list) -> expression_ref
        {
            vector<expression_ref> args;
            for(auto& element: list.elements)
                args.push_back(make_rule_template_expr(element, simple_args));
            return HsG::List(args);
        },
        // Compiles a rule-template tuple by translating each element into a
        // located Haskell expression.
        [&](const CM::Tuple<CM::NoAnn>& tuple) -> expression_ref
        {
            vector<expression_ref> args;
            for(auto& element: tuple.elements)
                args.push_back(make_rule_template_expr(element, simple_args));
            return HsG::Tuple(args);
        },
        [](const CM::Let<CM::NoAnn>&) -> expression_ref { throw myexception()<<"let expressions are not allowed in rule templates."; },
        // Preserves the old template lambda support by folding adjacent lambda
        // nodes into one Haskell lambda expression.
        [&](const CM::Lambda<CM::NoAnn>& lambda) -> expression_ref
        {
            auto pattern = lambda.pattern.to<CM::VarPattern>();
            if (not pattern)
                throw myexception()<<"Only variable lambda patterns are allowed in rule templates.";

            auto body = make_rule_template_expr(lambda.body, simple_args);
            Hs::LPat p = {noloc, Hs::VarPattern({noloc,Hs::Var(pattern->name)})};

            if (auto L = body.to<Hs::LambdaExp>())
            {
                auto LE = *L;
                auto& pats = LE.match.patterns;
                pats.insert(pats.begin(), p);
                return LE;
            }
            else
                return Hs::LambdaExp({p}, {noloc, body});
        },
        // Compatibility behavior: rule templates share the model parser, so
        // sample(@dist) is parsed as Sample but means a Haskell sample call here.
        [&](const CM::Sample<CM::NoAnn>& sample) -> expression_ref
        {
            auto dist = make_rule_template_expr(sample.dist, simple_args);
            return HsG::Apply(Hs::Var("sample"), {dist});
        }
    });
}

vector<bool> get_args_referenced(const vector<string>& arg_names, const vector<set<string>>& used_args_for_arg)
{
    vector<bool> referenced(arg_names.size(), false);
    for(int i=0;i<arg_names.size();i++)
        for(int j=0;j<arg_names.size();j++)
        {
            if (used_args_for_arg[i].count(arg_names[j]))
            {
                referenced[j] = true;
                if (log_verbose > 2)
                    std::cerr<<arg_names[i]<<" references "<<arg_names[j]<<"\n";
            }
        }
    return referenced;
}

vector<int> get_args_order(const vector<string>& arg_names, const vector<set<string>> used_args_for_arg)
{
    const int N = arg_names.size();

    // 1. Construct the directed reference graph
    auto G = make_graph(N, [&](int i, int j) { return used_args_for_arg[i].count(arg_names[j]); });

    // 2. Complain about loops
    for(auto& loop_component: get_loop_components(G))
    {
        if (loop_component.size() == 1)
        {
            int i = loop_component[0];
            throw myexception()<<"default argument for '"<<arg_names[i]<<"' references itself!";
        }
        else
        {
            vector<string> loop_names;
            for(int i: loop_component)
                loop_names.push_back(arg_names[i]);
            throw myexception()<<"Must specify at least one of: "<<join(loop_names,',');
        }
    }

    auto order = topo_sort(G);
//    std::reverse(order.begin(), order.end());
    return order;
}


// Builds generated code for a typed literal expression without passing through
// the annotated-ptree constant helper.
translation_result_t get_typed_constant_model(const CM::TypedExpr& expr)
{
    translation_result_t result;
    expr.visit(CM::overloaded{
        [&](const CM::IntLiteral& x) { result.code.E = Hs::Literal(Hs::Integer{integer(x.value)}); },
        [&](const CM::DoubleLiteral& x) { result.code.E = Hs::Literal(Hs::Floating{Hs::rationalFromDouble(x.value)}); },
        [&](const CM::BoolLiteral& x) { result.code.E = x.value ? Hs::True() : Hs::False(); },
        [&](const CM::StringLiteral& x) { result.code.E = Hs::Literal(Hs::String{x.value}); },
        [](const auto&) { std::abort(); }
    });
    return result;
}

// Builds generated code for typed variables and @argument references, matching
// the old scope and default-argument lookup behavior.
translation_result_t CodeGenState::get_typed_variable_model(const CM::TypedExpr& expr) const
{
    translation_result_t result;

    if (auto arg_ref = expr.to<CM::ArgRef>())
    {
        if (not arg_env)
            throw myexception()<<"Looking up argument '"<<arg_ref->name<<"' in an empty environment!";

        auto& env = *arg_env;
        if (not env.code_for_arg.count(arg_ref->name))
            throw myexception()<<env.func<<"."<<env.arg<<": can't find argument '"<<arg_ref->name<<"' referenced in default_value or alphabet";

        result.code.E = env.code_for_arg.at(arg_ref->name);
    }
    else if (auto var = expr.to<CM::Var>())
    {
        if (not identifiers.count(var->name))
            throw myexception()<<"No variable '"<<var->name<<"' in scope!";

        auto var_info = identifiers.at(var->name);
        if (var_info.depends_on_lambda)
            result.lambda_vars = {var->name};
        result.code.free_vars.insert({var->name, var_info.x});
        result.code.E = var_info.x;
    }
    else
        std::abort();

    return result;
}

// Builds generated code for a typed get_state expression by looking up the
// requested state variable in the current codegen state.
translation_result_t CodeGenState::get_typed_model_state(const CM::GetState& get_state) const
{
    if (state.count(get_state.state_name))
    {
        auto x = state.at(get_state.state_name);
        translation_result_t result;
        result.code.E = x;
        result.code.used_states = {get_state.state_name};
        return result;
    }
    else
        throw myexception()<<"No state '"<<get_state.state_name<<"'!";
}

// Generates code for a typed list by traversing CM elements directly while
// preserving the old element logging and action sequencing behavior.
translation_result_t CodeGenState::get_typed_model_list(const CM::List<CM::Ann>& list) const
{
    auto scope2 = *this;
    int N = list.elements.size();

    translation_result_t result;
    vector<Hs::Exp> elements(N);
    for(int i=0; i<N; i++)
    {
        string var_name = "_"+std::to_string(i+1);
        string log_name = "["+std::to_string(i+1)+"]";

        auto x = scope2.get_var(var_name);
        elements[i] = x;

        auto& element = list.elements[i];
        auto element_result = scope2.get_model_as(element);
        add(scope2.haskell_vars, element_result.haskell_vars);

        auto log_x = scope2.get_var("log"+var_name);
        use_block(result, log_x, element_result, log_name);

        bool do_log = is_unlogged_random(element) and is_loggable_type(element.ann.type);
        if (element_result.code.perform_function)
            HsG::Bind(result.code.stmts, HsG::VarPat(x), element_result.code.E);
        else if (do_log and not is_generated_var_expr(element_result.code.E))
            HsG::Let(result.code.stmts, x, element_result.code.E);
        else
            elements[i] = element_result.code.E;

        if (do_log)
            result.code.log_value(log_name, elements[i], element.ann.type);
    }

    result.code.E = HsG::List(elements);
    add(result.haskell_vars, scope2.haskell_vars);

    return result;
}

// Generates code for a typed tuple by traversing CM elements directly while
// preserving the old element logging and action sequencing behavior.
translation_result_t CodeGenState::get_typed_model_tuple(const CM::Tuple<CM::Ann>& tuple) const
{
    auto scope2 = *this;
    int N = tuple.elements.size();

    translation_result_t result;
    vector<Hs::Exp> elements(N);
    for(int i=0; i<N; i++)
    {
        string var_name = "_"+std::to_string(i+1);
        string log_name = "["+std::to_string(i+1)+"]";

        auto x = scope2.get_var(var_name);
        elements[i] = x;

        auto& element = tuple.elements[i];
        auto element_result = scope2.get_model_as(element);
        add(scope2.haskell_vars, element_result.haskell_vars);

        auto log_x = scope2.get_var("log"+var_name);
        use_block(result, log_x, element_result, log_name);

        bool do_log = is_unlogged_random(element) and is_loggable_type(element.ann.type);
        if (element_result.code.perform_function)
            HsG::Bind(result.code.stmts, HsG::VarPat(x), element_result.code.E);
        else if (do_log and not is_generated_var_expr(element_result.code.E))
            HsG::Let(result.code.stmts, x, element_result.code.E);
        else
            elements[i] = element_result.code.E;

        if (do_log)
            result.code.log_value(log_name, elements[i], element.ann.type);
    }

    result.code.E = HsG::Tuple(elements);
    add(result.haskell_vars, scope2.haskell_vars);

    return result;
}

// Generates code for a typed let by using native typed declaration codegen and
// then applying the same declaration grouping logic as the old ptree path.
translation_result_t CodeGenState::get_typed_model_let(const CM::Let<CM::Ann>& let) const
{
    auto scope2 = *this;

    auto body = scope2.get_var("body");
    auto log_body = scope2.get_var("log_body");

    auto result = scope2.get_model_decls(let.decls);

    auto body_result = scope2.get_model_as(let.body);

    use_block(result, log_body, body_result, "body");
    result.code.E = body_result.code.E;
    result.code.perform_function = body_result.code.perform_function;

    vector<std::vector<std::pair<Hs::Var, expression_ref>>> decls_groups(1);
    set<Hs::Var> prev_free_vars;
    for(auto& [x,E]: result.code.decls)
    {
        if (prev_free_vars.count(x))
        {
            decls_groups.push_back({});
            prev_free_vars.clear();
        }
        decls_groups.back().push_back({x,E});
        add(prev_free_vars, get_generated_free_vars(E));
        prev_free_vars.insert(x);
    }
    result.code.E = make_generated_let(decls_groups, result.code.E);
    result.code.decls.clear();

    for(auto& [var_name,_]: let.decls)
    {
        result.code.free_vars.erase(var_name);
        result.lambda_vars.erase(var_name);
    }

    return result;
}

// Generates code for a typed lambda by extending scope with typed pattern
// binders, then generating code for the typed body and pattern.
translation_result_t CodeGenState::get_typed_model_lambda(const CM::Lambda<CM::Ann>& lambda) const
{
    auto scope2 = *this;

    auto var_names = find_vars_in_pattern(lambda.pattern);

    for(auto& var_name: var_names)
    {
        if (identifiers.count(var_name))
        {
            auto x = identifiers.at(var_name).x;
            scope2.haskell_vars.erase(x);
        }
    }

    for(auto& var_name: var_names)
    {
        auto x = scope2.get_var(var_name);
        var_info_t var_info(x,false,true);
        scope2.extend_modify_scope(var_name, var_info);
    }
    auto body_result = scope2.get_model_as(lambda.body);

    for(auto& var_name: var_names)
        if (body_result.lambda_vars.count(var_name))
            body_result.lambda_vars.erase(var_name);

    auto pattern2 = get_typed_pattern(lambda.pattern, scope2);
    body_result.code.E = Hs::LambdaExp({pattern2}, {noloc, body_result.code.E});

    body_result.code.E = eta_reduce(body_result.code.E);
    for(auto& var_name: var_names)
        body_result.code.free_vars.erase(var_name);

    return body_result;
}

// Generates code for applying a function-valued variable or @argument to typed
// positional arguments, matching the old variable-call codegen path.
translation_result_t CodeGenState::get_typed_variable_call(const CM::Call<CM::Ann>& call) const
{
    translation_result_t result;
    string name = call.function;
    if (not name.empty() and name[0] == '@')
    {
        name = name.substr(1);
        if (not arg_env)
            throw myexception()<<"Looking up argument '"<<name<<"' in an empty environment!";

        auto& env = *arg_env;
        if (not env.code_for_arg.count(name))
            throw myexception()<<env.func<<"."<<env.arg<<": can't find argument '"<<name<<"' referenced in default_value or alphabet";

        result.code.E = env.code_for_arg.at(name);
    }
    else if (identifiers.count(name))
    {
        auto var_info = identifiers.at(name);
        if (var_info.depends_on_lambda)
            result.lambda_vars = {name};
        result.code.free_vars.insert({name, var_info.x});
        result.code.E = var_info.x;
    }
    else
        throw myexception()<<"No variable or rule named '"<<call.function<<"' in scope!";

    auto scope2 = *this;
    int i=0;
    for(auto& arg: call.args)
    {
        if (not arg.name.empty())
            throw myexception()<<"Named arguments not allowed in functions that are variables";

        string var_name = name + "_" + std::to_string(i+1);
        string log_name = name + ":" + std::to_string(i+1);

        auto& arg_expr = CM::require_arg_value(arg);
        auto arg_model = scope2.get_model_as(arg_expr);
        auto arg_code = arg_model.code;

        add(scope2.haskell_vars, arg_model.haskell_vars);

        auto x = scope2.get_var(var_name);
        auto log_x = scope2.get_var("log_" + var_name);

        bool do_log = is_unlogged_random(arg_expr) and is_loggable_type(arg_expr.ann.type) and arg_model.lambda_vars.empty();

        use_block(result, log_x, arg_model, log_name);
        expression_ref applied_arg = arg_code.E;
        if (arg_code.perform_function)
        {
            applied_arg = log_x;
            HsG::Bind(result.code.stmts, HsG::VarPat(x), arg_code.E);
            assert(arg_model.lambda_vars.empty());
        }
        else if (do_log and not is_generated_var_expr(arg_code.E))
        {
            applied_arg = log_x;
            HsG::Let(result.code.stmts, x, arg_code.E);
            assert(arg_model.lambda_vars.empty());
        }

        if (do_log)
            result.code.log_value(log_name, applied_arg, arg_expr.ann.type);

        result.code.E = HsG::Apply(result.code.E, {applied_arg});

        i++;
    }

    result.code.E = simplify_intToDouble(result.code.E);
    add(result.haskell_vars, scope2.haskell_vars);

    return result;
}

// Generates code for a typed rule-backed call, preserving the old argument
// ordering, default/alphabet handling, logging, and computed-variable behavior.
translation_result_t CodeGenState::get_typed_rule_call(const CM::Call<CM::Ann>& call) const
{
    auto scope2 = *this;
    auto name = call.function;

    translation_result_t result;

    auto rule = R->get_rule_for_func(name);
    if (not rule) throw myexception()<<"No rule for '"<<name<<"'";
    result.imports = rule->imports;

    result.code.perform_function = rule->perform;
    const auto& rule_call = rule->call;
    const auto& args = rule->args;

    map<string,const CM::Arg<CM::Ann>*> typed_args;
    for(auto& arg: call.args)
        typed_args[arg.name] = &arg;

    vector<string> arg_names(args.size());
    map<string,expression_ref> argument_environment;
    vector<Hs::Var> arg_vars;
    vector<Hs::Var> log_vars;
    vector<set<string>> used_args_for_arg(args.size());
    for(int i=0;i<args.size();i++)
    {
        arg_names[i] = args[i].name;
        auto& arg = *typed_args.at(arg_names[i]);
        auto& arg_expr = CM::require_arg_value(arg);

        if (arg.is_default_value)
            used_args_for_arg[i] = arg_expr.ann.used_args;
        if (arg.alphabet)
            add(used_args_for_arg[i], arg.alphabet->ann.used_args);

        auto var_name = arg_names[i];
        if (var_name == "submodel")
            if (auto func_name = get_func_name(arg_expr))
                var_name = (*func_name)+"_model";
        arg_vars.push_back(scope2.get_var(var_name));
        log_vars.push_back(scope2.get_var("log_"+var_name));

        argument_environment[arg_names[i]] = arg_vars.back();
    }

    auto arg_referenced = get_args_referenced(arg_names, used_args_for_arg);
    auto arg_order = get_args_order(arg_names, used_args_for_arg);

    vector<translation_result_t> arg_models(args.size());
    vector<string> log_names(args.size());

    for(int i: arg_order)
    {
        log_names[i] = name + ":" + arg_names[i];

        auto& arg = *typed_args.at(arg_names[i]);
        auto& arg_expr = CM::require_arg_value(arg);

        auto arg_scope = scope2;
        if (arg.is_default_value)
            arg_scope.arg_env = {{name,arg_names[i],argument_environment}};

        optional<translation_result_t> alphabet_result;
        optional<Hs::Var> alphabet_var;
        optional<Hs::Var> log_alphabet;
        if (arg.alphabet)
        {
            string var_name = "alpha";
            auto& alphabet_expr = *arg.alphabet;
            if (auto alphabet_var_expr = alphabet_expr.to<CM::Var>(); alphabet_var_expr and alphabet_var_expr->name == "getNucleotides")
                var_name = "nucs";
            alphabet_var = scope2.get_var(var_name);
            log_alphabet = scope2.get_var("log_"+arg_names[i]+"_alpha");

            auto alphabet_scope = scope2;
            alphabet_scope.arg_env = {{name,arg_names[i],argument_environment}};
            alphabet_result = alphabet_scope.get_model_as(alphabet_expr);
            add(arg_scope.haskell_vars, alphabet_result->haskell_vars);
            if (alphabet_result->lambda_vars.size())
                throw myexception()<<"An alphabet cannot depend on a lambda variable!";

            if (is_generated_var_expr(alphabet_result->code.E))
                arg_scope.set_state("alphabet", as_generated_var(alphabet_result->code.E));
            else
                arg_scope.set_state("alphabet", *alphabet_var);
        }

        arg_models[i] = arg_scope.get_model_as(arg_expr);
        add(arg_scope.haskell_vars, arg_models[i].haskell_vars);

        if (arg_models[i].code.used_states.count("alphabet") and alphabet_result)
        {
            assert(not alphabet_result->code.has_loggers());
            assert(not alphabet_result->code.perform_function);
            use_block(result, *log_alphabet, *alphabet_result, log_names[i]+":alphabet");

            if (not is_generated_var_expr(alphabet_result->code.E))
                HsG::Let(result.code.stmts, *alphabet_var, alphabet_result->code.E);
        }

        if (result.code.perform_function and arg_models[i].lambda_vars.size())
            throw myexception()<<"Argument '"<<arg_names[i]<<"' of '"<<name<<"' contains a lambda variable: not allowed!";

        auto x = arg_vars[i];
        auto log_x = log_vars[i];

        bool do_log = is_loggable_function(*R, name) and is_unlogged_random(arg_expr) and is_loggable_type(arg_expr.ann.type) and arg_models[i].lambda_vars.empty();

        use_block(result, log_x, arg_models[i], log_names[i]);
        if (arg_models[i].code.perform_function)
            HsG::Bind(result.code.stmts, HsG::VarPat(x), arg_models[i].code.E);
        else if ((arg_referenced[i] or do_log) and not is_generated_var_expr(arg_models[i].code.E))
            HsG::Let(result.code.stmts, x, arg_models[i].code.E);
        else
            argument_environment[arg_names[i]] = arg_models[i].code.E;

        if (do_log)
            result.code.log_value(log_names[i], argument_environment[arg_names[i]], arg_expr.ann.type);

        add(scope2.haskell_vars, arg_scope.haskell_vars);
    }

    if (not rule->computed.empty())
    {
        for(auto& x: rule->computed)
        {
            auto x_name = x.name;
            auto x_log_name = name + ":" + x_name;
            auto x_var = scope2.get_var(x_name);

            auto& value = x.value;
            auto x_type = type_t("unknown_type");
            HsG::Let(result.code.stmts, x_var, make_rule_template_expr(value, argument_environment));

            result.code.log_value(x_log_name, x_var, x_type);

            argument_environment[x_name] = x_var;
        }
    }

    try
    {
        result.code.E = make_rule_template_expr(rule_call, argument_environment);

        result.code.E = simplify_intToDouble(result.code.E);
    }
    catch(myexception& err)
    {
        err.prepend("In call for function '"+name+"': ");
        throw;
    }

    add(result.haskell_vars, scope2.haskell_vars);

    return result;
}

// Generates code for a typed call, choosing the rule-backed or function-valued
// variable path according to the current rules and scope.
translation_result_t CodeGenState::get_typed_model_call(const CM::Call<CM::Ann>& call) const
{
    if ((not call.function.empty() and call.function[0] == '@') or identifiers.count(call.function))
        return get_typed_variable_call(call);
    else if (R->get_rule_for_func(call.function))
        return get_typed_rule_call(call);
    else
        return get_typed_variable_call(call);
}

// Generates code for typed sample sugar by reusing the rule-backed sample call
// code path with the already typed distribution argument.
translation_result_t CodeGenState::get_typed_model_sample(const CM::Sample<CM::Ann>& sample) const
{
    CM::Call<CM::Ann> sample_call{
        "sample",
        {
            CM::Arg<CM::Ann>{
                "dist",
                sample.dist,
                false,
                false,
                std::nullopt
            }
        }
    };
    return get_typed_rule_call(sample_call);
}

// Dispatches typed expressions by AST variant for native typed codegen.
translation_result_t CodeGenState::get_model_as(const CM::TypedExpr& model_rep) const
{
    return model_rep.visit(CM::overloaded{
        [&](const CM::IntLiteral&) { return get_typed_constant_model(model_rep); },
        [&](const CM::DoubleLiteral&) { return get_typed_constant_model(model_rep); },
        [&](const CM::BoolLiteral&) { return get_typed_constant_model(model_rep); },
        [&](const CM::StringLiteral&) { return get_typed_constant_model(model_rep); },
        [&](const CM::Var&) { return get_typed_variable_model(model_rep); },
        [&](const CM::ArgRef&) { return get_typed_variable_model(model_rep); },
        [&](const CM::Placeholder&) -> translation_result_t { throw myexception()<<"Placeholder '_' cannot be code generated."; },
        [&](const CM::GetState& get_state) { return get_typed_model_state(get_state); },
        [&](const CM::Call<CM::Ann>& call) { return get_typed_model_call(call); },
        [&](const CM::List<CM::Ann>& list) { return get_typed_model_list(list); },
        [&](const CM::Tuple<CM::Ann>& tuple) { return get_typed_model_tuple(tuple); },
        [&](const CM::Let<CM::Ann>& let) { return get_typed_model_let(let); },
        [&](const CM::Lambda<CM::Ann>& lambda) { return get_typed_model_lambda(lambda); },
        [&](const CM::Sample<CM::Ann>& sample) { return get_typed_model_sample(sample); }
    });
}
