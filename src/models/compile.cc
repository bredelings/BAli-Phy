/*
  Copyright (C) 2004-2024 Benjamin Redelings

  This file is part of BAli-Phy.

  BAli-Phy is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2, or (at your option) any later
  version.

  BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
  for more details.

  You should have received a copy of the GNU General Public License
  along with BAli-Phy; see the file COPYING.  If not see
  <http://www.gnu.org/licenses/>.  */

///
/// \brief Create substitution models from strings of the form model[arg,arg]+...+model[arg,arg].
///

#include <vector>
#include <sstream>
#include <boost/program_options.hpp>

#include "util/graph.H"

#include "models/compile.H"
#include "sequence/doublets.H"
#include "sequence/codons.H"
#include "util/string/join.H"
#include "util/string/pred.H"
#include "util/set.H"
#include "util/range.H"
#include "util/myexception.H"
#include "models/rules.H"
#include "models/parse.H"
#include "models/path.H"
#include "computation/module.H"
#include "computation/expression/expression_ref.H"
#include "computation/operations.H"
#include "computation/haskell/ids.H"
#include "computation/expression/lambda.H"  // for is_lambda_exp( )
#include "computation/expression/tuple.H"   // for Tuple( )
#include "range/v3/all.hpp"

namespace views = ranges::views;

extern int log_verbose;

using std::optional;
using std::string;
using std::pair;
using std::tuple;
using std::set;
using std::map;
using std::vector;
using std::valarray;
using std::shared_ptr;
using boost::program_options::variables_map;

string model_t::show(bool top) const
{
    if (top)
        return show_model_annotated(description);
    else
        return unparse_annotated(description);
}

string model_t::show_pretty(bool top) const
{
    auto p = pretty_model_t(description);
    return p.show(not top);
}

string model_t::show_main(bool top) const
{
    auto p = pretty_model_t(description);
    return p.show_main(top);
}

string model_t::show_extracted() const
{
    auto p = pretty_model_t(description);
    return p.show_extracted();
}

model_t::model_t(const ptree& d, const set<string>& i, const ptree&t, const std::set<term_t>& s, const generated_code_t& c)
    :description(d), imports(i), type(t), constraints(s), code(c)
{
}

json::object convert_to_json(const pretty_model_t& m)
{
    json::object j;
    j["main"] = unparse_annotated(m.main);
    json::array extracted;
    for(int i=0;i<m.terms.size();i++)
    {
	json::array p(2);
        p[0] = json::string(m.term_names[i]);
        p[1] = convert_to_json(m.terms[i]);
        extracted.push_back(p);
    }
    j["extracted"] = extracted;
    return j;
}


string show(vector<string> args)
{
    string output = args[0];
    args.erase(args.begin());
    if (args.empty()) return output;

    output += '[';
    output += join(args,',');
    output += ']';
    return output;
}

string print_equals_function(expression_ref E)
{
    std::ostringstream result;
    while(is_lambda_exp(E))
    {
        auto x = E.sub()[0];
        E = E.sub()[1];
        result<<" "<<x;
    }
    result<<" = "<<E;
    return result.str();
}

/// \brief Return the default substitution model name for alphabet \a a, and "" if there is no default.
string default_markov_model(const alphabet& a) 
{
    if (dynamic_cast<const Nucleotides*>(&a))
        return "tn93";
    else if (dynamic_cast<const AminoAcidsWithStop*>(&a))
        return "";
    else if (dynamic_cast<const AminoAcids*>(&a))
        return "lg08";
    else if (dynamic_cast<const Codons*>(&a))
        return "gy94";
    else if (dynamic_cast<const Doublets*>(&a))
        return "tn93_sym +> x2_sym +> f";
    else if (dynamic_cast<const Triplets*>(&a))
        return "tn93 +> x3";
    else
        return "";
}

// See simplify(json& j) in models/model.cc and simplify(ptree&) models/in path.cc

expression_ref generated_code_t::generate() const
{
    do_block code(stmts);

    auto loggers2 = loggers;
    simplify(loggers2);
    auto L = generate_loggers_list(code,loggers2);

    auto R = simplify_intToDouble(E);

    if (not perform_function)
    {
        if (has_loggers())
        {
            // FIXME: technically, we should make sure that "result" and "loggers" are unique names
            // FIXME: it would be nice to use e.g. tn93_model = ... instead of just result = ....
            //        see:  var_name = (*func_name)+"_model";  for naming "submodel" arguments.
            code.let(var("result"),R);
            code.let(var("loggers"),L);
            R = Tuple(var("result"),var("loggers"));
        }
        // If there are let stmts, we could return let{decls} in R
        if (not code.empty())
            R = code.finish_return(R);
    }
    else
    {
        if (has_loggers())
        {
            // result <- E
            code.perform( var("result"), R );
            // return (result, loggers)
            R = code.finish_return( Tuple(var("result"),L) );
        }
        else if (not code.empty())
            R = code.finish( R );
    }

    // if is_action() is false, we should not have a do_block()
    assert(is_action() or not R.is_a<do_block>());

    for(auto& x : haskell_lambda_vars | views::reverse)
        R = lambda_quantify(x,R);

    return R;
}

void maybe_log(vector<expression_ref>& loggers,
               const string& name,
               const expression_ref& value,
               const expression_ref& subloggers)
{
    expression_ref logger_bit;
    if (value)
        loggers.push_back({var("%=%"),String(name),value});
    if (subloggers)
        loggers.push_back({var("%>%"),String(name),subloggers});
}


void generated_code_t::log_value(const string& name, expression_ref value, const type_t& type)
{
    auto [head,args] = get_type_apps(type);
    if (head == "DiscreteDist" and args[0] == "Double")
    {
	value = {var("sortDist"),value};
    }

    loggers.push_back(LogValue(name, value));
}

void generated_code_t::log_sub(const string& name, const var& log_var, const Loggers& ls)
{
    loggers.push_back(LogSub(name, log_var, ls));
}

void substitute_annotated(const equations& equations, ptree& model)
{
    if (model.has_value<string>() and model.get_value<string>() == "!Decls")
    {
	for(auto& [name,exp]: model)
	    substitute_annotated(equations, exp);
	return;
    }

    auto& type = model.get_child("type");
    substitute(equations, type);

    for(auto& [key, value]: model.get_child("value"))
        substitute_annotated(equations, value);
}

TypecheckingState makeTypechecker(const Rules& R,
				  const vector<pair<string,ptree>>& scope,
				  const map<string,pair<string,ptree>>& state)
{
    auto fv_source = std::make_shared<FVSource>();

    map<string,ptree> typed_scope;
    for(auto& [name,type]: scope)
        typed_scope.insert({name, type});

    map<string,ptree> typed_state;
    for(auto& [state_name,p]: state)
    {
        auto& [_, var_type] = p;
        typed_state.insert({state_name,var_type});
    }

    return TypecheckingState(R, fv_source, typed_scope, typed_state);
}

// QUESTION: How can we move the smodel definitions into the decls?
// QUESTION: How do we keep track of code_gen_state across (say) models?
// QUESTION: In decls, we WANT the same (non-haskell) name to override previous instances of the same name.
//           But for lifted arguments of cmdline-language expressions, maybe we don't?
// QUESTION: Can/should we have a pre-processing state where we lift monadic arguments into named prior expressions?
model_t compile_model(const Rules& R,
		      const TypecheckingState& TC,
		      CodeGenState code_gen_state,
		      ptree required_type, const string& model_string, const string& what,
		      const vector<pair<string,ptree>>& scope,
		      const map<string,pair<string,ptree>>& state)
{
    // 1. Parse
    auto model_rep = parse(R, model_string, what);
//    std::cout<<"model1 = "<<show(model_rep)<<std::endl;

    // 2. Typecheck
    auto TC2 = TC;
    for(auto& [name,type]: scope)
        TC2.identifiers.insert({name, type});
    auto model = TC2.typecheck_and_annotate(required_type, model_rep);

    substitute(TC2.eqs, required_type);
    substitute_annotated(TC2.eqs, model);

    set<ptree> constraints;
    for(auto constraint: TC2.eqs.get_constraints())
    {
	substitute(TC2.eqs, constraint);
	constraints.insert(constraint);
    }

    if (log_verbose >= 2)
    {
        std::cout<<"model = "<<unparse_annotated(model)<<std::endl;
        std::cout<<"type = "<<unparse_type(required_type)<<std::endl;
        std::cout<<"equations: "<<TC.eqs.show()<<std::endl;

	model_rep = extract_value(model);
	substitute(TC.eqs, model_rep);
        std::cout<<"structure = "<<show(model_rep)<<std::endl;

        std::cout<<"annotated structure = "<<show(model)<<std::endl;
        std::cout<<"pretty:\n"<<pretty_model_t(model).show()<<std::endl;
        std::cout<<std::endl;
    }

    // 3. Generate code - translate to Haskell
    vector<var> lambda_vars;

    for(auto& [name,type]: scope)
    {
        auto x = code_gen_state.get_var(name);
        code_gen_state.identifiers.insert({name, var_info_t(x)});
    }

    for(auto& [state_name,p]: state)
    {
        auto& [var_name, _] = p;
        auto x = code_gen_state.get_var(var_name);
        code_gen_state.set_state(state_name,x);
        lambda_vars.push_back(x);
    }

    auto [code, imports, _lambda_vars, _vars] = code_gen_state.get_model_as(model);

    if (log_verbose >= 3)
        std::cout<<"full_model = "<<code.print()<<std::endl;

    // 4. Hack to make sure we generate Haskell arguments to pass in the state variables.
    for(const string& state_name: code.used_states)
        code.haskell_lambda_vars.push_back( code_gen_state.state.at(state_name) );

    for(auto& [var_name, x]: code.free_vars)
	code.haskell_lambda_vars.push_back( x );

    return model_t{model, imports, required_type, constraints, code};
}

/*
 * A lot of work was done to allow writing x = sample(dist) instead of x <- sample(dist).
 * Do we still want that?
 */

// This is being called from bali-phy/A-T-model.cc: create_A_and_T_model( ).
model_t compile_decls(const Rules& R,
		      TypecheckingState& TC,
		      CodeGenState& code_gen_state,
		      const string& prog,
		      const vector<pair<string,ptree>>& scope,
		      const map<string,pair<string,ptree>>& state)
{
    // 1. Parse declarations and substitute any default values.
    auto decls = parse_defs(R, prog);

    // 2. Typecheck.
    auto decls2 = TC.typecheck_and_annotate_decls(decls);

    for(auto& [var,value]: decls2)
	substitute_annotated(TC.eqs, value);
    set<ptree> constraints;
    for(auto constraint: TC.eqs.get_constraints())
    {
	substitute(TC.eqs, constraint);
	constraints.insert(constraint);
    }

    // 3. Generate code - translate to Haskell
    vector<var> lambda_vars;

    for(auto& [name,type]: scope)
    {
        auto x = code_gen_state.get_var(name);
        code_gen_state.identifiers.insert({name, var_info_t(x)});
    }

    for(auto& [state_name,p]: state)
    {
        auto& [var_name, _] = p;
        auto x = code_gen_state.get_var(var_name);
        code_gen_state.set_state(state_name,x);
        lambda_vars.push_back(x);
    }

    auto [code, imports, _1, _2] = code_gen_state.get_model_decls(decls2);

    if (log_verbose >= 3)
        std::cout<<"full_model = "<<code.print()<<std::endl;

    // 4. Hack to make sure we generate Haskell arguments to pass in the state variables.
    for(const string& state_name: code.used_states)
        code.haskell_lambda_vars.push_back( code_gen_state.state.at(state_name) );

    return model_t{decls2, imports, {}, constraints, code};
}


// Some things, like log, exp, add, sub, etc. don't really have named arguments.
//    For these things exp[~normal[0,1]] remains exp[~normal[0,1]]
// Some things, like normal, gamma, etc. have named arguments.
//    For these things, we pull out random arguments, so that normal[~normal[0,1],1] becomes
//      normal[mu,1]: normal:mu ~ norma[0,1]
// Some things like tn, hky, etc. are considered to have named parameters.
//    For these things, we pull out all arguments that are numbers.
//    So, tn93[1,~log_normal[0,1]] becomes tn93 ; tn:kappa_pur=1 , tn:kappa_pyr ~ lognormal[0,1]

// However, we would like gy94[pi=f1x4] to NOT pull out the pi, because f1x4 is ALSO a model.

bool annotated_term_is_model(const ptree& term)
{
    if (term.get<string>("extract","none") == "all") return true;

    auto value = term.get_child("value");

    if (value.has_value<string>() and value.get_value<string>() == "List")
    {
        for(auto& [arg_name,arg_value]: term.get_child("value"))
            if (annotated_term_is_model(arg_value)) return true;
    }

    return false;
}

bool bound(const ptree& annotated_term, const set<string>& binders)
{
    assert(annotated_term.get_child_optional("value"));
    auto& value = annotated_term.get_child("value");

    // Handle constants
    if (not  value.has_value<string>()) return false;

    string func_name = value.get_value<string>();

    if (func_name == "!let")
    {
	auto& decls = value[0].second;
	auto& body  = value[1].second;

        auto binders2 = binders;
	for(auto& [var_name, _]: decls)
            binders2.erase(var_name);

        if (bound(body, binders2)) return true;

	for(auto& [var_name, exp]: decls)
	    if (bound(exp, binders2)) return true;
    }
    else if (func_name == "function")
    {
        string var_name = value[0].second.get_child("value").get_value<string>();
        auto binders2 = binders;
        binders2.erase(var_name);

        return bound(value[1].second, binders2);
    }
    else
    {
        if (binders.count(func_name)) return true;

        for(auto& [arg_name,arg_value]: value)
            if (bound(arg_value, binders)) return true;
    }

    return false;
}

// Don't extract terms that
// * contain function variables
// * don't extract gamma::n if its an integer
// Suppress gamma::a = getAlphabet
// Some terms seem to have types like 'var5' so they always fail the check for
//   extractable types.

bool do_extract(const ptree& func, const ptree& arg, const set<string>& binders)
{
    // 1. Don't extract arguments to e.g. log[], add[], sub[], etc.
    //    This is supposed to indicate things who arguments don't really have names?
    if (func.get("no_log",false)) return false;

    string func_name = func.get_child("value").get_value<string>();

    assert(func_name != "!let");
    assert(func_name != "function");

    // 1d. Don't pull anything out of lists.
    if (func_name == "List") return false;
    // 1e. Don't pull anything out of tuples.
    if (func_name == "Tuple") return false;

    if (bound(arg, binders)) return false;

    auto arg_value = arg.get_child("value");
    string arg_type = unparse_type(arg.get_child("type"));

    // 2. If this is a model, then extract non-random things that are not models.
    if (annotated_term_is_model(func))
    {
        if (annotated_term_is_model(arg)) return false;

        // FIXME - It would be nice if we could universally return true here.
        //         But first, we need to handle - not extracting gamma::n
        //                                      - suppressing gamma::a = getAlphabet
        if (arg_type == "Int" or arg_type == "Double" or arg_type == "LogDouble")
            return true;
        if (arg_type == "List<Double>" or arg_type == "List<(String,Double)>")
            return true;
    }

    if (not is_constant(arg_value))
    {
        auto arg_name = arg_value.get_value<string>();

        // 3. Pull out random arguments
        if (arg_name == "sample") return true;
    }

    return false;
}

// E = {type: T,value:{arg1:E,...argn:E}}

// This only extracts from the top level...

vector<pair<string, ptree>> extract_terms(ptree& m, const set<string>& binders)
{
    // move value's children out of the structure
    ptree& value = m.get_child("value");

    vector<pair<string,ptree>> extracted;
    // 1. Let statements
    if (value.has_value<string>() and value.get_value<string>() == "!let")
    {
        assert(value.size() == 2);
	auto decls = value[0].second;
	auto body  = value[1].second;

        auto extracted = extract_terms(body, binders);
        m = body;

	for(auto& [var_name, exp]: decls)
	    extracted.insert(extracted.begin(),{var_name,exp});
    }
    // 2. Lambda functions
    else if (value.has_value<string>() and value.get_value<string>() == "function")
    {
        string var_name = value[0].second.get_child("value").get_value<string>();
        auto binders2 = binders;
        binders2.insert(var_name);

        for(auto& [sub_name, sub_term]: extract_terms(value[1].second, binders2))
            extracted.emplace_back(sub_name, std::move(sub_term));
    }
    // 3. Function calls
    else
    {
        vector<pair<string,ptree>> extracted_top;
        int i=0;
        // Walk each argument and determine if it should be pulled out
        for(auto& [arg_name,arg_value]: value)
        {
            auto func = value.get_value<string>();
            string name = func + ":" + arg_name;
            if (func == "List" or func == "Tuple")
            {
                name = "["+std::to_string(++i)+"]";
            }

            // If we should pull out the argument then do so
            if (do_extract(m, arg_value, binders))
            {
                ptree extracted_value;
                std::swap(arg_value, extracted_value);
                extracted_top.push_back({name, extracted_value});
            }
            // Otherwise look into the argument's value and try to pull things out
            else if (not arg_value.is_null()) // for function[x=null,body=E]
            {
                for(auto& [sub_name,sub_term]: extract_terms(arg_value, binders))
                {
                    auto sup_name = name + "/" + sub_name;
                    // Fuse subscripts like [1] into the name.
                    if (sub_name.size() and sub_name[0] == '[')
                        sup_name = name + sub_name;
                    extracted.emplace_back(sup_name, std::move(sub_term));
                }
            }
        }
        std::move(extracted_top.begin(), extracted_top.end(), std::back_inserter(extracted));
    }

    return extracted;
}

#include "util/text.H"

string pretty_model_t::show_extracted() const
{
    const int indent = 4;

    string output;

    for(int i=0; i<terms.size(); i++)
    {
        string t = string(indent,' ') + term_names[i] + " ";
        string value = terms[i].show(false);
        output += "\n" + t + indent_and_wrap(0, t.size() + 2, 10000, value);
    }
    return output;
}

string pretty_model_t::show_main(bool top) const
{
    if (top)
        return unparse_annotated(main);
    else
        return show_model_annotated(main);
}

string pretty_model_t::show(bool top) const
{
    return show_main(top) + show_extracted();
}

pretty_model_t::pretty_model_t(const ptree& m)
    :main(m)
{
    // 1. Extract terms
    for(auto& [name,term]: extract_terms(main, {}))
    {
        term_names.push_back(name);
        terms.push_back(term);
    }

    term_names = short_parameter_names(term_names);
}
