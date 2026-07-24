#ifndef CODE_GEN_H
#define CODE_GEN_H

#include <string>                                   // for string
#include <map>                                      // for map
#include <set>                                      // for set
#include <vector>                                   // for vector
#include <optional>                                 // for optional
#include <utility>                                  // for pair
#include "models/typecheck.H"
#include "computation/haskell/haskell.H"            // for Hs::Stmts, Hs::Var

#include "logger.H"

class Rules;

struct generated_code_t
{
    Hs::Stmts stmts;
    std::vector<std::pair<Hs::Var, Hs::Exp>> decls;

    // We prefix by these lambda vars at the end?
    std::vector<Hs::Var> haskell_lambda_vars;
    Hs::Exp E;

    Loggers loggers;
    bool perform_function = false;

    // Which states were actually used.
    std::set<std::string> used_states;
    std::map<std::string,Hs::Var> free_vars;

    // Under these circumstances, we will return a monadic action
    bool is_action() const {return perform_function or not stmts.stmts.empty();}
    bool has_loggers() const {return not loggers.empty();}
    bool has_parameter_loggers() const {return ::has_loggers(loggers, LogValueKind::parameter);}
    bool has_context_loggers() const {return ::has_loggers(loggers, LogValueKind::context);}

    Hs::Exp generate() const;

    void log_value(const std::string&, Hs::Exp, const type_t&, LogValueKind = LogValueKind::parameter);
    void log_context_fields(const std::string&, const std::string&, Hs::Exp);
    void log_sub(const std::string&, const Hs::Var&, const Loggers&);

    Hs::Exp add_arguments(const Hs::Exp& E, const std::map<std::string,Hs::Exp>& state_values) const;

    std::string print() const {return generate().print();}
};

struct var_info_t
{
    Hs::Var x;
    bool is_random = false;
    bool depends_on_lambda = false;
    var_info_t(const Hs::Var& v, bool r=false, bool l=false)
        :x(v),is_random(r),depends_on_lambda(l)
    { }
};

struct arg_env_t
{
    std::string func; // context: calling func:arg
    std::string arg;
    std::map<std::string,Hs::Exp> code_for_arg;

    arg_env_t(const std::string& f, const std::string& a, const std::map<std::string,Hs::Exp>& c):
        func(f),
        arg(a),
        code_for_arg(c)
    { };
};

struct translation_result_t
{
    generated_code_t code;
    std::set<std::string> imports;
    std::set<std::string> lambda_vars;
    std::set<Hs::Var> haskell_vars;
};

struct CodeGenState
{
    const Rules* R;
    std::map<std::string,var_info_t> identifiers;
    std::set<Hs::Var> haskell_vars;
    std::optional<arg_env_t> arg_env;
    std::map<std::string, Hs::Var> state;

    bool is_random(const CM::TypedExpr&) const;
    bool is_unlogged_random(const CM::TypedExpr& model) const;

    Hs::Var get_var(std::string name)
    {
        if (name.empty() or not std::islower(name[0]))
            name = "_"+name;
        Hs::Var x(name);
        for(int i=2; haskell_vars.count(x); i++)
            x = Hs::Var(name+"_"+std::to_string(i));
        haskell_vars.insert(x);
        return x;
    }

    void set_state(const std::string& name, const Hs::Var& x)
    {
        assert(haskell_vars.count(x));
        if (state.count(name))
            state.erase(name);
        state.insert({name,x});
    }

    CodeGenState extend_scope(const std::string& var, const var_info_t& var_info) const;
    CodeGenState& extend_modify_scope(const std::string& var, const var_info_t& var_info);

    translation_result_t get_model_decls(const CM::TypedDecls& decls);
    translation_result_t get_typed_variable_model(const CM::TypedExpr& model) const;
    translation_result_t get_typed_model_state(const CM::GetState& get_state) const;
    translation_result_t get_typed_model_list(const CM::List<CM::Ann>& list) const;
    translation_result_t get_typed_model_tuple(const CM::Tuple<CM::Ann>& tuple) const;
    translation_result_t get_typed_model_let(const CM::Let<CM::Ann>& let) const;
    translation_result_t get_typed_model_lambda(const CM::Lambda<CM::Ann>& lambda) const;
    translation_result_t get_typed_variable_call(const CM::Call<CM::Ann>& call) const;
    translation_result_t get_typed_rule_call(const CM::Call<CM::Ann>& call) const;
    translation_result_t get_typed_model_call(const CM::Call<CM::Ann>& call) const;
    translation_result_t get_typed_model_sample(const CM::Sample<CM::Ann>& sample) const;
    translation_result_t get_model_as(const CM::TypedExpr& model_rep) const;

    CodeGenState(const Rules& r):R(&r) {}
};

Hs::Exp simplify_intToDouble(const Hs::Exp& E);

#endif
