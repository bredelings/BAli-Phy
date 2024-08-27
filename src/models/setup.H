#ifndef SMODEL_SETUP_H
#define SMODEL_SETUP_H
#include <set>                                      // for set
#include <string>                                   // for string
#include <utility>                                  // for pair
#include <vector>                                   // for vector
#include "computation/expression/expression_ref.H"  // for expression_ref
#include "computation/expression/do_block.H"        // for do_block
#include "sequence/alphabet.H"                      // for alphabet
#include "unification.H"                            // for term_t, less
#include "util/json.hh"                             // for json
#include "util/ptree.H"                             // for ptree
class Rules;

class pretty_model_t
{
public:
    ptree main;
    std::vector<std::string> term_names;
    std::vector<pretty_model_t> terms;

    std::string show(bool = true) const;
    std::string show_main(bool = true) const;
    std::string show_extracted() const;

    pretty_model_t(const ptree& m);
};

struct LoggerObj
{
    virtual std::string get_name() const = 0;
    virtual LoggerObj* clone() const = 0;
    virtual ~LoggerObj() {};
};

typedef owned_ptr<LoggerObj> Logger;

typedef std::vector<Logger> Loggers;

struct LogValue: public LoggerObj
{
    std::string name;
    expression_ref value;

    LogValue* clone() const {return new LogValue(*this);}

    std::string get_name() const {return name;}

    LogValue(const std::string& s, const expression_ref& e)
        :name(s),value(e)
    {}
};

struct LogSub: public LoggerObj
{
    std::string prefix;
    var log_var; // we might emit logger_var = loggers, and then do prefix %>% logger_var
    Loggers loggers;

    LogSub* clone() const {return new LogSub(*this);}

    std::string get_name() const {return prefix+"/";}

    LogSub(const std::string& s, const var& v, const Loggers& ls)
        :prefix(s), log_var(v), loggers(ls)
    {}
};

expression_ref generate_loggers(do_block& code, const Loggers& Loggers);

struct generated_code_t
{
    std::vector<var> lambda_vars;

    Stmts stmts;
    expression_ref E;
    Loggers loggers;
    bool perform_function = false;

    // Which states were actually used.
    std::set<std::string> used_states;

    // Under these circumstances, we will return a monadic action
    bool is_action() const {return perform_function or not stmts.empty();}
    bool has_loggers() const {return not loggers.empty();}

    expression_ref generate() const;

    void log_value(const std::string&, expression_ref, const ptree&);
    void log_sub(const std::string&, const var&, const Loggers&);

    std::string print() const {return generate().print();}
};

void perform_action_simplified(Stmts& block, const var& x, const var& log_x, bool is_referenced, expression_ref E, bool is_action, bool has_loggers);
void perform_action_simplified(generated_code_t& block, const var& x, const var& log_x, bool is_referenced, const generated_code_t& code);

void maybe_log(std::vector<expression_ref>& logger_bits,
               const std::string& name,
               const expression_ref& value,
               const expression_ref& subloggers);

class model_t
{
    ptree description;
public:
    std::set<std::string> imports;
    ptree type;
    std::set<term_t> constraints;
    generated_code_t code;
    std::set<std::string> used_args;

    pretty_model_t pretty_model() const {return {description};};

    std::string show(bool = true) const;

    std::string show_pretty(bool = true) const;
    std::string show_main(bool = true) const;
    std::string show_extracted() const;

    model_t() = default;
    model_t(const ptree&,const std::set<std::string>&, const ptree&, const std::set<term_t>&, const generated_code_t&);
};


json::object convert_to_json(const pretty_model_t& m);

model_t get_model(const Rules& R, const std::string& type, const std::string& model, const std::string& what,
                  const std::vector<std::pair<std::string,std::string>>& = {},
                  const std::map<std::string,std::pair<std::string,std::string>>& = {}
    );

void compile_defs(const Rules& R,
		  const std::string& prog,
		  const std::vector<std::pair<std::string,std::string>>& scope,
		  const std::map<std::string,std::pair<std::string,std::string>>& state);

std::string print_equals_function(expression_ref E);

std::string default_markov_model(const alphabet& a);
#endif
