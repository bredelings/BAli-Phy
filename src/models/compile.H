#ifndef MODELS_COMPILE_H
#define MODELS_COMPILE_H
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
#include "models/typecheck.H"
#include "models/code-generation.H"

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

    bool empty() const {return description.empty();}
    pretty_model_t pretty_model() const {return {description};};

    std::string show(bool = true) const;

    std::string show_pretty(bool = true) const;
    std::string show_main(bool = true) const;
    std::string show_extracted() const;

    model_t() = default;
    model_t(const ptree&,const std::set<std::string>&, const ptree&, const std::set<term_t>&, const generated_code_t&);
};

model_t compile_model(const Rules& R,
		      const TypecheckingState& TC, CodeGenState code_gen_state,
		      ptree required_type,
		      const std::string& model,
		      const std::string& what,
		      const std::vector<std::pair<std::string,ptree>>& = {},
		      const std::map<std::string,std::pair<std::string,ptree>>& = {}
    );

model_t compile_decls(const Rules& R,
		      TypecheckingState& TC, CodeGenState& code_gen_state,
		      const std::string& decls,
		      const std::vector<std::pair<std::string,ptree>>& scope,
		      const std::map<std::string,std::pair<std::string,ptree>>& state);

json::object convert_to_json(const pretty_model_t& m);

TypecheckingState makeTypechecker(const Rules& R,
				  const std::vector<std::pair<std::string,ptree>>& scope,
				  const std::map<std::string,std::pair<std::string,ptree>>& state);

std::string print_equals_function(expression_ref E);

std::string default_markov_model(const alphabet& a);
#endif
