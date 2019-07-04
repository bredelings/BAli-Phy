#include "do_block.H"
#include "util/string/join.H"
#include "computation/expression/lambda.H"
#include "computation/expression/let.H"
#include "computation/expression/tuple.H"

using std::pair;
using std::vector;
using std::string;

expression_ref do_block::get_expression() const
{
    return (*this);
}

do_block& do_block::perform(const expression_ref& E1)
{
    stmts.push_back(E1.print());
    return *this;
}

do_block& do_block::perform(const var& x, const expression_ref& E)
{
    stmts.push_back(x.print() + " <- " + E.print());
    return *this;
}

do_block& do_block::let(const CDecls& decls)
{
    vector<string> print_decls;
    for(auto& [x,e]: decls)
        print_decls.push_back(x.print() + " = " + e.print());
    stmts.push_back("let {" + join(print_decls,';') + "}");
    return *this;
}

do_block& do_block::let(const var& x, const expression_ref& body)
{
    return let({{x,body}});
}

do_block& do_block::rec(const do_block& rec_block)
{
    stmts.push_back("rec {" + join(rec_block.get_stmts(),';') + "}");
    return *this;
}

expression_ref do_block::finish(const expression_ref& E)
{
    stmts.push_back(E.print());
    return get_expression();
}

expression_ref do_block::finish_return(const expression_ref& E)
{
    stmts.push_back("return " + E.print());
    return get_expression();
}

pair<expression_ref, expression_ref> do_block::bind_model(const std::string& prefix, const expression_ref& model)
{
    var pair("pair_arg_" + prefix);
    var x("arg_" + prefix);
    var loggers("log_arg_" + prefix);
    perform(pair,model);               // pair <- smodel
    let({{x,{fst,pair}},               // let x     = fst pair
         {loggers,{snd,pair}}});       //     loggers = snd smodel_pair
    return {x,loggers};
}

expression_ref logger(const string& prefix, const expression_ref& x, const expression_ref& x_loggers, bool do_log)
{
    auto maybe_x = do_log ? expression_ref({ var("Just"), x }) : expression_ref(var("Nothing"));
    return Tuple( String(prefix), Tuple( maybe_x, x_loggers) );
}

expression_ref do_block::bind_and_log_model(const string& prefix, const expression_ref& model, vector<expression_ref>& loggers, bool do_log)
{
    auto [x, x_loggers] = bind_model(prefix,model);
    loggers.push_back( logger(prefix, x, x_loggers, do_log) );
    return x;
}

string do_block::print() const
{
    return "do {" + join(stmts,';') + "}";
}
