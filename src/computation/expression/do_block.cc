#include "do_block.H"
#include "computation/expression/lambda.H"
#include "computation/expression/let.H"
#include "computation/expression/tuple.H"

using std::pair;
using std::vector;
using std::string;

expression_ref do_block::get_expression() const {
    return code({});
}

do_block& do_block::perform(const expression_ref& E1)
{
    auto new_code = [code=code,E1](const expression_ref& E2) 
                        {
                            return code({var("Compiler.Base.>>"),E1,E2});
                        };
    code = new_code;
    return *this;
}

do_block& do_block::perform(const var& x, const expression_ref& E1)
{
    auto new_code = [code=code,x,E1](const expression_ref& E2)
                        {
                            return code({var("Compiler.Base.>>="),E1,lambda_quantify(x,E2)});
                        };
    code = new_code;
    return *this;
}

do_block& do_block::let(const CDecls& decls)
{
    auto new_code = [code=code,decls](const expression_ref& E)
                        {
                            return code(let_expression(decls,E));
                        };
    code = new_code;
    return *this;
}

do_block& do_block::let(const var& x, const expression_ref& body)
{
    return let({{x,body}});
}

expression_ref do_block::finish(const expression_ref& E1)
{
    auto new_code = [code=code,E1](const expression_ref&)
                        {
                            return code(E1);
                        };
    code = new_code;
    return get_expression();
}

expression_ref do_block::finish_return(const expression_ref& E)
{
    return finish({var("Compiler.Base.return"),E});
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
    auto maybe_x = do_log ? expression_ref({ var("Data.Maybe.Just"), x }) : expression_ref(var("Data.Maybe.Nothing"));
    return Tuple( prefix, Tuple( maybe_x, x_loggers) );
}

expression_ref do_block::bind_and_log_model(const string& prefix, const expression_ref& model, vector<expression_ref>& loggers, bool do_log)
{
    auto [x, x_loggers] = bind_model(prefix,model);
    loggers.push_back( logger(prefix, x, x_loggers, do_log) );
    return x;
}
