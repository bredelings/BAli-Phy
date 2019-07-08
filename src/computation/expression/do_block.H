#ifndef DO_BLOCK_H
#define DO_BLOCK_H
#include "computation/expression/expression_ref.H"
#include "computation/expression/var.H"
#include "computation/expression/let.H"


#include <functional>
#include <utility>

class do_block: public Object
{
    std::vector<std::string> stmts;
public:
    do_block* clone() const {return new do_block(*this);}

    std::string print() const;

    expression_ref get_expression() const;

    do_block& perform(const expression_ref& E1);

    do_block& perform(const var& x, const expression_ref& E1);

    do_block& let(const CDecls& decls);

    do_block& let(const var&x, const expression_ref& body);

    expression_ref finish(const expression_ref& E1);

    expression_ref finish_return(const expression_ref& E);

    std::pair<expression_ref, expression_ref> bind_model(const std::string& prefix, const expression_ref& model);

    expression_ref bind_and_log_model(const std::string& prefix, const expression_ref& model, std::vector<expression_ref>& loggers, bool do_log = true);
};

expression_ref logger(const std::string& prefix, const expression_ref& x, const expression_ref& x_loggers, bool do_log = true);

#endif
