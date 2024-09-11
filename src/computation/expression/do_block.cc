#include "do_block.H"
#include "util/string/join.H"
#include "computation/expression/lambda.H"
#include "computation/expression/let.H"
#include "computation/expression/tuple.H"

using std::pair;
using std::vector;
using std::string;

std::string Decl::print() const
{
    return bindpat.print() + " = " + rhs.print();
}

std::string Binds::print() const
{
    vector<string> bind_string;
    for(auto& decl: *this)
        bind_string.push_back(decl.print());
    return "{"+join(bind_string,"\n;")+"}";
}

Binds::Binds(const std::initializer_list<Decl>& decls)
    :Box<vector<Decl>>(decls)
{ }

std::string EmptyStmt::print() const {return "";}

std::string PatQual::print() const
{
    return bindpat.print() + " <- " + exp.print();
}

std::string SimpleQual::print() const
{
    return exp.print();
}

std::string LetQual::print() const
{
    return "let " + binds.print();
}

std::string Stmts::print() const
{
    vector<string> stmt_string;
    for(auto& stmt: *this)
        stmt_string.push_back(stmt.print());
    return "{"+join(stmt_string,"\n;")+"\n}";
}

Stmts& Stmts::perform(const expression_ref& E1)
{
    push_back(SimpleQual(E1));
    return *this;
}

Stmts& Stmts::perform(const expression_ref& pattern, const expression_ref& E)
{
    push_back(PatQual(pattern,E));
    return *this;
}

Stmts& Stmts::let(const Binds& binds)
{
    push_back(LetQual(binds));
    return *this;
}

Stmts& Stmts::let(expression_ref pattern, expression_ref body, bool lambda_to_function)
{
    if (lambda_to_function)
    {
	assert(pattern.is_a<var>());
	while(is_lambda_exp(body))
	{
	    pattern = {pattern, body.sub()[0]};
	    body = body.sub()[1];
	}
    }

    Decl decl{pattern,body};
    return let(Binds({decl}));
}

Stmts& Stmts::rec(const do_block& block)
{
    push_back(Rec(block.get_stmts()));
    return *this;
}

Stmts& Stmts::empty_stmt()
{
    push_back(EmptyStmt());
    return *this;
}

string Rec::print() const
{
    return "rec " + stmts.print();
}

string do_block::print() const
{
    return "do " + stmts.print();
}

expression_ref do_block::get_expression() const
{
    return (*this);
}

expression_ref do_block::finish(const expression_ref& E)
{
    perform(E);
    return get_expression();
}

expression_ref do_block::finish_return(const expression_ref& E)
{
    return finish({var("return"),E});
}

Decl::Decl(const expression_ref& b, const expression_ref& r):bindpat(b),rhs(r) {}

SimpleQual::SimpleQual(const expression_ref& e):exp(e) {}

PatQual::PatQual(const expression_ref& b, const expression_ref& e):bindpat(b),exp(e) {}

LetQual::LetQual(const Binds& b):binds(b) {}

Rec::Rec(const Stmts& s):stmts(s) {}

do_block::do_block(const Stmts& s): stmts(s) {}

pair<expression_ref, expression_ref> do_block::bind_model(const std::string& prefix, const expression_ref& model)
{
    var x(prefix);
    if (prefix.empty() or not std::islower(prefix[0]))
        x = var("_"+prefix);
    var loggers("log_" + prefix);
    perform(Tuple(x,loggers),model);        // (x,loggers) <- model
    return {x,loggers};
}

expression_ref logger(const string& prefix, const expression_ref& x, const expression_ref& x_loggers, bool do_log)
{
    bool has_subloggers = not x_loggers or not (x_loggers.is_a<constructor>());
    if (do_log and not has_subloggers)
        return expression_ref({var("%=%"),String(prefix),x});
    else if (has_subloggers and not do_log)
        return expression_ref({var("%>%"),String(prefix),x_loggers});
    else if (has_subloggers and do_log)
        return expression_ref({var("%=>%"),String(prefix),Tuple(x,x_loggers)});
    else
        return {}; // we shouldn't be making a logger
}

expression_ref do_block::bind_and_log_model(const string& prefix, const expression_ref& model, vector<expression_ref>& loggers, bool do_log)
{
    auto [x, x_loggers] = bind_model(prefix,model);
    if (auto l = logger(prefix, x, x_loggers, do_log) )
        loggers.push_back(l);
    return x;
}

string list_comprehension::print() const
{
    vector<string> qual_strings;
    for(auto& qual: quals)
        qual_strings.push_back(qual.print());
    return "[" + body.print() + " | " + join(qual_strings,", ") + "]";
}

list_comprehension::list_comprehension(const expression_ref& b, const vector<expression_ref>& qs)
    :body(b), quals(qs)
{
}
