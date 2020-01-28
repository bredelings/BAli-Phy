#ifndef DO_BLOCK_H
#define DO_BLOCK_H
#include "computation/expression/expression_ref.H"
#include "computation/expression/var.H"
#include "computation/expression/let.H"


#include <functional>
#include <utility>

struct Stmt: public Object
{
    Stmt* clone() const {return new Stmt(*this);}
};

struct Decl: public Object
{
    expression_ref bindpat;
    expression_ref rhs;

    Decl* clone() const {return new Decl(*this);}
    std::string print() const;
    Decl(const expression_ref& b, const expression_ref& r);
};

struct Binds: public Box<std::vector<Decl>>
{
    Binds* clone() const {return new Binds(*this);}
    std::string print() const;
    Binds() = default;
    Binds(const std::initializer_list<Decl>& decls);
};


struct EmptyStmt: public Stmt
{
    EmptyStmt* clone() const {return new EmptyStmt(*this);}
    std::string print() const;
};

struct PatQual: public Stmt
{
    expression_ref bindpat;
    expression_ref exp;

    PatQual* clone() const {return new PatQual(*this);}
    std::string print() const;
    PatQual(const expression_ref& b, const expression_ref& e);
};

struct SimpleQual: public Stmt
{
    expression_ref exp;

    SimpleQual* clone() const {return new SimpleQual(*this);}
    std::string print() const;

    SimpleQual(const expression_ref& e);
};

struct LetQual: public Stmt
{
    Binds binds;

    LetQual* clone() const {return new LetQual(*this);}
    std::string print() const;

    LetQual(const Binds& b);
};

struct Stmts: public Box<std::vector<expression_ref>>
{
    Stmts* clone() const {return new Stmts(*this);}

    std::string print() const;
};

struct Rec: public Stmt
{
    Stmts stmts;

    Rec* clone() const {return new Rec(*this);}

    std::string print() const;

    Rec(const Stmts&);
};

// A Qual
// A stmt is either a Rec or a Qual
// Probably we should make a variant, instead of a base class.

class do_block: public Object
{
    Stmts stmts;

public:
    do_block* clone() const {return new do_block(*this);}

    std::string print() const;

    int size() const {return stmts.size();}

    const Stmts& get_stmts() const { return stmts; }

    expression_ref get_expression() const;

    do_block& perform(const expression_ref& E1);

    do_block& perform(const expression_ref& pattern, const expression_ref& E1);

    do_block& let(const Binds&);

    do_block& let(const expression_ref& pattern, const expression_ref& E);

    do_block& rec(const do_block& rec_block);

    do_block& empty_stmt();

    expression_ref finish(const expression_ref& E1);

    expression_ref finish_return(const expression_ref& E);

    std::pair<expression_ref, expression_ref> bind_model(const std::string& prefix, const expression_ref& model);

    expression_ref bind_and_log_model(const std::string& prefix, const expression_ref& model, std::vector<expression_ref>& loggers, bool do_log = true);
};

class list_comprehension: public Object
{
    expression_ref body;
    std::vector<expression_ref> quals;
public:
    list_comprehension* clone() const {return new list_comprehension(*this);}

    std::string print() const;
    list_comprehension(const expression_ref& b, const std::vector<expression_ref>& qs);
};

expression_ref logger(const std::string& prefix, const expression_ref& x, const expression_ref& x_loggers, bool do_log = true);

#endif
