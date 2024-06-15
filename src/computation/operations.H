#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <vector>
#include <string>
#include "util/myexception.H"
#include "operation.H"
#include "expression/expression_ref.H"

class OperationArgs;

// In general, these operations simply ask for the value of each input
// from the OperationArgs object.  They therefore need have no knowledge of
// how these values are arrived at.

closure apply_op(OperationArgs&);

struct Apply: public Operation
{
    virtual Apply* clone() const {return new Apply(*this);}

    int precedence() const {return 10;}

    type_constant type() const {return apply_type;}

    Apply():Operation(apply_op,"@") {}
};

closure case_op(OperationArgs&);

struct Case: public Operation
{
    virtual Case* clone() const {return new Case(*this);}

    type_constant type() const {return case_type;}

    Case():Operation(case_op, "case") {}
};

closure let_op(OperationArgs&);

struct Let: public Operation
{
    virtual Let* clone() const {return new Let(*this);}

    type_constant type() const {return let2_type;}

    std::string print() const;

    std::vector<expression_ref> binds;

    expression_ref body;

    Let(const std::vector<expression_ref>& bs, const expression_ref& b)
        :Operation(let_op, "let"),
         binds(bs),
         body(b)
        {}
};

closure seq_op(OperationArgs&);

struct Seq: public Operation
{
    virtual Seq* clone() const {return new Seq(*this);}

    type_constant type() const {return seq_type;}

    Seq():Operation(seq_op, "seq") {}
};

bool is_seq(const expression_ref& E);

closure join_op(OperationArgs&);

struct Join: public Operation
{
    virtual Join* clone() const {return new Join(*this);}

    type_constant type() const {return join_type;}

    Join():Operation(join_op, "join") {}
};


bool is_join(const expression_ref& E);
#endif
