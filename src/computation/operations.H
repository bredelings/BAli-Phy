#ifndef OPERATIONS_H
#define OPERATIONS_H

#include <vector>
#include <string>
#include "myexception.H"
#include "operation.H"
#include "computation.H"
#include "expression.H"


// In general, these operations simply ask for the value of each input
// from the OperationArgs object.  They therefore need have no knowledge of
// how these values are arrived at.

struct Apply: public Operation
{
  virtual Apply* clone() const {return new Apply(*this);}

  tribool compare(const Object& o) const 
  {
    const Apply* E = dynamic_cast<const Apply*>(&o);
    return E;
  }

  int precedence() const {return 10;}

  assoc_type associativity() const {return assoc_left;}

  closure operator()(OperationArgs& Args) const;

  std::string name() const;

  Apply():Operation(-1) {}
};

struct Case: public Operation
{
  virtual Case* clone() const {return new Case(*this);}

  tribool compare(const Object& o) const 
  {
    const Case* E = dynamic_cast<const Case*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const;

  int type() const {return case_type;}

  Case():Operation(2) {}
};

struct Let: public Operation
{
  virtual Let* clone() const {return new Let(*this);}

  tribool compare(const Object& o) const 
  {
    const Let* E = dynamic_cast<const Let*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const;

  std::string name() const;

  int type() const {return let2_type;}

  Let():Operation(2) {}
};

#endif
