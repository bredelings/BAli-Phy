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

  Case():Operation(2) {}
};

template<typename T, typename U>
struct Vector_From_List_Op: public Operation
{
  virtual Vector_From_List_Op* clone() const {return new Vector_From_List_Op(*this);}

  tribool compare(const Object& o) const 
  {
    const Vector_From_List_Op* E = dynamic_cast<const Vector_From_List_Op<T,U>*>(&o);
    return E;
  }

  closure operator()(OperationArgs& Args) const
  {
    object_ptr<Box<std::vector<T> > > v (new Box<std::vector<T> >);

    const closure* top = &Args.evaluate_slot_to_closure(0);
    while(top->exp->size())
    {
      assert(is_exactly(top->exp,":"));
      assert(top->exp->size() == 2);

      int element_index = assert_is_a<index_var>(top->exp->sub[0])->index;
      int element_reg = top->lookup_in_env( element_index );

      int next_index = assert_is_a<index_var>(top->exp->sub[1])->index;
      int next_reg = top->lookup_in_env( next_index );

      // Add the element to the list.
      v->push_back( *convert<const U>(Args.evaluate_reg_to_object(element_reg)) );
      // Move to the next element or end
      top = &Args.evaluate_reg_to_closure(next_reg);
    }
    assert(is_exactly(top->exp,"[]"));

    return v;
  }

  std::string name() const {return "Vector_From_List";}
  
  Vector_From_List_Op():Operation(1) {}
};

template <typename T,typename U>
expression_ref Vector_From_List()
{
  return lambda_expression(Vector_From_List_Op<T,U>());
}

#endif
