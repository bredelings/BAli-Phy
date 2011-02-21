#include <vector>
#include <string>
#include <iostream>

#include <boost/shared_ptr.hpp>
#include "util.H"

#include "formula.H"
#include "context.H"
#include "computation.H"
#include "operation.H"
#include "operations.H"

using boost::shared_ptr;
using std::vector;
using std::string;
using std::ostream;

int main()
{
  Formula f;
  polymorphic_cow_ptr<Formula> F(f);
  term_ref x = F->add_state_node("X");
  term_ref y = F->add_state_node("Y");
  term_ref z = F->add_state_node("Z");
  term_ref w = F->add_state_node("W");
  term_ref one = F->add_constant_node(Double(1));

  typed_expression_ref<Double> X = x;
  typed_expression_ref<Double> Y = y;
  typed_expression_ref<Int> W = w;
  typed_expression_ref<Double> Z = z;
  typed_expression_ref<Double> One = one;

  F->add_constant_node(Double(1));

  expression_ref mul = Multiply<Double>();
  expression_ref muli = Multiply<Int>();
  expression_ref plus = Add<Double>();
  expression_ref gt = GreaterThan<Double>();
  expression_ref If = IfThenElse();

  std::cout<<"Demonstrate lambda functions\n";
  std::cout<<"mul = "<<mul->print()<<"\n";
  std::cout<<"mul(x) = "<<mul(x)->print()<<"\n";
  std::cout<<"mul(x)(y) = "<<mul(x)(y)->print()<<"\n";
  std::cout<<"mul(x,y) = "<<mul(x,y)->print()<<"\n\n\n";

  term_ref x_times_y_plus_one = F->add_computed_node( plus(mul(x)(y))(one) );

  term_ref z_gt_1 = F->add_computed_node(gt(z)(one));

  term_ref x_plus_y = F->add_computed_node(plus(x)(y));

  term_ref w_2 = F->add_computed_node( muli(w)(w) );

  term_ref cond = F->add_computed_node( If(z_gt_1, x_times_y_plus_one, w_2));

  // this should be a dup and do nothing
  F->add_computed_node( If( gt(z)(one) ) ( plus( mul(x)(y))(one) ) ( muli(w)(w) ) );
  // -- using multiple arguments instead of one at a time.  This works up to 3 arguments
  F->add_computed_node( If( gt(z, one) , plus( mul(x, y), one) , muli(w,w) ) );
  // -- using automatic creation of operators based on typed references
  F->add_computed_node( If( Z > One , X*Y+One , W*W ) );

  Context CTX1(F);

  CTX1.set_value(x,Double(2));
  CTX1.set_value(y,Double(3));
  CTX1.set_value(z,Double(4));
  CTX1.set_value(w,Int(5));

  std::cout<<"CTX1 = \n"<<CTX1<<"\n";

  Context CTX2 = CTX1;

  std::cout<<"CTX1 = \n"<<CTX1<<"\n";

  shared_ptr<const Object> result = CTX1.evaluate(x_times_y_plus_one);
  CTX1.evaluate(cond);

  std::cout<<"CTX1 = \n"<<CTX1<<"\n";
  std::cout<<"CTX2 = \n"<<CTX2<<"\n";
  std::cout<<"Fiddling X and Y in CTX1...\n";
  CTX1.set_value(x,Double(3));
  CTX1.set_value(y,Double(2));
  std::cout<<"CTX1 = \n"<<CTX1<<"\n";
  std::cout<<"CTX2 = \n"<<CTX2<<"\n";

  result = CTX1.evaluate(x_times_y_plus_one);

  std::cout<<"Fiddling W in CTX2...\n";
  CTX2.set_value(w,Int(-1));
  std::cout<<"CTX2 = \n"<<CTX2<<"\n";

  std::cout<<"Fiddling Z in CTX2...\n";
  CTX2.set_value(z,Double(0));
  result = CTX2.evaluate(cond);
  std::cout<<"CTX2 = \n"<<CTX2<<"\n";
}
