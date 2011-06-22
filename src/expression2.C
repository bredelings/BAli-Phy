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

using std::cout;
using std::cerr;
using std::endl;

/* 1. To do a series of operations on a parameter, obtained a shared_ptr to the value and mark it uncomputed.
 * We think of this as obtaining a 'lock' on the value.  Any attempts to obtain the variable's
 *   value will then throw an exception.
 * After we are finished modifying the value, we can mark it up-to-date again.
 * We think of that as unlocking the value.
 */

/*
 * 2. A probability model has a collection of variables, and also
 * (i)  a series of annotations (x,y) ~ distribution(a,b,c)
 * (ii) we can represent this as (~ (x,y) distribution (a,b,c)) where (x,y)
 *  + here (x,y) and (a,b,c) are tuples.
 *  + here distribution is a constant object.
 * (iii) the distribution object will specify
 *  + expressions for the density
 *  + member functions for the density
 *  + a distribution name
 *  + a possibly an UNNORMALIZED density.
 *  + a random sample( ): function.
 *   o How do we evaluate random functions?  I guess we could mark them uncacheable.  See I/O monad in Haskell?
 */


/*
 * 3. A probability model is a collection of variables, where each variable has a 
 *    (i) name
 *    (ii) integer index
 *    (iii) a distribution/dependency-structure, if and only if the variable is random.
 *      - The dependency structure is as x ~ x,y|z,w
 *      - The distribution specifies what the actual density is.
 *    A variable that is not random is a parameter, and can be changed by optimization routines,
 *      but not MCMC routines.
 *    A variable that is random is "random variable" and can be changed by MCMC routines.
 *    The distribution 
 *
 *    Each distribution has
 *    (i) a name
 *    (ii) a density function
 *    (iii) an un-normalized density function
 *    (iv) a sampling function.
 *    Perhaps each distribution should also specify the dimension of each random variable that
 *    is sampled from it (not parameters), so that we know if it is a density or not.  0 would
 *    indicate a discrete variable, while 1 and higher would mean that it was continuous.
 *
 *    Q: How shall we sample densities like x,y|z,w?
 *
 *    ------------
 *    Issue. There could be multiple factorizations of the same joint distribution.  Thus,
 *    we could have Pr(x,y) = Pr(x)*Pr(y|x) or Pr(y)*Pr(x|y).
 * 
 *    Different parts of the graph could, independently, allow multiple alternate factorizations.
 *    Then, specifying the relationship between these factorizations should be difficult.
 *
 *    Finally, it might be possible to augment, or unaugment, various parts of the model.
 *    ------------
 *
 *    Anyway, this approach allows distribution to be C++ objects.  They are outside the calculation
 *    framework, so that's OK.  On the other hand, they need to provide calculations that fit inside
 *    that framework as member functions.  Perhaps the density functions can ALSO provide normal C++
 *    density functions a member functions, also?
 *
 *    Implementation: A probability model is independent of specific values for its variables
 *    Therefore it is independent from any Context.  However, a probability model should be able
 *    to create a context.  So, perhaps a probability model allows creating a "probability context".
 *
 *    Hmm... mightn't we need to modify the probability model from the context?
 */

int main()
{
  Formula f;
  polymorphic_cow_ptr<Formula> F(f);
  //  term_ref x = F->add_state_node("X");
  expression_ref x("X");
  expression_ref y("Y");
  expression_ref z("Z");
  expression_ref w("W");
  term_ref one = F->add_expression(Double(1));

  typed_expression_ref<Double> X = x;
  typed_expression_ref<Double> Y = y;
  typed_expression_ref<Int> W = w;
  typed_expression_ref<Double> Z = z;
  typed_expression_ref<Double> One(1.0);

  F->add_expression(Double(1));

  expression_ref mul = lambda_expression( Multiply<Double>() );
  expression_ref muli = lambda_expression( Multiply<Int>() );
  expression_ref plus = lambda_expression( Add<Double>() );
  expression_ref gt = lambda_expression( GreaterThan<Double>() );
  expression_ref If = lambda_expression( IfThenElse() );

  cout<<"Demonstrate lambda functions\n";
  cout<<"mul = "<<mul->print()<<"\n";
  cout<<"mul(x) = "<<mul(x)->print()<<"\n";
  cout<<"mul(x)(y) = "<<mul(x)(y)->print()<<"\n";
  cout<<"mul(x,y) = "<<mul(x,y)->print()<<"\n\n\n";

  term_ref x_times_y_plus_one = F->add_expression( plus(mul(x)(y))(one) );

  term_ref z_gt_1 = F->add_expression(gt(z)(one));

  term_ref z_gt = F->add_expression(gt(z));

  term_ref x_plus_y = F->add_expression(plus(x)(y));

  term_ref w_2 = F->add_expression( muli(w)(w) );

  term_ref cond = F->add_expression( If(z_gt_1, x_times_y_plus_one, w_2));

  // this should be a dup and do nothing
  F->add_expression( If( gt(z)(one) ) ( plus( mul(x)(y))(one) ) ( muli(w)(w) ) );
  // -- using multiple arguments instead of one at a time.  This works up to 3 arguments
  F->add_expression( If( gt(z, one) , plus( mul(x, y), one) , muli(w,w) ) );
  // -- using automatic creation of operators based on typed references
  F->add_expression( If( Z > One , X*Y+One , W*W ) );
  // -- can we create constants easily?
  F->add_expression( If( Z > Constant(Double(1.0)), X*Y+Constant(Double(1)), W*W ) );

  expression_ref default_value = lambda_expression(data_function("default_value",2));
  term_ref defv = F->add_expression(  default_value("X")(Constant(Double(2.0))) );
  term_ref list_x_y = F->add_expression(Cons(X,Cons(Y,ListEnd)));
  term_ref tuple_x_y = F->add_expression(Tuple(2)(X,Y));

  Context CTX1(F);

  //  CTX1.set_value("X",Double(2));
  CTX1.set_value("Y",Double(3));
  CTX1.set_value("Z",Double(4));
  CTX1.set_value("W",Int(5));

  cout<<"CTX1 = \n"<<CTX1<<"\n";

  Context CTX2 = CTX1;

  cout<<"CTX1 = \n"<<CTX1<<"\n";

  shared_ptr<const Object> result = CTX1.evaluate(x_times_y_plus_one);
  CTX1.evaluate(cond);

  cout<<"CTX1 = \n"<<CTX1<<"\n";
  cout<<"CTX2 = \n"<<CTX2<<"\n";
  cout<<"Fiddling X and Y in CTX1...\n";
  CTX1.set_value("X",Double(3));
  CTX1.set_value("Y",Double(2));
  cout<<"CTX1 = \n"<<CTX1<<"\n";
  cout<<"CTX2 = \n"<<CTX2<<"\n";

  result = CTX1.evaluate(x_times_y_plus_one);

  cout<<"Fiddling W in CTX2...\n";
  CTX2.set_value("W",Int(-1));
  cout<<"CTX2 = \n"<<CTX2<<"\n";

  cout<<"Fiddling Z in CTX2...\n";
  CTX2.set_value("Z",Double(0));
  result = CTX2.evaluate(cond);
  CTX2.evaluate(defv);
  CTX2.evaluate(z_gt);
  CTX2.evaluate(list_x_y);
  CTX2.evaluate(tuple_x_y);
  cout<<"CTX2 = \n"<<CTX2<<"\n";

  // I guess the current framework could not evaluate X:Y to X:Y.  It would simply return value(X):value(Y).
  // I could introduce a QUOTE expression... that sounds rather LISP-y.

  expression_ref pattern = default_value("X")(match(0));
  expression_ref target = default_value("X")(One);
  vector< expression_ref > results;
  cout<<"Checking if expression "<<target->print()<<" matches query "<<pattern->print()<<":\n";
  if (find_match(pattern, target,results))
  {
    cout<<"match has size "<<results.size()<<"\n";
    if (results.size())
      cout<<"   value = "<<results[0]->print()<<"\n";
  }
  else
    cout<<"no match!";
}
