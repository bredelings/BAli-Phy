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
 * 2. A probability model has a collection of variables, and also a series of densities of the form
 *
 *              (x,y) ~ distribution(a,b,c)
 * 
 * These will be expressed as annotations of the form:
 * 
 *              (~ distribution (x,y) (a,b,c))
 *
 *  + here (x,y) and (a,b,c) are either single elements, or tuples if more than one element.
 *  + here distribution is a constant object.
 *
 * The distribution object currently specifies
 *  + a distribution name
 *  + an expression for the density
 *
 * It will eventually also specify
 *  + a possibly an UNNORMALIZED density.
 *  + a random sample( ): function.
 *   o How do we evaluate random functions?  I guess we could mark them uncacheable.
 *     See I/O monad in Haskell?
 *
 * A variable that is not random is a parameter, and can be changed by optimization routines,
 *    but not MCMC routines.
 * A variable that is random is "random variable" and can be changed by MCMC routines.
 *    The distribution 
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
 */

/*
 * 3. The static-flow evaluation framework allows caching along a DAG, but has a
 * lot of limitations on the kind of expressions it allows.  
 *
 * 1. No data constructors as a head.
 * 2. No eval_match( ).
 * 3. No body functions.
 * 4. Operations may not evaluate to expressions (and so provoke further evaluation).
 * 5. No Functions as function arguments
 *
 * Idea: Regarding #1, We should be able to allow data constructors easily enough.
 *
 * Idea: When evaluating (\x (f x)) (expression),
 *       we could actually assign values to the dummy variables.
 *       (Would we need a stack-frame equivalent, with recursions?)  
 *
 * Idea: Always re-lookup expressions.  This will be general, but slow.
 *       However, it will give us an idea what is going on.
 *      
 */

struct exponential_density: public Operation
{
  exponential_density* clone() const {return new exponential_density;}
  
  boost::shared_ptr<const Object> operator()(OperationArgs& Args) const
  {
    shared_ptr<const Double> x = Args.evaluate_as<Double>(0);
    shared_ptr<const Double> mu = Args.evaluate_as<Double>(1);

    Log_Double result = log_double_t(0);
    if (*x >= 0.0)
      result = exp<log_double_t>(-*x/ *mu)/ *mu;
    return shared_ptr<const Object>(result.clone());
  }

  string name() const {return "exponential_density";}

  exponential_density():Operation(2) { }
};

term_ref add_probability_expression(polymorphic_cow_ptr<Formula>& F)
{
  expression_ref query = distributed_as(prob_density(_,_1),_2,_3);

  typed_expression_ref<Log_Double> Pr;

  // Check each expression in the Formula
  for(int i=0;i<F->size();i++)
  {
    vector<expression_ref> results; 

    // If its a probability expression, then...
    if (find_match(query,(*F)[i],results))
    {
      // Extract the density operation
      shared_ptr<const Operation> density_op = boost::dynamic_pointer_cast<const Operation>(results[0]);
      if (not density_op) throw myexception()<<"Expression "<<i<<" does have an Op in the right place!";

      // Create an expression for calculating the density of these random variables given their inputs
      expression_ref density_func = lambda_expression( *density_op );
      typed_expression_ref<Log_Double> Pr_i = density_func(results[1], results[2]);

      // Extend the probability expression to include this term also.
      // (FIXME: a balanced tree could save computation time)
      if (not Pr)
	Pr = Pr_i;
      else
	Pr = Pr_i * Pr;
    }
  }

  // If this model has random variables... 
  if (Pr)
  {
    F->add_expression(prob(Pr));

    vector<int> results;
    F->find_match_expression2(prob(_1),results);
    return term_ref(results[0],F);
  }
  else
    return term_ref();
}

int main()
{
  Formula f;
  polymorphic_cow_ptr<Formula> F(f);
  //  term_ref x = F->add_state_node("X");
  expression_ref x = parameter("X");
  expression_ref y = parameter("Y");
  expression_ref z = parameter("Z");
  expression_ref w = parameter("W");
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
  expression_ref minusi = lambda_expression( Minus<Int>() );
  expression_ref gt = lambda_expression( GreaterThan<Double>() );

  cout<<"Demonstrate lambda functions\n";
  cout<<"mul = "<<mul<<"\n";
  cout<<"mul(x) = "<<mul(x)<<"\n";
  cout<<"mul(x)(y) = "<<mul(x)(y)<<"\n";
  cout<<"mul(x,y) = "<<mul(x,y)<<"\n\n\n";

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
  F->add_expression( If( Z > 1.0, X*Y+1.0, W*W ) );

  expression_ref square = lambda_expression(body_function("square",1));
  F->add_expression( defun( square(_1), true, mul(_1,_1)) );

  expression_ref fmap = lambda_expression(body_function("fmap",2));
  F->add_expression( defun( fmap(_,ListEnd), true, ListEnd) );
  F->add_expression( defun( fmap(_1,Cons(_2,_3)), true, Cons(_1(_2),fmap(_1,_3) )) );

  expression_ref take = lambda_expression(body_function("take",2));
  F->add_expression( defun( take(_,ListEnd), true, ListEnd) );
  F->add_expression( defun( take(0,_), true, ListEnd) );
  F->add_expression( defun( take(_1,Cons(_2,_3)), true, Cons(_2,take(minusi(_1,1),_3)) ) );

  expression_ref repeat = lambda_expression(body_function("repeat",1));
  F->add_expression( defun( repeat(_1), true, Cons(_1,repeat(_1)) ) );

  expression_ref iterate = lambda_expression(body_function("iterate",2));
  F->add_expression( defun( iterate(_1,_2), true, Cons(_2,iterate(_1,_1(_2))) ) );

  expression_ref print = lambda_expression(body_function("print",1));
  expression_ref print_list = lambda_expression(body_function("print_list",1));

  F->add_expression( defun( print_list(Cons(_1,ListEnd)), true, print(_1) ) );
  F->add_expression( defun( print_list(Cons(_1,_2)), true, concat(print(_1),concat(", ",print_list(_2))) ) );
  F->add_expression( defun( print(Cons(_1,_2)), true, concat("[",concat(print_list(Cons(_1,_2)),"]")) ) );
  F->add_expression( defun( print(_1), true, sys_print(_1) ) );

  term_ref defv = F->add_expression(  default_value(parameter("X"), 2.0) );
  term_ref list_x_y = F->add_expression(Cons(X,Cons(Y,ListEnd)));
  term_ref tuple_x_y = F->add_expression(Tuple(2)(X,Y));
  expression_ref Exp = prob_density("Exp",exponential_density());
  term_ref prior_x_y = F->add_expression(distributed_as(Exp,parameter("X"),Y+One));
  term_ref prior_y_z = F->add_expression(distributed_as(Exp,parameter("Y"),Z+One));
  term_ref probability_expression = add_probability_expression(F);

  Context CTX1(F);

  //  CTX1.set_parameter_value("X",Double(2));
  CTX1.set_parameter_value("Y",Double(3));
  CTX1.set_parameter_value("Z",Double(4));
  CTX1.set_parameter_value("W",Int(5));

  cout<<"CTX1 = \n"<<CTX1<<"\n";

  Context CTX2 = CTX1;

  cout<<"CTX1 = \n"<<CTX1<<"\n";

  shared_ptr<const Object> result = CTX1.evaluate(x_times_y_plus_one);
  CTX1.evaluate(cond);

  cout<<"CTX1 = \n"<<CTX1<<"\n";
  cout<<"CTX2 = \n"<<CTX2<<"\n";
  cout<<"Fiddling X and Y in CTX1...\n";
  CTX1.set_parameter_value("X",Double(3));
  CTX1.set_parameter_value("Y",Double(2));
  cout<<"CTX1 = \n"<<CTX1<<"\n";
  cout<<"CTX2 = \n"<<CTX2<<"\n";

  result = CTX1.evaluate(x_times_y_plus_one);

  cout<<"Fiddling W in CTX2...\n";
  CTX2.set_parameter_value("W",Int(-1));
  cout<<"CTX2 = \n"<<CTX2<<"\n";

  cout<<"Fiddling Z in CTX2...\n";
  CTX2.set_parameter_value("Z",Double(0));
  result = CTX2.evaluate(cond);
  CTX2.evaluate(defv);
  CTX2.evaluate(z_gt);
  CTX2.evaluate(list_x_y);
  CTX2.evaluate(tuple_x_y);
  CTX2.evaluate(prior_x_y);
  CTX2.evaluate(probability_expression);
  cout<<"CTX2 = \n"<<CTX2<<"\n";

  // I guess the current framework could not evaluate X:Y to X:Y.  It would simply return value(X):value(Y).
  // I could introduce a QUOTE expression to prevent this... that sounds rather LISP-y.

  expression_ref pattern = default_value(parameter("X"))(match(0));
  expression_ref target = default_value(parameter("X"))(One);
  vector< expression_ref > results;
  cout<<"Checking if expression "<<target<<" matches query "<<pattern<<":\n";
  if (find_match(pattern, target,results))
  {
    cout<<"match has size "<<results.size()<<"\n";
    if (results.size())
      cout<<"   value = "<<results[0]<<"\n";
  }
  else
    cout<<"no match!";

  results.clear();
  expression_ref test = Tuple(2)(One+One,One+One+One);
  if (eval_match(CTX1,test,Tuple(2)(2.0,3.0),results) )
    cout<<"R = "<<test<<"\n";


  expression_ref test2 = Tuple(2)(square(parameter("X")),One+One);
  results.clear();
  if (eval_match(CTX1, test2,Tuple(2)(9.0, 2.0), results))
    cout<<"R2 = "<<test2<<"\n";

  expression_ref test3 = fmap(square,Cons(parameter("X"),Cons(One+One,ListEnd)));
  cout<<" "<<test3<<" = ";
  test3 = eval(CTX1, test3);
  cout<<test3<<"\n";

  expression_ref test4 = take(3,repeat(X));
  cout<<" "<<test4<<" = ";
  test4 = eval(CTX1, test4);
  cout<<test4<<"\n";

  expression_ref test5 = take(3,fmap(square,repeat(X)));
  cout<<" "<<test5<<" = ";
  test5 = eval(CTX1, test5);
  cout<<test5<<"\n";

  // Here we hit the problem of substituting lambda expressions into lambda expressions: match name collision!
  expression_ref test6 = take(3,fmap(square,iterate(plus(1.0),1.0)));
  cout<<" "<<test6<<" = ";
  test6 = eval(CTX1, test6);
  cout<<test6<<"\n";

  expression_ref test7 = print(take(3,fmap(square,iterate(plus(1.0),1.0))));
  cout<<" "<<test7<<" = ";
  test7 = eval(CTX1, test7);
  cout<<test7<<"\n";
}
