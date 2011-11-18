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
#include "distribution-operations.H"
#include "graph_register.H"

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
 *              (~ (x,y) (distribution (a,b,c)))
 *
 *  + Here (x,y) and (a,b,c) are either single elements, or tuples if more than one element.
 *  + Here distribution is a constant object that is currently of the form (prob_density "Exp", exponential_density)
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

term_ref add_probability_expression(polymorphic_cow_ptr<Formula>& F)
{
  expression_ref query = distributed(_2,Tuple(prob_density(_,_1),_3));

  typed_expression_ref<Log_Double> Pr;

  // Check each expression in the Formula
  for(int i=0;i<F->size();i++)
  {
    vector<expression_ref> results; 

    // If its a probability expression, then...
    if (find_match(query,(*F)[i],results))
    {
      // Extract the density operation
      shared_ptr<const Operation> density_op = dynamic_pointer_cast<Operation>(results[0]);
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
  cout<<"mul(#1) = "<<mul(dummy(1))<<"\n";
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

  term_ref defv = F->add_expression(  default_value(parameter("X"), 2.0) );
  term_ref list_x_y = F->add_expression(Cons(X,Cons(Y,ListEnd)));
  term_ref tuple_x_y = F->add_expression(Tuple(X,Y));

  term_ref prior_x_y = F->add_expression(distributed(parameter("X"),Tuple(exponential_dist,Y+One)));
  term_ref prior_y_z = F->add_expression(distributed(parameter("Y"),Tuple(exponential_dist,Z+One)));
  term_ref probability_expression = add_probability_expression(F);

  cout<<"Creating an Context...\n";
  Context CTX1(F);

  cout<<"Setting a few variables...\n";
  //  CTX1.set_parameter_value("X",Double(2));
  CTX1.set_parameter_value("Y",Double(3));
  CTX1.set_parameter_value("Z",Double(4));
  CTX1.set_parameter_value("W",Int(5));

  cout<<"X should have 2 as a default value.\n";
  cout<<"CTX1 = \n"<<CTX1<<"\n";

  cout<<"\n\n";
  cout<<"Copying the context: values should be shared between CTX1 and CTX2.\n";
  Context CTX2 = CTX1;

  cout<<"CTX1 = \n"<<CTX1<<"\n";

  cout<<"\n\n";
  cout<<"Evaluating expressions in CTX1: results should be visible in CTX2 also.\n";
  shared_ptr<const Object> result = CTX1.evaluate(x_times_y_plus_one);
  CTX1.evaluate(cond);
  CTX1.evaluate(defv);
  CTX1.evaluate(z_gt);
  CTX1.evaluate(list_x_y);
  CTX1.evaluate(tuple_x_y);
  CTX1.evaluate(prior_x_y);
  CTX1.evaluate(probability_expression);
  cout<<"CTX1 = \n"<<CTX1<<"\n";
  cout<<"CTX2 = \n"<<CTX2<<"\n";

  cout<<"\n\n";
  cout<<"Changing X and Y from 2,3 to 3,2 in CTX1: downstream computations should be invalidated in CTX1.\n";
  CTX1.set_parameter_value("X",Double(3));
  CTX1.set_parameter_value("Y",Double(2));
  cout<<"CTX1 = \n"<<CTX1<<"\n";
  cout<<"CTX2 = \n"<<CTX2<<"\n";

  cout<<"\n\n";
  cout<<"Evaluating X*Y+1.0 in CTX1: since X*Y is unchanged, old computation should be re-used.\n";
  result = CTX1.evaluate(x_times_y_plus_one);

  cout<<"\n\n";
  cout<<"Changing W in CTX2: If(Z>1,X*Y+1,W*W) is unaffected should remain valid.\n";
  CTX2.set_parameter_value("W",Int(-1));
  cout<<"CTX2 = \n"<<CTX2<<"\n";

  cout<<"\n\n";
  cout<<"Changing Z in CTX2 and evaluating If(Z>1,X*Y+1,W*W):\n";
  CTX2.set_parameter_value("Z",Double(0));
  result = CTX2.evaluate(cond);
  cout<<"CTX2 = \n"<<CTX2<<"\n";

  cout<<"\n\n";
  cout<<"Non-cached higher-order function framework:\n";
  // I guess the current framework could not evaluate X:Y to X:Y.  It would simply return value(X):value(Y).
  // I could introduce a QUOTE expression to prevent this... that sounds rather LISP-y.

  cout<<" Adding function 'square' to CTX1.\n";
  expression_ref square = lambda_expression(body_function("square",1));
  CTX1.add_expression( defun( square(_1), true, mul(_1,_1)) );

  cout<<" Adding function 'fmap' to CTX1.\n";
  expression_ref fmap = lambda_expression(body_function("fmap",2));
  CTX1.add_expression( defun( fmap(_,ListEnd), true, ListEnd) );
  CTX1.add_expression( defun( fmap(_1,Cons(_2,_3)), true, Cons(_1(_2),fmap(_1,_3) )) );

  cout<<" Adding function 'take' to CTX1.\n";
  expression_ref take = lambda_expression(body_function("take",2));
  CTX1.add_expression( defun( take(_,ListEnd), true, ListEnd) );
  CTX1.add_expression( defun( take(0,_), true, ListEnd) );
  CTX1.add_expression( defun( take(_1,Cons(_2,_3)), true, Cons(_2,take(minusi(_1,1),_3)) ) );

  cout<<" Adding function 'repeat' to CTX1.\n";
  expression_ref repeat = lambda_expression(body_function("repeat",1));
  CTX1.add_expression( defun( repeat(_1), true, Cons(_1,repeat(_1)) ) );

  cout<<" Adding function 'iterate' to CTX1.\n";
  expression_ref iterate = lambda_expression(body_function("iterate",2));
  CTX1.add_expression( defun( iterate(_1,_2), true, Cons(_2,iterate(_1,_1(_2))) ) );

  cout<<" Adding function 'print' to CTX1.\n";
  cout<<" Adding function 'print_list' to CTX1.\n";
  expression_ref print = lambda_expression(body_function("print",1));
  expression_ref print_list = lambda_expression(body_function("print_list",1));

  CTX1.add_expression( defun( print_list(Cons(_1,ListEnd)), true, print(_1) ) );
  CTX1.add_expression( defun( print_list(Cons(_1,_2)), true, concat(print(_1),concat(", ",print_list(_2))) ) );
  CTX1.add_expression( defun( print(Cons(_1,_2)), true, concat("[",concat(print_list(Cons(_1,_2)),"]")) ) );
  CTX1.add_expression( defun( print(_1), true, sys_print(_1) ) );

  expression_ref v0 = dummy(0);
  expression_ref v1 = dummy(1);
  expression_ref v2 = dummy(2);
  expression_ref v3 = dummy(3);
  expression_ref v4 = dummy(4);

  CTX1.add_expression( case_expression(false, dummy(0), Cons(dummy(1),ListEnd), dummy(1)) );
  CTX1.add_expression( case_expression(true, dummy(0), Cons(dummy(1),ListEnd), dummy(1)) );
  CTX1.add_expression( def_function(false, Tuple(v0, Cons(v1, v2)), Cons(v1, v3((typed_expression_ref<Double>(v0)-1.0),v2))) );
  CTX1.add_expression( def_function(true, Tuple(v0, Cons(v1, v2)), Cons(v1, v3((typed_expression_ref<Double>(v0)-1.0),v2))) );

  cout<<"\n CTX1 now contains this list of non-sub expressions:\n";
  cout<<*CTX1.F<<"\n";

  for(int i=0;i<CTX1.F->n_exp();i++)
  {
    expression_ref R = CTX1.F->exp(i);
    cout<<i<<"  "<<R->print()<<"\n";

    expression_ref N = launchbury_normalize(R);
    cout<<"     "<<N->print()<<"\n";
  }


  cout<<"\n\n";
  cout<<"Match test:\n";
  expression_ref pattern = default_value(parameter("X"))(match(0));
  expression_ref target = default_value(parameter("X"))(One);
  vector< expression_ref > results;
  cout<<"Checking if expression "<<target<<" matches query "<<pattern<<":\n";
  if (find_match(pattern, target,results))
  {
    cout<<"match has size "<<results.size()<<"\n";
    if (results.size())
      cout<<"   _0 = "<<results[0]<<"\n";
  }
  else
    cout<<"no match!";

  expression_ref test = Tuple(One+One,One+One+One);
  cout<<"\n";
  cout<<"Eval test: does "<<test<<" match "<<Tuple(2.0,3.0)<<"?\n";
  results.clear();
  if (eval_match(CTX1,test,Tuple(2.0,3.0),results) )
    cout<<"yes, result = "<<test<<"\n";


  expression_ref test2 = Tuple(square(parameter("X")),One+One);
  cout<<"\n";
  cout<<"Eval test: does "<<test2<<" match "<<Tuple(9.0, 2.0)<<"?\n";
  results.clear();
  if (eval_match(CTX1, test2,Tuple(9.0, 2.0), results))
    cout<<"yes, result = "<<test2<<"\n";

  expression_ref test3 = fmap(square,Cons(parameter("X"),Cons(One+One,ListEnd)));
  cout<<"\n";
  cout<<"Eval test: "<<test3<<" = ";
  test3 = eval(CTX1, test3);
  cout<<test3<<"\n";

  expression_ref test4 = take(3,repeat(X));
  cout<<"\n";
  cout<<"Eval test: "<<test4<<" = ";
  test4 = eval(CTX1, test4);
  cout<<test4<<"\n";

  expression_ref test5 = take(3,fmap(square,repeat(X)));
  cout<<"\n";
  cout<<"Eval test: "<<test5<<" = ";
  test5 = eval(CTX1, test5);
  cout<<test5<<"\n";

  // Here we hit the problem of substituting lambda expressions into lambda expressions: match name collision!
  expression_ref test6 = take(3,fmap(square,iterate(plus(1.0),1.0)));
  cout<<"\n";
  cout<<"Eval test: "<<test6<<" = ";
  test6 = eval(CTX1, test6);
  cout<<test6<<"\n";

  expression_ref test7 = print(take(3,fmap(square,iterate(plus(1.0),1.0))));
  cout<<"\n";
  cout<<"Eval test: "<<test7<<" = ";
  test7 = eval(CTX1, test7);
  cout<<test7<<"\n";

  // take - actually, this requires handling operators.
  expression_ref def_take;
  {  
    take = dummy("take");

    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    patterns.push_back( Tuple(0, v1) );
    bodies.push_back( ListEnd );

    patterns.push_back( Tuple(v1, ListEnd) );
    bodies.push_back( ListEnd );

    typed_expression_ref<Int> I1 = v1;
    patterns.push_back( Tuple(v1, Cons(v2,v3) ) );
    bodies.push_back( Cons(v2, take(I1-1)(v3) ) );

    // FIXME - get rid of def_function.
    //       - start using a fixpoint function definition.
    // take 0  _  = []
    // take _  [] = []
    // take n h:t = h:(take (n-1) t)

    //       - lambda take.n.x.case n of {0->[],_->case x of {[] -> [],h:t->h:(take (n-3) t)}}
    def_take = def_function(true, patterns, bodies);

    CTX1.add_expression( def_function(false, patterns, bodies) );
    CTX1.add_expression( def_take );
  }

  // iterate
  expression_ref def_iterate;
  {  
    iterate = dummy("iterate");

    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    patterns.push_back( Tuple(v1, v2) );
    bodies.push_back( Cons(v2, iterate(v1)(v1(v2))) );

    def_iterate = def_function(true, patterns, bodies);
  }

  // fmap
  expression_ref def_fmap;
  {  
    fmap = dummy("fmap");

    vector<expression_ref> patterns;
    vector<expression_ref> bodies;
    patterns.push_back( Tuple(v1, ListEnd) );
    bodies.push_back( ListEnd );

    patterns.push_back( Tuple(v1, Cons(v2,v3)) );
    bodies.push_back( Cons(v1(v2), fmap(v2)(v3) ) );

    def_fmap = def_function(true, patterns, bodies);
  }

  expression_ref test8 = let_expression(v0,1,v0);
  cout<<"\n";
  cout<<"Eval test:     "<<test8<<" = \n";
  test8 = launchbury_normalize(test8);
  cout<<"   normalized: "<<test8<<" = \n";
  test8 = evaluate_mark1(test8);
  cout<<test8<<"\n";

  expression_ref test9 = case_expression(true, Cons(1,Cons(2,ListEnd)), Cons(v1,v2), v2);

  cout<<"\n";
  cout<<"Eval test:     "<<test9<<" = \n";
  test9 = launchbury_normalize(test9);
  cout<<"   normalized: "<<test9<<" = \n";
  test9 = evaluate_mark1(test9);
  cout<<test9<<"\n";


  expression_ref test10 = let_expression(take, def_take,
					 take(2)(Cons(1,Cons(2,Cons(3,ListEnd))))
					);
  cout<<"\n";
  cout<<"Eval test:     "<<test10<<" = \n";
  test10 = launchbury_normalize(test10);
  cout<<"   normalized: "<<test10<<" = \n";
  test10 = evaluate_mark1(test10);
  cout<<test10<<"\n";

  // We might actually have to print the result to calculate the whole thing.
  expression_ref test11 = let_expression(take, def_take,
					let_expression(iterate, def_iterate,
						       take(3)(iterate(plus(1),1))
						       )
					);
  cout<<"\n";
  cout<<"Eval test:     "<<test11<<" = \n";
  test11 = launchbury_normalize(test11);
  cout<<"   normalized: "<<test11<<" = \n";
  test11 = evaluate_mark1(test11);
  cout<<test11<<"\n";

  context C;
  cout<<incremental_evaluate(C,6)<<"\n";

  expression_ref test12 = apply_expression(apply_expression(plus,One),One);

  cout<<"\n";
  cout<<"Eval test:     "<<test12<<" = \n";
  test12 = launchbury_normalize(test12);
  cout<<"   normalized: "<<test12<<" = \n";
  test12 = incremental_evaluate(C,test12);
  cout<<test12<<"\n";

  expression_ref test13 = case_expression(true, Cons(1,Cons(2,ListEnd)), Cons(v1,v2), v2);

  cout<<"\n";
  cout<<"Eval test:     "<<test13<<" = \n";
  test13 = launchbury_normalize(test13);
  cout<<"   normalized: "<<test13<<" = \n";
  test13 = incremental_evaluate(C,test13);
  cout<<test13<<"\n";

  expression_ref test14 = let_expression(take, def_take,
					let_expression(iterate, def_iterate,
						       take(3)(iterate(plus(1),1))
						       )
					);

  cout<<"\n";
  cout<<"Eval test:     "<<test14<<" = \n";
  test14 = launchbury_normalize(test14);
  cout<<"   normalized: "<<test14<<" = \n";
  test14 = incremental_evaluate(C,test14);
  cout<<test14<<"\n";


  //print(take(3,fmap(square,iterate(plus(1.0),1.0))));
  expression_ref test15 = let_expression(take, def_take,
					let_expression(iterate, def_iterate,
						       take(3)(iterate(plus(1),1))
						       )
					);

  cout<<"\n";
  cout<<"Eval test:     "<<test15<<" = \n";
  test15 = launchbury_normalize(test15);
  cout<<"   normalized: "<<test15<<" = \n";
  test15 = incremental_evaluate(C,test15);
  cout<<test15<<"\n";

  C.add_parameter("X");
  C.add_parameter("Y");
  C.add_parameter("Z");
  C.add_expression((X+Y)+Z);
  C.set_parameter_value("X",1.0);
  C.set_parameter_value("Y",2.0);
  C.set_parameter_value("Z",4.0);
  cout<<"C.evaluate(0) = "<<C.evaluate(0)<<"\n";
  cout<<"C.evaluate(0) = "<<C.evaluate(0)<<"\n";

  C.add_expression( apply_expression(apply_expression(plus,apply_expression(apply_expression(plus,X),Y)),Z) );
  cout<<"C.evaluate(1) = "<<C.evaluate(1)<<"\n";
  C.set_parameter_value("Z",7.0);
  cout<<"C.evaluate(1) = "<<C.evaluate(1)<<"\n";
}
