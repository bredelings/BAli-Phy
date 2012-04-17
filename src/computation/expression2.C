#include <vector>
#include <string>
#include <iostream>

#include <boost/shared_ptr.hpp>
#include "util.H"

#include "operation.H"
#include "operations.H"
#include "distribution-operations.H"
#include "context.H"
#include "prelude.H"

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

int main()
{
  //  term_ref x = F->add_state_node("X");
  expression_ref x = parameter("X");
  expression_ref y = parameter("Y");
  expression_ref z = parameter("Z");
  expression_ref w = parameter("W");
  expression_ref one = Double(1);

  typed_expression_ref<Double> X ( x );
  typed_expression_ref<Double> Y ( y );
  typed_expression_ref<Int> W ( w );
  typed_expression_ref<Double> Z ( z );
  typed_expression_ref<Double> One(1.0);

  expression_ref mul = lambda_expression( Multiply<Double>() );
  expression_ref muli = lambda_expression( Multiply<Int>() );
  expression_ref plus = lambda_expression( Add<Double>() );
  expression_ref plus_i = lambda_expression( Add<Int>() );
  expression_ref minusi = lambda_expression( Minus<Int>() );
  expression_ref gt = lambda_expression( GreaterThan<Double>() );

  cout<<"Demonstrate lambda functions\n";
  cout<<"mul = "<<mul<<"\n";
  cout<<"mul(x) = "<<mul(x)<<"\n";
  cout<<"mul(x)(y) = "<<mul(x)(y)<<"\n";
  cout<<"mul(#1) = "<<mul(dummy(1))<<"\n";
  cout<<"mul(x,y) = "<<mul(x,y)<<"\n\n\n";

  // This doesn't work!  Therefore, let_float objects out of IF statements doesn't work either.
  cout<<lambda_quantify(dummy("x"),lambda_quantify(dummy("y"),dummy("x")))<<"\n";
  cout<<let_float(lambda_quantify(dummy("x"),lambda_quantify(dummy("y"),dummy("x"))))<<"\n";
  cout<<let_float(let_float(lambda_quantify(dummy("x"),lambda_quantify(dummy("y"),dummy("x")))))<<"\n";

  cout<<lambda_quantify(dummy("x"),5)<<"\n";
  cout<<let_float(lambda_quantify(dummy("x"),5))<<"\n";
  cout<<let_float(let_float(lambda_quantify(dummy("x"),5)))<<"\n";
  expression_ref case_if;
  {
    vector<expression_ref> patterns;
    patterns.push_back(Bool(true));
    patterns.push_back(Bool(false));

    vector<expression_ref> bodies;
    bodies.push_back(dummy(1));
    bodies.push_back(dummy(2));

    case_if = 
      lambda_quantify(dummy(0),
		      lambda_quantify(dummy(1),
				      lambda_quantify(dummy(2),
						      make_case_expression(dummy(0),
									   patterns,
									   bodies)
						      )
				      )
		      );
  }

  expression_ref let_cons = mul(1)(2);
  cout<<let_cons<<"\n";
  cout<<graph_normalize(let_cons)<<"\n";
  cout<<let_float(graph_normalize(let_cons))<<"\n";
  cout<<"graph normalize: "<<graph_normalize(case_if(Z>1.0,X*Y,W))<<"\n";
  cout<<"let_float: "<<let_float(case_if(Z>1.0,X*Y,W))<<"\n";
  cout<<"let_float & graph_normalize: "<<let_float(graph_normalize(case_if(Z>1.0,X*Y,W)))<<"\n";


  cout<<"\nPrelude = \n"<<Prelude<<"\n\n";

  cout<<"Creating an Context...\n";
  context CTX1;

  CTX1.add_parameter("X");
  CTX1.add_parameter("Y");
  CTX1.add_parameter("Z");
  CTX1.add_parameter("W");

  expression_ref x_times_y_plus_one = plus(mul(x)(y))(one);
  int x_times_y_plus_one_ = CTX1.add_compute_expression(x_times_y_plus_one);

  expression_ref z_gt_1 = gt(z)(one);

  int z_gt = CTX1.add_compute_expression(gt(z));

  CTX1.add_compute_expression(plus(x)(y));

  int w_2 = CTX1.add_compute_expression( muli(w)(w) );

  int cond = CTX1.add_compute_expression( If(z_gt_1, x_times_y_plus_one, w_2));

  // this should be a dup and do nothing
  CTX1.add_compute_expression( If( gt(z)(one) ) ( plus( mul(x)(y))(one) ) ( muli(w)(w) ) );
  // -- using multiple arguments instead of one at a time.  This works up to 3 arguments
  CTX1.add_compute_expression( If( gt(z, one) , plus( mul(x, y), one) , muli(w,w) ) );
  // -- using automatic creation of operators based on typed references
  CTX1.add_compute_expression( If( Z > One , X*Y+One , W*W ) );
  // -- can we create constants easily?
  CTX1.add_compute_expression( If( Z > 1.0, X*Y+1.0, W*W ) );

  int defv = CTX1.add_compute_expression(  default_value(parameter("X"), 2.0) );
  int list_x_y = CTX1.add_compute_expression(Cons(X,Cons(Y,ListEnd)));
  int tuple_x_y = CTX1.add_compute_expression(Tuple(X,Y));

  int prior_x_y = CTX1.add_note(distributed(parameter("X"),Tuple(exponential_dist,Y+One)));
  CTX1.add_note(distributed(parameter("Y"),Tuple(exponential_dist,Z+One)));
  int probability_expression = add_probability_expression(CTX1);

  cout<<"Setting a few variables...\n";
  CTX1.set_parameter_value("X",Double(2));
  CTX1.set_parameter_value("Y",Double(3));
  CTX1.set_parameter_value("Z",Double(4));
  CTX1.set_parameter_value("W",Int(5));

  cout<<"X should have 2 as a default value.\n";
  cout<<"CTX1 = \n"<<CTX1<<"\n";

  cout<<"\n\n";
  cout<<"Copying the context: values should be shared between CTX1 and CTX2.\n";
  context CTX2 = CTX1;

  cout<<"CTX1 = \n"<<CTX1<<"\n";

  cout<<"\n\n";
  cout<<"Evaluating expressions in CTX1: results should be visible in CTX2 also.\n";
  shared_ptr<const Object> result = CTX1.evaluate(x_times_y_plus_one_);
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
  result = CTX1.evaluate(x_times_y_plus_one_);

  cout<<"\n\n";
  CTX2.evaluate(cond);
  cout<<"Changing W in CTX2: If(Z>1,X*Y+1,W*W) is unaffected should remain valid.\n";
  CTX2.set_parameter_value("W",Int(-1));
  CTX2.evaluate(cond);
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

  for(int i=0;i<CTX1.n_expressions();i++)
  {
    expression_ref R = CTX1.get_expression(i);
    cout<<i<<"  "<<R->print()<<"\n";

    expression_ref N = launchbury_normalize(R);
    cout<<"     "<<N->print()<<"\n";
  }

  expression_ref v0 = dummy(0);
  expression_ref v1 = dummy(1);
  expression_ref v2 = dummy(2);
  expression_ref v3 = dummy(3);
  expression_ref v4 = dummy(4);

  expression_ref test8 = let_expression(v0,1,v0);
  cout<<"\n";
  cout<<"Eval test:     "<<test8<<" = \n";
  test8 = launchbury_normalize(test8);
  cout<<"   normalized: "<<test8<<" = \n";
  test8 = CTX1.evaluate_expression(test8);
  cout<<test8<<"\n";

  expression_ref test9 = case_expression(Cons(1,Cons(2,ListEnd)), Cons(v1,v2), v2);

  cout<<"\n";
  cout<<"Eval test:     "<<test9<<" = \n";
  test9 = launchbury_normalize(test9);
  cout<<"   normalized: "<<test9<<" = \n";
  test9 = CTX1.evaluate_expression(test9);
  cout<<test9<<"\n";


  expression_ref test10 = take(2)(Cons(1,Cons(2,Cons(3,ListEnd))));

  cout<<"\n";
  cout<<"Eval test:     "<<test10<<" = \n";
  test10 = launchbury_normalize(test10);
  cout<<"   normalized: "<<test10<<" = \n";
  test10 = CTX1.evaluate_expression(test10);
  cout<<test10<<"\n";

  // We might actually have to print the result to calculate the whole thing.
  expression_ref test11 = take(3)(iterate(plus_i(1),1));
  cout<<"\n";
  cout<<"Eval test:     "<<test11<<" = \n";
  test11 = launchbury_normalize(test11);
  cout<<"   normalized: "<<test11<<" = \n";
  test11 = CTX1.evaluate_expression(test11);
  cout<<test11<<"\n";

  context C;


  expression_ref myArray = var("myArray");
  expression_ref square = var("square");
  expression_ref myFib = var("myFib");
  {
    Program P;
    P += Def( (square,v1),(typed_expression_ref<Int>(v1)*v1) );

    P += Def( (myFib, 0), 1 )
            ( (myFib, 1), 1 )
            ( (myFib,v1), typed_expression_ref<Int>((getIndex,myArray,v1-2)) + (getIndex,myArray,v1-1) );

    P += Def( myArray, (mkArray, 10, myFib) );

    C += P;
  }

  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  cout<<C.evaluate_expression(6)<<"\n";

  expression_ref test12 = apply_expression(apply_expression(plus,One),One);

  cout<<"\n";
  cout<<"Eval test:     "<<test12<<" = \n";
  test12 = launchbury_normalize(test12);
  cout<<"   normalized: "<<test12<<" = \n";
  test12 = C.evaluate_expression(test12);
  cout<<test12<<"\n";
  test12.reset();
  cout<<"A:C.n_regs() = "<<C.n_regs()<<"\n";

  expression_ref test13 = case_expression(Cons(1,Cons(2,ListEnd)), Cons(v1,v2), v2);

  cout<<"\n";
  cout<<"Eval test:     "<<test13<<" = \n";
  test13 = launchbury_normalize(test13);
  cout<<"   normalized: "<<test13<<" = \n";
  test13 = C.evaluate_expression(test13);
  cout<<test13<<"\n";
  test13.reset();
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";

  expression_ref test14 = take(3)(iterate(plus_i(1),1));

  cout<<"\n";
  cout<<"Eval test:     "<<test14<<" = \n";
  test14 = launchbury_normalize(test14);
  cout<<"   normalized: "<<test14<<" = \n";
  test14 = C.evaluate_expression(test14);
  cout<<test14<<"\n";
  test14.reset();
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";


  //print(take(3,fmap(square,iterate(plus(1.0),1.0))));
  expression_ref test15 = take(3)(iterate(plus_i(1),1));

  cout<<"\n";
  cout<<"Eval test:     "<<test15<<" = \n";
  test15 = launchbury_normalize(test15);
  cout<<"   normalized: "<<test15<<" = \n";
  test15 = C.evaluate_expression(test15);
  cout<<test15<<"\n";
  test15.reset();
  cout<<"C.n_used_regs() = "<<C.n_used_regs()<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";

  cout<<"add parameters X,Y,Z\n";
  C.add_parameter("X");
  C.add_parameter("Y");
  C.add_parameter("Z");
  cout<<"C.n_used_regs() = "<<C.n_used_regs()<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  C.add_compute_expression((X+Y)+Z);
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  C.set_parameter_value("X",1.0);
  C.set_parameter_value("Y",2.0);
  C.set_parameter_value("Z",4.0);
  cout<<"B:C.n_regs() = "<<C.n_regs()<<"\n";
  cout<<"C.evaluate(0) = "<<C.evaluate(0)<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  cout<<"C.evaluate(0) = "<<C.evaluate(0)<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";

  {
    C.set_parameter_value("Z",5.0);
    context D = C;
    cout<<"D.evaluate(0) = "<<D.evaluate(0)<<"\n";
    cout<<"C.evaluate(0) = "<<C.evaluate(0)<<"\n";
    D.set_parameter_value("Z",6.0);
    cout<<"D.evaluate(0) = "<<D.evaluate(0)<<"\n";
    cout<<"C.evaluate(0) = "<<C.evaluate(0)<<"\n";
    C.set_parameter_value("Z",7.0);
    D.set_parameter_value("Z",8.0);
    cout<<"D.evaluate(0) = "<<D.evaluate(0)<<"\n";
    cout<<"C.evaluate(0) = "<<C.evaluate(0)<<"\n";
    D.set_parameter_value("X",0.0);
    cout<<"D.evaluate(0) = "<<D.evaluate(0)<<"\n";
    cout<<"C.evaluate(0) = "<<C.evaluate(0)<<"\n";
  }
  C.add_compute_expression( apply_expression(apply_expression(plus,apply_expression(apply_expression(plus,X),Y)),Z) );
  cout<<"C.evaluate(1) = "<<C.evaluate(1)<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  C.set_parameter_value("Z",7.0);
  cout<<"C.evaluate(1) = "<<C.evaluate(1)<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";

  C.set_parameter_value("X",1);
  C.set_parameter_value("Y",2);
  C.set_parameter_value("Z",4);
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";

  expression_ref test16 = take(y)(iterate(plus_i(x),z));

  C.add_compute_expression( test16 );
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  cout<<"C.evaluate(2) = "<<C.evaluate(2)<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  C.set_parameter_value("Y",0);
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  cout<<"C.evaluate(2) = "<<C.evaluate(2)<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";

  expression_ref test17 =  sum_i(take(y)(iterate(plus_i(x),z)));

  C.add_compute_expression( test17 );
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  cout<<"C.evaluate(3) = "<<C.evaluate(3)<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  C.set_parameter_value("Y",2);
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  cout<<"C.evaluate(3) = "<<C.evaluate(3)<<"\n";
  C.set_parameter_value("Y",1);
  cout<<"C.evaluate(3) = "<<C.evaluate(3)<<"\n";
  C.set_parameter_value("Y",3);
  cout<<"C.evaluate(3) = "<<C.evaluate(3)<<"\n";
  C.set_parameter_value("X",2);
  cout<<"C.evaluate(3) = "<<C.evaluate(3)<<"\n";
  C.set_parameter_value("Z",3);
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  cout<<"C.evaluate(3) = "<<C.evaluate(3)<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  cout<<"set Y=0\n";

  // Apparently we are generating new garbage each time we run this.
  C.set_parameter_value("Y",5);
  cout<<"C.evaluate(3) = "<<C.evaluate(3)<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  C.set_parameter_value("Y",5);
  cout<<"C.evaluate(3) = "<<C.evaluate(3)<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";
  C.set_parameter_value("Y",5);
  cout<<"C.evaluate(3) = "<<C.evaluate(3)<<"\n";
  cout<<"C.n_regs() = "<<C.n_regs()<<"\n";

  C.set_parameter_value("Y",5);
  context D = C;
  cout<<"D.evaluate(3) = "<<D.evaluate(3)<<"\n";
  cout<<"C.evaluate(3) = "<<C.evaluate(3)<<"\n";
  D.set_parameter_value("Y",4);
  cout<<"D.evaluate(3) = "<<D.evaluate(3)<<"\n";
  cout<<"C.evaluate(3) = "<<C.evaluate(3)<<"\n";


  expression_ref test18 = (getIndex,myArray,9);
  cout<<"Evaluating "<<test18<<std::endl;
  cout<<"  result = "<<C.evaluate_expression( test18 )<<std::endl;
  cout<<"  result = "<<C.evaluate_expression( test18 )<<std::endl;
  cout<<"  result = "<<C.evaluate_expression( (bounds,myArray) )<<std::endl;
  cout<<"  result = "<<C.evaluate_expression( myArray )<<std::endl;
  cout<<"  result = "<<C.evaluate_expression( myArray )<<std::endl;
}
