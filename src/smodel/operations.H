#ifndef SMODEL_OPERATIONS_H
#define SMODEL_OPERATIONS_H

#include <vector>
#include <valarray>
#include "sequence/alphabet.H"
#include "smodel/objects.H"
#include "object.H"
#include "probability/probability.H"
#include "models/model.H"
#include "computation/formula_expression.H"
#include "computation/expression.H"

namespace substitution {

  formula_expression_ref F1x4_Model(const Triplets& T);

  formula_expression_ref F3x4_Model(const Triplets& T);

  formula_expression_ref MG94_Model(const Triplets& T);

  formula_expression_ref MG94w9_Model(const Triplets& T);

  formula_expression_ref EQU_Model(const alphabet& a);
  
  object_ptr<SymmetricMatrixObject> SingletToTripletExchangeFunction(const Triplets& T, const SymmetricMatrixObject& S2);

  formula_expression_ref Plus_F_Model(const alphabet& a, const std::valarray<double>& pi);

  formula_expression_ref Plus_F_Model(const alphabet& a);

  formula_expression_ref Plus_gwF_Model(const alphabet& a, const std::valarray<double>& pi);

  formula_expression_ref Plus_gwF_Model(const alphabet& a);

  formula_expression_ref Reversible_Markov_Model(const formula_expression_ref& S, const formula_expression_ref& R);

  formula_expression_ref Unit_Model(const formula_expression_ref&);

  formula_expression_ref Mixture_Model(const std::vector<formula_expression_ref>& models);
}

// PROBLEMS!
// - To do FunctionOp<"PAM",PAM_Exchange_Function>(), we'd have to allow templates that take
//   generic function pointers.  (That is, function pointers whose type isn't known a priori.)
//
// - Converting Args_t... to 0, 1, ... n_args-1 is not supported by the language.  We have to
//   perform various hacks to accomplish this.
//
// - Probably I could to GenericFunctionOp("PAM","PAM_Exchange_Function"), but that would be using
//   run-time lookups to call the function pointer.
//


// http://stackoverflow.com/questions/7858817/unpacking-a-tuple-to-call-a-matching-function-pointer
// http://stackoverflow.com/questions/10254289/apply-function-from-stdtuple
// http://stackoverflow.com/questions/4763736/vs2010-c-variadic-template-example
// Ask question: "Template Template parameters in function pointer template arguments"
//  -> See suggestions.

/*

template<int ...>
struct seq { };

template<int N, int ...S>
struct gens : gens<N-1, N-1, S...> { };

template<int ...S>
struct gens<0, S...> {
  typedef seq<S...> type;
};

template <const char* s, template <typename R_t, typename ...Args_t> R_t F(Args_t)>
class FunctionOp
{
  template<int... Indices>
  object_ptr<const Object> call_func(OperationArgs& Args) const
  {
    // the issue here is that we need to convert Args_t = type0, type1, type2 to 0, 1, 2, ...
    // it should be possible to make a parameter pack of integers..
    return F( convert<const Args_t>(Args.evaluate(Indices))... );
  }

public:
  FunctionOp* clone() const {return new Function_Op(*this);}

  tribool compare(const Object& O) const
  {
    if (this == &O) 
      return true;
    
    if (typeid(*this) != typeid(O)) return false;
    
    return true;
  }

  object_ptr<const Object> operator()(OperationArgs& Args) const
  {
    return call_func<gens<sizeof...(Args_t)>::type >(Args);
  }
 
  std::string name() const {return s;}
  
  Function_Op(): Operation(sizeof...(Args_t)) { }
};

expression_ref PAM = lambda_expresion( FunctionOp<"PAM",PAM_Exchange_Function>() );
*/
#endif
