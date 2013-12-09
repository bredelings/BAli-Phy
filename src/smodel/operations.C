#include <sstream>
#include "smodel/operations.H"
#include "computation/operations.H"
#include "computation/expression.H"
#include "io.H"

using boost::shared_ptr;
using std::vector;
using std::valarray;
using std::string;
using boost::shared_ptr;
using boost::dynamic_pointer_cast;
using std::istringstream;
using std::istream;

namespace substitution
{
  formula_expression_ref EQU_Model(const alphabet& a)
  {
    return (identifier("SModel.equ"), a.size());
  }
  
  formula_expression_ref Frequencies_Model(const alphabet& a, const valarray<double>& pi)
  {
    formula_expression_ref F = List();
    for(int i=a.size()-1; i>=0; i--)
    {
      string pname = string("pi") + a.letter(i);
      formula_expression_ref Var  = def_parameter(pname, pi[i], between(0,1));
      F = Var&F;
    }

    F.add_expression( constructor(":~",2)+ F.exp() + (identifier("dirichlet'"),Tuple((int)a.size(), 1.0) ));

    return F;
  }

  formula_expression_ref Frequencies_Model(const alphabet& a)
  {
    valarray<double> pi (1.0/a.size(), a.size());
    return Frequencies_Model(a, pi);
  }

  // Improvement: make all the variables ALSO be a formula_expression_ref, containing their own bounds, etc.
  formula_expression_ref Plus_F_Model(const alphabet& a, const valarray<double>& pi0)
  {
    formula_expression_ref pi = Frequencies_Model(a,pi0);

    return let_expression(v1,(identifier("listToVectorDouble"),pi),
			  (identifier("ReversibleFrequency"), a, (identifier("iotaUnsigned"), a.size()), v1, (identifier("plusGWF"), a, 1.0, v1))
			  );
  }

  formula_expression_ref Plus_F_Model(const alphabet& a)
  {
    valarray<double> pi (1.0/a.size(), a.size());
    return Plus_Model(a,pi);
  }

  // FIXME** - Below I first coded something where 
  //
  //             F1x4 -> Muse&Gaut  (MG94)    if nuc R matrix is +F
  //             F3x4 -> Muse&Gautw9 (MG94w9)  if nuc R matrices are all +F.
  //
  //           This raises the question about whether the codon +F model can be modified to have
  //           60 degrees of freedom like the Codon +F model, while still retaining the nice properties of the
  //           MG94 models.  Can we do something where some codon positions have high nuc frequencies because
  //           of conservation (low f) and some have high nuc frequencies becase of mutation pressuve (high f)?
  //
  //           One way of doing this would be to try and make a completely general codon model with
  //              R[ijk -> ijl] = R_nuc[k->l] * R_aa[aa(ijk)->aa(ijl)] * R_codon_bias[ijk -> ijl]
  //           This raises the question about what equilibrium codon frequencies would result from such a
  //           matrix (when combined with S to yield Q = R ** S).
  //
  //           Also, would it be possible to put this in the context of 2Ns for all of the different values?
  //           And, could they be independently estimated? (i.e. are the identifiable?)
  //            (a) from frequencies alone?
  //            (b) from a pair of aligned sequences? (i.e. from counts of changes)
  //           Finally, how do these models relate to the +gwF model?
  //
  //           The challenge would be to make a generic way to fix what is currently called F3x4_Matrix to take
  //           3 R matrices (one for each codon position) and combine them in some kind of most-general way.
  //
  //           A1: We can replace pi[ijk] with pi[ijk]/pi[ij*] in the +gwF formulation.  This has the benefical
  //               property of NOT claiming that mutations between two infrequent codons happen infrequently
  //               relative to changes between two frequent codons.  The resulting matrix is then:
  //
  //                 R[ijk->ijl] = pi[ijl]^f / (pi[ijk]^(1-f)) * pi[ij*]^(1-2f)
  //
  //               Clearly this is the same as the straight-forward +gwF model with 1-2f=0 and f=1/2.
  //
  //           A2: For a general way to combine three R matrices, we can consider both:
  //
  //               * MG94: select the matrix for the codon position that changed.
  //               * Yang: multiply the R matrices.
  //
  //               The MG94 way seems better -- that is, it matches nucleotide models without setting
  //               f=1/2.
  //

  // Improvement: make all the variables ALSO be a formula_expression_ref, containing their own bounds, etc.
  formula_expression_ref F1x4_Model(const Triplets& T)
  {
    const Nucleotides& N = T.getNucleotides();
    formula_expression_ref pi = Frequencies_Model(N);
    pi = prefix_formula("F1x4", pi);

    return let(v2,(identifier("listToVectorDouble"),pi),
	       v1,(identifier("SModel.f3x4_frequencies"),T,v2,v2,v2),
	       (identifier("ReversibleFrequency"), T, (identifier("iotaUnsigned"), T.size()), v1, (identifier("SModel.plus_gwF"), T, 1.0, v1))
	       );
  }

  // Improvement: make all the variables ALSO be a formula_expression_ref, containing their own bounds, etc.
  formula_expression_ref MG94_Model(const Triplets& T)
  {
    const Nucleotides& N = T.getNucleotides();
    formula_expression_ref pi = Frequencies_Model(N);
    pi = prefix_formula("MG94", pi);

    return let(v2,(identifier("listToVectorDouble"),pi),
	       v1,(identifier("SModel.f3x4_frequencies"),T,v2,v2,v2),
	       v3,(identifier("SModel.plus_gwF"), N, 1.0, v2),
	       (identifier("ReversibleFrequency"), T, (identifier("iotaUnsigned"), T.size()), v1, (identifier("SModel.muse_gaut_matrix"), T, v3, v3, v3))
	       );
  }

  formula_expression_ref F3x4_Model(const Triplets& T)
  {
    const Nucleotides& N = T.getNucleotides();
    formula_expression_ref pi1 = Frequencies_Model(N);
    pi1 = prefix_formula("Site1",pi1);
    formula_expression_ref pi2 = Frequencies_Model(N);
    pi2 = prefix_formula("Site2",pi2);
    formula_expression_ref pi3 = Frequencies_Model(N);
    pi3 = prefix_formula("Site3",pi3);

    return let(v1, (identifier("listToVectorDouble"),pi1),
	       v2, (identifier("listToVectorDouble"),pi2),
	       v3, (identifier("listToVectorDouble"),pi3),
	       v4, (identifier("SModel.f3x4_frequencies"),T,v1,v2,v3),
	       (identifier("ReversibleFrequency"), T, (identifier("iotaUnsigned"), T.size()), v4, (identifier("SModel.plus_gwF"), T, 1.0, v4))
	       );
  }

  formula_expression_ref MG94w9_Model(const Triplets& T)
  {
    const Nucleotides& N = T.getNucleotides();
    formula_expression_ref pi1 = Frequencies_Model(N);
    pi1 = prefix_formula("Site1",pi1);
    formula_expression_ref pi2 = Frequencies_Model(N);
    pi2 = prefix_formula("Site2",pi2);
    formula_expression_ref pi3 = Frequencies_Model(N);
    pi3 = prefix_formula("Site3",pi3);

    return let(v1, (identifier("listToVectorDouble"),pi1),
	       v2, (identifier("listToVectorDouble"),pi2),
	       v3, (identifier("listToVectorDouble"),pi3),
	       v4, (identifier("SModel.plus_gwF"), N, 1.0, v1),
	       v5, (identifier("SModel.plus_gwF"), N, 1.0, v2),
	       v6, (identifier("SModel.plus_gwF"), N, 1.0, v3),
	       (identifier("ReversibleFrequency"), T, (identifier("iotaUnsigned"), T.size()), (identifier("SModel.f3x4_frequencies"),T,v1,v2,v3), (identifier("SModel.muse_gaut_matrix"), T, v4, v5, v6))
	       );
  }

  // Improvement: make all the variables ALSO be a formula_expression_ref, containing their own bounds, etc.
  formula_expression_ref Plus_gwF_Model(const alphabet& a, const valarray<double>& pi0)
  {
    formula_expression_ref f = def_parameter("f", 1.0, between(0,1), (identifier("uniform"), Tuple(0.0, 1.0)));

    formula_expression_ref pi = Frequencies_Model(a,pi0);

    return let_expression(v1,(identifier("listToVectorDouble"),pi),
			  (identifier("ReversibleFrequency"), a, (identifier("iotaUnsigned"), a.size()), v1, (identifier("SModel.plus_gwF"), a, f, v1))
			  );
  }

  formula_expression_ref Plus_gwF_Model(const alphabet& a)
  {
    valarray<double> pi (1.0/a.size(), a.size());
    return Plus_gwF_Model(a,pi);
  }

  formula_expression_ref Reversible_Markov_Model(const formula_expression_ref& FS, const formula_expression_ref& FR)
  {
    formula_expression_ref S = prefix_formula("S",FS);
    formula_expression_ref R = prefix_formula("R",FR);
    
    return (identifier("qFromSandR"), S, R);
  }

  formula_expression_ref Unit_Model(const formula_expression_ref& R)
  {
    formula_expression_ref R2 = R;

    R2 = (identifier("MixtureModel"), (identifier("DiscreteDistribution"), List(Tuple(1.0,R))));

    return R2;
  }

  formula_expression_ref Mixture_Model(const vector<formula_expression_ref>& models)
  {
    const int N = models.size();

    formula_expression_ref M = ListEnd;
    formula_expression_ref P = ListEnd;

    for(int i=N-1;i>=0;i--)
    {
      string I = convertToString(i+1);

      formula_expression_ref p = def_parameter( "Mixture.p"+I, 1.0/N, between(0,1)); 
      formula_expression_ref m = prefix_formula(I, models[i]);

      M = m & M;
      P = p & P;
    }
    formula_expression_ref R = (identifier("mixMixtureModels"),P,M);

    R.add_expression(constructor(":~",2) + P.exp() + (identifier("dirichlet'"), Tuple(N, 1.0)));

    return R;
  }
}

