#include <sstream>
#include "rates.H"
#include "smodel/operations.H"
#include "distribution-operations.H"
#include "computation/operations.H"
#include "computation/expression.H"
#include "computation/prelude.H"
#include "smodel/functions.H"
#include "io.H"

using boost::shared_ptr;
using std::vector;
using std::valarray;
using std::string;
using boost::shared_ptr;
using boost::dynamic_pointer_cast;
using std::istringstream;
using std::istream;

const expression_ref HKY = lambda_expression( substitution::HKY_Op());
const expression_ref TN = lambda_expression( substitution::TN_Op());
const expression_ref GTR = lambda_expression( substitution::GTR_Op());
const expression_ref Singlet_to_Triplet_Exchange = lambda_expression( substitution::Singlet_to_Triplet_Exchange_Op() );

namespace substitution
{
  object_ptr<const Object> Plus_gwF_Function(const alphabet& a, double f, const vector<double>& pi_)
  {
    object_ptr<MatrixObject> R( new MatrixObject );

    const int n = a.size();

    R->t.resize(n, n);

    // compute frequencies
    vector<double> pi = pi_;
    normalize(pi);
    assert(a.size() == pi.size());
    
    // compute transition rates
    valarray<double> pi_f(n);
    for(int i=0;i<n;i++)
      pi_f[i] = pow(pi[i],f);

    for(int i=0;i<n;i++)
      for(int j=0;j<n;j++)
	R->t(i,j) = pi_f[i]/pi[i] * pi_f[j];

    // diagonal entries should have no effect
    for(int i=0;i<n;i++)
      R->t(i,i) = 0;

    return R;
  }

  closure Plus_gwF_Op::operator()(OperationArgs& Args) const
  {
    const alphabet& a = *Args.evaluate_as<alphabet>(0);

    double f = *Args.evaluate_as<Double>(1);

    object_ptr< const Vector<double> > pi = Args.evaluate_as< Vector<double> >(2);

    return Plus_gwF_Function(a,f,*pi);
  }

  const expression_ref Plus_gwF = lambda_expression( Plus_gwF_Op() );


  closure F3x4_Frequencies_Op::operator()(OperationArgs& Args) const
  {
    const Triplets& T = *Args.evaluate_as<Triplets>(0);
    // The way alphabet is currently implemented, triplets must be triplets of nucleotides.

    const vector<double>& pi1 = Args.evaluate_as<Vector<double>>(1)->t;
    const vector<double>& pi2 = Args.evaluate_as<Vector<double>>(2)->t;
    const vector<double>& pi3 = Args.evaluate_as<Vector<double>>(3)->t;

    Vector<double> pi;
    pi.t.resize(T.size());
    for(int i=0;i<T.size();i++)
      pi.t[i] = pi1[T.sub_nuc(i,0)] * pi2[T.sub_nuc(i,1)] * pi3[T.sub_nuc(i,2)];

    // Some triplets may be missing from the triplet alphabet (e.g. stop codons).  So renormalize.

    double scale = 1.0/sum(pi.t);
    for(double& d : pi.t)
      d *= scale;

    assert(std::abs(sum(pi.t) - 1.0) < 1.0e-9);

    return pi;
  }

  const expression_ref F3x4_Frequencies = lambda_expression( F3x4_Frequencies_Op() );

  closure F3x4_Matrix_Op::operator()(OperationArgs& Args) const
  {
    const Triplets& T = *Args.evaluate_as<Triplets>(0);
  
    const Matrix& R1 = Args.evaluate_as<MatrixObject>(1)->t; 
    const Matrix& R2 = Args.evaluate_as<MatrixObject>(2)->t;
    const Matrix& R3 = Args.evaluate_as<MatrixObject>(3)->t;

    // The way alphabet is currently implemented, triplets must be triplets of nucleotides.
    assert(R1.size1() == 4);
    assert(R1.size2() == 4);
    assert(R2.size1() == 4);
    assert(R2.size2() == 4);
    assert(R3.size1() == 4);
    assert(R3.size2() == 4);

    object_ptr<MatrixObject> R( new MatrixObject );

    const int n = T.size();

    R->t.resize(n, n);
    for(int i=0;i<n;i++)
      for(int j=0;j<n;j++)
      {
	int nmuts=0;
	int from=-1;
	int to=-1;
	int pos=-1;
	for(int p=0;p<3;p++)
	  if (T.sub_nuc(i,p) != T.sub_nuc(j,p)) {
	    nmuts++;
	    pos = p;
	    from = T.sub_nuc(i,p);
	    to = T.sub_nuc(j,p);
	  }

	double r = 0;
	if (nmuts == 1)
	{
	  if (pos == 0)
	    r = R1(from,to);
	  else if (pos == 1)
	    r = R2(from,to);
	  else if (pos == 2)
	    r = R3(from,to);
	  else
	    std::abort();
	}
	R->t(i,j) = r;
      }

    return R;
  }

  const expression_ref F3x4_Matrix = lambda_expression( F3x4_Matrix_Op() );


  using namespace probability;

  object_ptr<Object> SimpleExchangeFunction(double rho, int n)
  {
    object_ptr<SymmetricMatrixObject> R(new SymmetricMatrixObject);

    R->t.resize(n);

    for(int i=0;i<n;i++) {
      for(int j=0;j<n;j++)
	R->t(i,j) = rho;

      R->t(i,i) = 0;       // this is NOT a rate away.
    }

    return R;
  }

  object_ptr<const Object> EQU_Exchange_Function(const alphabet& a)
  {
    object_ptr<SymmetricMatrixObject> R(new SymmetricMatrixObject);

    R->t.resize(a.size());

    // Calculate S matrix
    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++)
	R->t(i,j) = 1;

    return R;
  }

  object_ptr<const Object> HKY_Function(const Nucleotides& a, double kappa)
  {
    assert(a.size()==4);

    object_ptr<SymmetricMatrixObject> R(new SymmetricMatrixObject);

    R->t.resize(a.size());

    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++) {
	if (i==j) continue;
	if (a.transversion(i,j))
	  R->t(i,j) = 1;
	else
	  R->t(i,j) = kappa;
      }

    return R;
  }

  closure HKY_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const Nucleotides> N = Args.evaluate_as<Nucleotides>(0);
    double kappa = *Args.evaluate_as<Double>(1);
    
    return HKY_Function(*N,kappa);
  }

  formula_expression_ref HKY_Model(const alphabet& a)
  {
    formula_expression_ref kappa = def_parameter("HKY::kappa", 2.0, lower_bound(0.0), log_laplace_dist, Tuple(log(2), 0.25));

    return (HKY, a, kappa);
  }
  
  object_ptr<const Object> TN_Function(const Nucleotides& a, double kappa1, double kappa2)
  {
    assert(a.size()==4);
  
    object_ptr<SymmetricMatrixObject> R(new SymmetricMatrixObject);

    R->t.resize(a.size());
  
    for(int i=0;i<a.size();i++)
      for(int j=0;j<a.size();j++) {
	if (i==j) continue;
	if (a.transversion(i,j))
	  R->t(i,j) = 1;
	else if (a.purine(i))
	  R->t(i,j) = kappa1;
	else
	  R->t(i,j) = kappa2;
      }
  
    return R;
  }

  closure TN_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const Nucleotides> N = Args.evaluate_as<Nucleotides>(0);
    double kappa1 = *Args.evaluate_as<Double>(1);
    double kappa2 = *Args.evaluate_as<Double>(2);
    
    return TN_Function(*N,kappa1,kappa2);
  }

  formula_expression_ref TN_Model(const alphabet& a)
  {
    formula_expression_ref kappa1 = def_parameter("TN::kappa(pur)", 2.0, lower_bound(0.0), log_laplace_dist, Tuple(log(2), 0.25));
    formula_expression_ref kappa2 = def_parameter("TN::kappa(pyr)", 2.0, lower_bound(0.0), log_laplace_dist, Tuple(log(2), 0.25));

    return (TN, a, kappa1, kappa2);
  }
  
  object_ptr<const Object> GTR_Function(const Nucleotides& a, 
					double AG, double AT, double AC,
					double GT, double GC, 
					double TC)
  {
    assert(a.size()==4);

    object_ptr<SymmetricMatrixObject> R(new SymmetricMatrixObject);

    R->t.resize(a.size());
  
    double total = AG + AT + AC + GT + GC + TC;

    R->t(0,1) = AG/total;
    R->t(0,2) = AT/total;
    R->t(0,3) = AC/total;

    R->t(1,2) = GT/total;
    R->t(1,3) = GC/total;

    R->t(2,3) = TC/total;

    return R;
  }

  closure GTR_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const Nucleotides> N = Args.evaluate_as<Nucleotides>(0);
    double AG = *Args.evaluate_as<Double>(1);
    double AT = *Args.evaluate_as<Double>(2);
    double AC = *Args.evaluate_as<Double>(3);
    double GT = *Args.evaluate_as<Double>(4);
    double GC = *Args.evaluate_as<Double>(5);
    double TC = *Args.evaluate_as<Double>(6);
    
    return GTR_Function(*N,AG,AT,AC,GT,GC,TC);
  }

  formula_expression_ref GTR_Model(const alphabet& a)
  {
    formula_expression_ref AG = def_parameter("GTR::AG", 2.0/8, between(0.0,1.0));
    formula_expression_ref AT = def_parameter("GTR::AT", 1.0/8, between(0.0,1.0));
    formula_expression_ref AC = def_parameter("GTR::AC", 1.0/8, between(0.0,1.0));
    formula_expression_ref GT = def_parameter("GTR::GT", 1.0/8, between(0.0,1.0));
    formula_expression_ref GC = def_parameter("GTR::GC", 1.0/8, between(0.0,1.0));
    formula_expression_ref TC = def_parameter("GTR::TC", 2.0/8, between(0.0,1.0));

    formula_expression_ref R = (GTR, a, AG, AT, AC, GT, GC, TC);

    // I should generalize this...
    // Should I make a tuple of tuples?
    R.add_expression((distributed, 
		      AG&(AT&(AC&(GT&(GC&(TC&ListEnd))))),
		      Tuple(dirichlet_dist, Tuple(8.0, 4.0, 4.0, 4.0, 4.0, 8.0) )
		      )
		     );

    return R;
  }
  
  object_ptr<const Object> M0_Function(const Codons& C, const SymmetricMatrixObject& S2,double omega)
  {
    object_ptr<SymmetricMatrixObject> R ( new SymmetricMatrixObject );

    R->t.resize(C.size());

    for(int i=0;i<C.size();i++) 
    {
      for(int j=0;j<i;j++) {
	int nmuts=0;
	int pos=-1;
	for(int p=0;p<3;p++)
	  if (C.sub_nuc(i,p) != C.sub_nuc(j,p)) {
	    nmuts++;
	    pos=p;
	  }
	assert(nmuts>0);
	assert(pos >= 0 and pos < 3);

	double rate=0.0;

	if (nmuts == 1) {

	  int l1 = C.sub_nuc(i,pos);
	  int l2 = C.sub_nuc(j,pos);
	  assert(l1 != l2);

	  rate = S2.t(l1,l2);

	  if (C.translate(i) != C.translate(j))
	    rate *= omega;	
	}

	R->t(i,j) = R->t(j,i) = rate;
      }
    }

    return R;
  }


  closure M0_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const Codons> C = Args.evaluate_as<Codons>(0);
    object_ptr<const SymmetricMatrixObject> S = Args.evaluate_as<SymmetricMatrixObject>(1);
    object_ptr<const Double> omega = Args.evaluate_as<Double>(2);
    
    return M0_Function(*C, *S, *omega);
  }

  expression_ref M0E = lambda_expression( M0_Op() );

  object_ptr<SymmetricMatrixObject> SingletToTripletExchangeFunction(const Triplets& T, const SymmetricMatrixObject& R2)
  {
    int N = T.size();

    object_ptr<SymmetricMatrixObject> R ( new SymmetricMatrixObject(N) );

    SymmetricMatrix& S = R->t;
    const SymmetricMatrix& S2 = R2.t;

    for(int i=0;i<T.size();i++)
      for(int j=0;j<i;j++) 
      {
	int nmuts=0;
	int pos=-1;
	for(int p=0;p<3;p++)
	  if (T.sub_nuc(i,p) != T.sub_nuc(j,p)) {
	    nmuts++;
	    pos=p;
	  }
	assert(nmuts>0);
	assert(pos >= 0 and pos < 3);
	
	S(i,j) = 0;

	if (nmuts == 1) {

	  int l1 = T.sub_nuc(i,pos);
	  int l2 = T.sub_nuc(j,pos);
	  assert(l1 != l2);

	  S(i,j) = S2(l1,l2);
	}
      }

    return R;
  }

  closure Singlet_to_Triplet_Exchange_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const Triplets> T = Args.evaluate_as<Triplets>(0);
    object_ptr<const SymmetricMatrixObject> S = Args.evaluate_as<SymmetricMatrixObject>(1);
    
    return SingletToTripletExchangeFunction(*T,*S);
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

    expression_ref N = get_tuple(vector<double>(a.size(), 1.0) );
    F.add_expression( (distributed, F, Tuple(dirichlet_dist,N ) ) );

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

    return let_expression(v1,(Vector_From_List<double,Double>(),pi),
			  (ReversibleFrequency, a, (Iota<unsigned>(), a.size()), v1, (Plus_gwF, a, 1.0, v1))
			  );
  }

  formula_expression_ref Plus_F_Model(const alphabet& a)
  {
    valarray<double> pi (1.0/a.size(), a.size());
    return Plus_gwF_Model(a,pi);
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

    return let(v2,(Vector_From_List<double,Double>(),pi),
	       v1,(F3x4_Frequencies,T,v2,v2,v2),
	       (ReversibleFrequency, T, (Iota<unsigned>(), T.size()), v1, (Plus_gwF, T, 1.0, v1))
	       );
  }

  // Improvement: make all the variables ALSO be a formula_expression_ref, containing their own bounds, etc.
  formula_expression_ref MG94_Model(const Triplets& T)
  {
    const Nucleotides& N = T.getNucleotides();
    formula_expression_ref pi = Frequencies_Model(N);

    return let(v2,(Vector_From_List<double,Double>(),pi),
	       v1,(F3x4_Frequencies,T,v2,v2,v2),
	       v3,(Plus_gwF, N, 1.0, v2),
	       (ReversibleFrequency, T, (Iota<unsigned>(), T.size()), v1, (F3x4_Matrix, T, v3, v3, v3))
	       );
  }

  formula_expression_ref F3x4_Model(const Triplets& T)
  {
    const Nucleotides& N = T.getNucleotides();
    formula_expression_ref pi1 = Frequencies_Model(N);
    pi1 = prefix_formula("1",pi1);
    formula_expression_ref pi2 = Frequencies_Model(N);
    pi2 = prefix_formula("2",pi2);
    formula_expression_ref pi3 = Frequencies_Model(N);
    pi3 = prefix_formula("3",pi3);

    return let(v1, (Vector_From_List<double,Double>(),pi1),
	       v2, (Vector_From_List<double,Double>(),pi2),
	       v3, (Vector_From_List<double,Double>(),pi3),
	       v4, (F3x4_Frequencies,T,v1,v2,v3),
	       (ReversibleFrequency, T, (Iota<unsigned>(), T.size()), v4, (Plus_gwF, T, 1.0, v4))
	       );
  }

  formula_expression_ref MG94w9_Model(const Triplets& T)
  {
    const Nucleotides& N = T.getNucleotides();
    formula_expression_ref pi1 = Frequencies_Model(N);
    pi1 = prefix_formula("1",pi1);
    formula_expression_ref pi2 = Frequencies_Model(N);
    pi2 = prefix_formula("2",pi2);
    formula_expression_ref pi3 = Frequencies_Model(N);
    pi3 = prefix_formula("3",pi3);

    return let(v1, (Vector_From_List<double,Double>(),pi1),
	       v2, (Vector_From_List<double,Double>(),pi2),
	       v3, (Vector_From_List<double,Double>(),pi3),
	       v4, (Plus_gwF, N, 1.0, v1),
	       v5, (Plus_gwF, N, 1.0, v2),
	       v6, (Plus_gwF, N, 1.0, v3),
	       (ReversibleFrequency, T, (Iota<unsigned>(), T.size()), (F3x4_Frequencies,T,v1,v2,v3), (F3x4_Matrix, T, v4, v5, v6))
	       );
  }

  // Improvement: make all the variables ALSO be a formula_expression_ref, containing their own bounds, etc.
  formula_expression_ref Plus_gwF_Model(const alphabet& a, const valarray<double>& pi0)
  {
    formula_expression_ref f = def_parameter("f", 1.0, between(0,1), uniform_dist, Tuple(0.0, 1.0));

    formula_expression_ref pi = Frequencies_Model(a,pi0);

    return let_expression(v1,(Vector_From_List<double,Double>(),pi),
			  (ReversibleFrequency, a, (Iota<unsigned>(), a.size()), v1, (Plus_gwF, a, f, v1))
			  );
  }

  formula_expression_ref Plus_gwF_Model(const alphabet& a)
  {
    valarray<double> pi (1.0/a.size(), a.size());
    return Plus_gwF_Model(a,pi);
  }

  object_ptr<const MatrixObject> Q_Function(const SymmetricMatrix& S, const Matrix& R)
  {
    const unsigned N = S.size1();
    assert(S.size1() == R.size1());
    assert(S.size1() == R.size2());

    Matrix Q(N,N);

    for(int i=0;i<N;i++) {
      double sum=0;
      for(int j=0;j<N;j++) {
	if (i==j) continue;
	Q(i,j) = S(i,j) * R(i,j);
	sum += Q(i,j);
      }
      Q(i,i) = -sum;
    }

    return object_ptr<const MatrixObject>(new MatrixObject(Q));
  }

  closure Q_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const SymmetricMatrixObject> S = Args.evaluate_as<SymmetricMatrixObject>(0);
    object_ptr<const MatrixObject> F = Args.evaluate_as<MatrixObject>(1);
    
    return Q_Function(*S, *F);
  }

  const expression_ref Q = lambda_expression( Q_Op() );

  formula_expression_ref Reversible_Markov_Model(const formula_expression_ref& FS, const formula_expression_ref& FR)
  {
    formula_expression_ref S = prefix_formula("S",FS);
    formula_expression_ref R = prefix_formula("R",FR);
    
    return (Q_from_S_and_R, S, R);
  }

  formula_expression_ref Unit_Model(const formula_expression_ref& R)
  {
    formula_expression_ref R2 = R;

    R2 = (MixtureModel, (DiscreteDistribution, List(Tuple(1.0,R))));

    return R2;
  }

  //FIXME!
  expression_ref DiscretizationFunction(const Distribution& D, Int n)
  {
    // Make a discretization - not uniform.
    Discretization d(n,D);

    double ratio = d.scale()/D.mean();
    
    // this used to affect the prior
    //    bool good_enough = (ratio > 1.0/1.5 and ratio < 1.5);

    // problem - this isn't completely general
    d.scale(1.0/ratio);
    
    vector<expression_ref> pairs;

    for(int i=0;i<n;i++)
      pairs.push_back( Tuple( d.f[i], d.r[i] ) );

    return graph_normalize((DiscreteDistribution, get_list(pairs) ));
  }

  // We want Q(mi -> mj) = Q[m](i -> j)   for letter exchange
  //         Q(mi -> ni) = R(m->n)        for model exchange
  // and     Q(mi -> nj) = 0              for all other pairs

  // We assume that R(m->n) = S(m,n) * M->distribution()[n]

  // This should result in a Markov chain where the frequencies are
  //  frequencies()[mi] = pi[i] * f[m] 
  // with pi = M->frequencies() 
  // and   f = M->distribution()

  // PROBLEM: I don't have a good way of defining the switching rate.
  // Right now, I have S(m,n) = rho, S(m,m) = 0
  // But, the S(m,n) do not correspond to switching rates exactly.
  // Instead, the switching rate is now rho*f[n], which is going to
  // be something like rho*(n-1)/n if there are n categories.
  
  // ADDITIONALLY, depending on how fine-grained the categories are,
  // a switching rate has a different interpretation.

  // HOWEVER, I think the current approach works for now, because it
  // approximates the model that at rate 'rho' the rate is randomly
  // re-drawn from the underlying distribution.  A lot of the time it
  // will fall in the same bin, giving a lower observed switching rate
  // when the discrete approximation to the continuous distribution has
  // low resolution.

  /*
  object_ptr<ReversibleMarkovModelObject> Modulated_Markov_Function(const ExchangeModelObject& S,MultiModelObject M)
  {
    // Make a copy and use this.

    // FIXME - how do we do this? M.set_rate(1);

    unsigned T = 0;
    for(int m=0; m < M.n_base_models(); m++) 
    {
      const ReversibleMarkovModelObject* RM = dynamic_cast<const ReversibleMarkovModelObject*>(&M.base_model(m));
      if (not RM)
	throw myexception()<<"Can't construct a modulated Markov model from non-Markov model"; // what is the name?
      T += RM->n_states();
    }

    object_ptr<ReversibleMarkovModelObject> R ( new ReversibleMarkovModelObject(*M.get_alphabet(), T) );

    // calculate the state_letters() map here!

    T = 0;
    for(int m=0; m < M.n_base_models(); m++) 
    {
      unsigned N = M.base_model(m).n_states();
      for(int i=0; i<N; i++)
	R->state_letters_[T+i] = M.base_model(m).state_letters()[i];

      T += N;
    }

    const int n_models = M.n_base_models();


    // The submodels had better all have equal frequencies!
    const valarray<double>& M_pi = M.base_models[0]->frequencies();
    const vector<double>&   M_f  = M.distribution();

    // calculate pi[ ] for each state
    T = 0;
    for(int m=0; m < n_models; m++) {
      unsigned N = M.base_model(m).n_states();
      for(int s=0; s < N; s++) 
	R->pi[T+s] = M_pi[s] * M_f[m];
      T += N;
    }
    

    // initially zero out the matrix
    for(int i=0;i<R->Q.size1();i++)
      for(int j=0;j<R->Q.size2();j++)
	R->Q(i,j) = 0;

    // rates for within-model transitions
    T=0;
    for(int m=0; m < n_models; m++) 
    {
      const ReversibleMarkovModelObject* RM = dynamic_cast<const ReversibleMarkovModelObject*>(&M.base_model(m));
      if (not RM)
	throw myexception()<<"Can't construct a modulated Markov model from non-Markov model"; // what is the name?

      unsigned N = RM->n_states();
      
      for(int s1=0; s1 < N; s1++) 
	for(int s2=0; s2 < N; s2++)
	  R->Q(T+s1,T+s2) = RM->Q(s1,s2);

      T += N;
    }

    // rates for between-model transitions
    unsigned T1=0;
    for(int m1=0; m1 < n_models; m1++) 
    {
      const ReversibleMarkovModelObject* RM1 = dynamic_cast<const ReversibleMarkovModelObject*>(&M.base_model(m1));
      unsigned N1 = RM1->n_states();

      unsigned T2=0;
      for(int m2=0; m2 < n_models; m2++) 
      {
	const ReversibleMarkovModelObject* RM2 = dynamic_cast<const ReversibleMarkovModelObject*>(&M.base_model(m2));
	unsigned N2 = RM2->n_states();
	assert(N1 == N2);

	if (m1 != m2) {
	  double S12 = S(m1,m2);
	  for(int s1=0;s1<N1;s1++)
	    R->Q(T1+s1,T2+s1) = S12*M_f[m2];
	}

	T2 += N2;
      }
      T1 += N1;
    }

    // recompute diagonals 
    for(int i=0;i<R->Q.size1();i++) 
    {
      double sum=0;
      for(int j=0;j<R->Q.size2();j++)
	if (i!=j)
	  sum += R->Q(i,j);
      R->Q(i,i) = -sum;
    }

    return R;
  }
  */

  expression_ref Modulated_Markov_E;

  formula_expression_ref Mixture_Model(const vector<formula_expression_ref>& models)
  {
    const int N = models.size();

    formula_expression_ref D = ListEnd;
    formula_expression_ref V = ListEnd;

    for(int i=0;i<N;i++)
    {
      string I = convertToString(i+1);
      formula_expression_ref p = def_parameter( "Mixture::p"+I, 1.0/N, between(0,1)); 
      formula_expression_ref m = prefix_formula(I, models[i]);

      D = Tuple(p,m)&D;
      V = p & V;
    }
    formula_expression_ref R = (MixtureModel, (DiscreteDistribution,D));

    R.add_expression((distributed, V, Tuple(dirichlet_dist, get_tuple(vector<Double>(N,1.0) ) ) )) ;

    return R;
  }

  /*
   * 1. pi[i]*Q(i,j) = pi[j]*Q(j,i)         - Because Q is reversible
   * 2. Q(i,j)/pi[j] = Q(j,i)/pi[i] = S1(i,j)
   * 3. pi[i]^1/2 * Q(j,i) / pi[j]^1/2 = S2(i,j)
   * 4. exp(Q) = pi^-1.2 * exp(pi^1/2 * Q * pi^-1/2) * pi^1/2
   *           = pi^-1.2 * exp(S2) * pi^1/2
   */

  object_ptr<const EigenValues> Get_Eigensystem_Function(const Matrix& Q, const vector<double>& pi)
  {
    const unsigned n = Q.size1();
    assert(Q.size2() == Q.size1());

#ifdef DEBUG_RATE_MATRIX
    cerr<<"scale = "<<rate()<<endl;

    assert(std::abs(sum(pi)-1.0) < 1.0e-6);
    for(int i=0;i<n;i++) {
      double sum = 0;
      for(int j=0;j<n;j++)
	sum += Q(i,j);
      assert(abs(sum) < 1.0e-6);
    }
#endif

    //--------- Compute pi[i]**0.5 and pi[i]**-0.5 ----------//
    vector<double> sqrt_pi(n);
    vector<double> inverse_sqrt_pi(n);
    for(int i=0;i<n;i++) {
      sqrt_pi[i] = sqrt(pi[i]);
      inverse_sqrt_pi[i] = 1.0/sqrt_pi[i];
    }

    //--------------- Calculate eigensystem -----------------//
    ublas::symmetric_matrix<double> S(n,n);
    for(int i=0;i<n;i++)
      for(int j=0;j<=i;j++) {
	S(i,j) = Q(i,j) * sqrt_pi[i] * inverse_sqrt_pi[j];

#ifdef DEBUG_RATE_MATRIX
	// check reversibility of rate matrix
	if (i != j) {
	  assert (S(i,j) >= 0);
	  double p12 = Q(i,j)*pi[i];
	  double p21 = Q(j,i)*pi[j];
	  assert (abs(p12-p21) < 1.0e-12*(1.0+abs(p12)));
	}
	else
	  assert (Q(i,j) <= 0);
#endif
      }

    //---------------- Compute eigensystem ------------------//
    return object_ptr<const EigenValues>(new EigenValues(S));
  }

  closure Get_Eigensystem_Op::operator()(OperationArgs& Args) const
  {
    const Matrix& Q = *Args.evaluate_as<MatrixObject>(0);
    const Vector<double>& pi = *Args.evaluate_as< Vector<double> >(1);
    
    return Get_Eigensystem_Function(Q, pi);
  }

  expression_ref Get_Eigensystem = lambda_expression(Get_Eigensystem_Op());
  //---------------------------------------------------------------------------------------//

  expression_ref RateMatrix = lambda_expression( constructor("RateMatrix", 4) );
  // Q_from_R_and_S(R,S) = let Q = S*R in (RateMatrix, Q, (frequencies, R), (get_eigensystem, Q, pi), 1.0)

  // 
  object_ptr<const Double> 
  Get_Equilibrium_Rate_Function(const alphabet& a, const vector<unsigned>& smap, const Matrix& Q, const vector<double>& pi)
  {
    assert(Q.size2() == Q.size1());
    const unsigned N = smap.size();
    
    double scale=0;

    if (N == a.size()) 
    {
      for(int i=0;i<Q.size1();i++) 
	scale -= pi[i]*Q(i,i);
    }
    else 
    {
      for(int s1=0;s1<N;s1++)
      {
	double temp = 0;
	for(int s2=0;s2<N;s2++)
	  if (smap[s1] != smap[s2])
	    temp += Q(s1,s2);

	scale += temp*pi[s1];
      }
    }

    return object_ptr<const Double>(new Double(scale/a.width()));
  }

  closure Get_Equilibrium_Rate_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const alphabet> a = Args.evaluate_as<alphabet>(0);
    object_ptr<const Box<vector<unsigned> > > smap = Args.evaluate_as< Box<vector<unsigned> > >(1);
    object_ptr<const MatrixObject > Q = Args.evaluate_as< MatrixObject >(2);
    object_ptr<const Vector<double> > pi = Args.evaluate_as< Vector<double> >(3);
    
    return Get_Equilibrium_Rate_Function(*a, *smap, *Q, pi->t);
  }

  expression_ref Get_Equilibrium_Rate = lambda_expression(Get_Equilibrium_Rate_Op());

  object_ref Empirical_Exchange_Function(const alphabet& a, istream& ifile)
  {
    object_ptr<SymmetricMatrixObject> R(new SymmetricMatrixObject);

    int n = a.size();

    R->t.resize(n);
  
    for(int i=0;i<n;i++)
      for(int j=0;j<i;j++) {
	ifile>>R->t(i,j);
	R->t(j,i) = R->t(i,j);
      }

    return object_ref(R);
  }

  object_ref Empirical_Exchange_Function(const alphabet& a, const String& filename)
  {
    checked_ifstream ifile(filename, "empirical rate matrix file");
    return Empirical_Exchange_Function(a,ifile);
  }

  closure Empirical_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const alphabet> a = Args.evaluate_as<alphabet>(0);
    object_ptr<const String> S = Args.evaluate_as<String>(1);
    return Empirical_Exchange_Function(*a, *S);
  }

  object_ref PAM_Exchange_Function(const alphabet& a)
  {
    istringstream file(
		       "27                                                                         \
 98  32                                                                     \
120   0 905                                                                 \
 36  23   0   0                                                             \
 89 246 103 134   0                                                         \
198   1 148 1153  0 716                                                     \
240   9 139 125  11  28  81                                                 \
 23 240 535  86  28 606  43  10                                             \
 65  64  77  24  44  18  61   0   7                                         \
 41  15  34   0   0  73  11   7  44 257                                     \
 26 464 318  71   0 153  83  27  26  46  18                                 \
 72  90   1   0   0 114  30  17   0 336 527 243                             \
 18  14  14   0   0   0   0  15  48 196 157   0  92                         \
250 103  42  13  19 153  51  34  94  12  32  33  17  11                     \
409 154 495  95 161  56  79 234  35  24  17  96  62  46 245                 \
371  26 229  66  16  53  34  30  22 192  33 136 104  13  78 550             \
  0 201  23   0   0   0   0   0  27   0  46   0   0  76   0  75   0         \
 24   8  95   0  96   0  22   0 127  37  28  13   0 698   0  34  42  61     \
208  24  15  18  49  35  37  54  44 889 175  10 258  12  48  30 157   0  28 \
");
    return Empirical_Exchange_Function(a, file);
  }

  closure PAM_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const alphabet> a = Args.evaluate_as<alphabet>(0);
    return PAM_Exchange_Function(*a);
  }

  object_ref JTT_Exchange_Function(const alphabet& a)
  {
    istringstream file(
		       " 58                                                                        \
 54  45                                                                    \
 81  16 528                                                                \
 56 113  34  10                                                            \
 57 310  86  49   9                                                        \
105  29  58 767   5 323                                                    \
179 137  81 130  59  26 119                                                \
 27 328 391 112  69 597  26  23                                            \
 36  22  47  11  17   9  12   6  16                                        \
 30  38  12   7  23  72   9   6  56 229                                    \
 35 646 263  26   7 292 181  27  45  21  14                                \
 54  44  30  15  31  43  18  14  33 479 388  65                            \
 15   5  10   4  78   4   5   5  40  89 248   4  43                        \
194  74  15  15  14 164  18  24 115  10 102  21  16  17                    \
378 101 503  59 223  53  30 201  73  40  59  47  29  92 285                \
475  64 232  38  42  51  32  33  46 245  25 103 226  12 118 477            \
  9 126   8   4 115  18  10  55   8   9  52  10  24  53   6  35  12        \
 11  20  70  46 209  24   7   8 573  32  24   8  18 536  10  63  21  71    \
298  17  16  31  62  20  45  47  11 961 180  14 323  62  23  38 112  25  16 \
");
    return Empirical_Exchange_Function(a, file);
  }

  closure JTT_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const alphabet> a = Args.evaluate_as<alphabet>(0);
    return JTT_Exchange_Function(*a);
  }

  object_ref WAG_Exchange_Function(const alphabet& a)
  {
    istringstream file(
		       "0.551571 \
0.509848  0.635346 \
0.738998  0.147304  5.429420 \
1.027040  0.528191  0.265256  0.0302949 \
0.908598  3.035500  1.543640  0.616783  0.0988179 \
1.582850  0.439157  0.947198  6.174160  0.021352  5.469470 \
1.416720  0.584665  1.125560  0.865584  0.306674  0.330052  0.567717 \
0.316954  2.137150  3.956290  0.930676  0.248972  4.294110  0.570025  0.249410 \
0.193335  0.186979  0.554236  0.039437  0.170135  0.113917  0.127395  0.0304501 0.138190 \
0.397915  0.497671  0.131528  0.0848047 0.384287  0.869489  0.154263  0.0613037 0.499462  3.170970 \
0.906265  5.351420  3.012010  0.479855  0.0740339 3.894900  2.584430  0.373558  0.890432  0.323832  0.257555 \
0.893496  0.683162  0.198221  0.103754  0.390482  1.545260  0.315124  0.174100  0.404141  4.257460  4.854020  0.934276 \
0.210494  0.102711  0.0961621 0.0467304 0.398020  0.0999208 0.0811339 0.049931  0.679371  1.059470  2.115170  0.088836  1.190630 \
1.438550  0.679489  0.195081  0.423984  0.109404  0.933372  0.682355  0.243570  0.696198  0.0999288 0.415844  0.556896  0.171329  0.161444 \
3.370790  1.224190  3.974230  1.071760  1.407660  1.028870  0.704939  1.341820  0.740169  0.319440  0.344739  0.967130  0.493905  0.545931  1.613280 \
2.121110  0.554413  2.030060  0.374866  0.512984  0.857928  0.822765  0.225833  0.473307  1.458160  0.326622  1.386980  1.516120  0.171903  0.795384  4.378020 \
0.113133  1.163920  0.0719167 0.129767  0.717070  0.215737  0.156557  0.336983  0.262569  0.212483  0.665309  0.137505  0.515706  1.529640  0.139405  0.523742  0.110864 \
0.240735  0.381533  1.086000  0.325711  0.543833  0.227710  0.196303  0.103604  3.873440  0.420170  0.398618  0.133264  0.428437  6.454280  0.216046  0.786993  0.291148  2.485390 \
2.006010  0.251849  0.196246  0.152335  1.002140  0.301281  0.588731  0.187247  0.118358  7.821300  1.800340  0.305434  2.058450  0.649892  0.314887  0.232739  1.388230  0.365369  0.314730 \
");
    return Empirical_Exchange_Function(a, file);
  }

  closure WAG_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const alphabet> a = Args.evaluate_as<alphabet>(0);
    return WAG_Exchange_Function(*a);
  }

  object_ref LG_Exchange_Function(const alphabet& a)
  {
    istringstream file(
		       "0.425093 \
0.276818 0.751878 \
0.395144 0.123954 5.076149 \
2.489084 0.534551 0.528768 0.062556 \
0.969894 2.807908 1.695752 0.523386 0.084808 \
1.038545 0.363970 0.541712 5.243870 0.003499 4.128591 \
2.066040 0.390192 1.437645 0.844926 0.569265 0.267959 0.348847 \
0.358858 2.426601 4.509238 0.927114 0.640543 4.813505 0.423881 0.311484 \
0.149830 0.126991 0.191503 0.010690 0.320627 0.072854 0.044265 0.008705 0.108882 \
0.395337 0.301848 0.068427 0.015076 0.594007 0.582457 0.069673 0.044261 0.366317 4.145067 \
0.536518 6.326067 2.145078 0.282959 0.013266 3.234294 1.807177 0.296636 0.697264 0.159069 0.137500 \
1.124035 0.484133 0.371004 0.025548 0.893680 1.672569 0.173735 0.139538 0.442472 4.273607 6.312358 0.656604 \
0.253701 0.052722 0.089525 0.017416 1.105251 0.035855 0.018811 0.089586 0.682139 1.112727 2.592692 0.023918 1.798853 \
1.177651 0.332533 0.161787 0.394456 0.075382 0.624294 0.419409 0.196961 0.508851 0.078281 0.249060 0.390322 0.099849 0.094464 \
4.727182 0.858151 4.008358 1.240275 2.784478 1.223828 0.611973 1.739990 0.990012 0.064105 0.182287 0.748683 0.346960 0.361819 1.338132 \
2.139501 0.578987 2.000679 0.425860 1.143480 1.080136 0.604545 0.129836 0.584262 1.033739 0.302936 1.136863 2.020366 0.165001 0.571468 6.472279 \
0.180717 0.593607 0.045376 0.029890 0.670128 0.236199 0.077852 0.268491 0.597054 0.111660 0.619632 0.049906 0.696175 2.457121 0.095131 0.248862 0.140825 \
0.218959 0.314440 0.612025 0.135107 1.165532 0.257336 0.120037 0.054679 5.306834 0.232523 0.299648 0.131932 0.481306 7.803902 0.089613 0.400547 0.245841 3.151815 \
2.547870 0.170887 0.083688 0.037967 1.959291 0.210332 0.245034 0.076701 0.119013 10.649107 1.702745 0.185202 1.898718 0.654683 0.296501 0.098369 2.188158 0.189510 0.249313 \
");
    return Empirical_Exchange_Function(a, file);
  }

  closure LG_Op::operator()(OperationArgs& Args) const
  {
    object_ptr<const alphabet> a = Args.evaluate_as<alphabet>(0);
    return WAG_Exchange_Function(*a);
  }
  const expression_ref Empirical = lambda_expression( substitution::Empirical_Op());
  const expression_ref PAM = lambda_expression( substitution::PAM_Op());
  const expression_ref JTT = lambda_expression( substitution::JTT_Op());
  const expression_ref WAG = lambda_expression( substitution::WAG_Op());
  const expression_ref LG = lambda_expression( substitution::LG_Op());
}
