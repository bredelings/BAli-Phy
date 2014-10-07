#define DEBUG_RATE_MATRIX
#include "computation/computation.H"
#include "math/exponential.H"
#include "math/eigenvalue.H"
#include "sequence/alphabet.H"
#include "io.H"
#include <valarray>

using std::vector;
using std::istringstream;
using std::istream;
using std::valarray;

using std::cerr;
using std::endl;

extern "C" closure builtin_function_lExp(OperationArgs& Args)
{
  auto L = Args.evaluate_as<EigenValues>(0);
  auto pi = Args.evaluate_as< Vector<double> >(1);
  double t = *Args.evaluate_as<Double>(2);

  Box<Matrix>* M = new Box<Matrix>;
  *M = exp(*L, *pi, t);
  return M;
}

extern "C" closure builtin_function_reversible_rate_matrix(OperationArgs& Args)
{
  object_ptr<const Box<Matrix>> S_ = Args.evaluate_as<Box<Matrix>>(0);
  const Matrix& S = *S_;

  object_ptr<const Box<Matrix>> R_ = Args.evaluate_as<Box<Matrix>>(1);
  const Matrix& R = *R_;
    
  const unsigned N = S.size1();
  assert(S.size1() == R.size1());
  assert(S.size1() == R.size2());

  object_ptr<Box<Matrix>> Q_(new Box<Matrix>(N,N));
  Matrix& Q = *Q_;

  for(int i=0;i<N;i++) {
    double sum=0;
    for(int j=0;j<N;j++) {
      if (i==j) continue;
      Q(i,j) = S(i,j) * R(i,j);
      sum += Q(i,j);
    }
    Q(i,i) = -sum;
  }

  return Q_;
}

/*
 * 1. pi[i]*Q(i,j) = pi[j]*Q(j,i)         - Because Q is reversible
 * 2. Q(i,j)/pi[j] = Q(j,i)/pi[i] = S1(i,j)
 * 3. pi[i]^1/2 * Q(j,i) / pi[j]^1/2 = S2(i,j)
 * 4. exp(Q) = pi^-1.2 * exp(pi^1/2 * Q * pi^-1/2) * pi^1/2
 *           = pi^-1.2 * exp(S2) * pi^1/2
 */

extern "C" closure builtin_function_get_eigensystem(OperationArgs& Args)
{
  object_ptr<const Box<Matrix>> Q_ = Args.evaluate_as<Box<Matrix>>(0);
  const Matrix& Q = *Q_;

  object_ptr<const Vector<double>> pi_ = Args.evaluate_as< Vector<double> >(1);
  const vector<double>& pi = *pi_;

  const unsigned n = Q.size1();
  assert(Q.size2() == Q.size1());

#ifdef DEBUG_RATE_MATRIX
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
  Matrix S(n,n);
  for(int i=0;i<n;i++)
    for(int j=0;j<=i;j++) {
      S(j,i) = S(i,j) = Q(i,j) * sqrt_pi[i] * inverse_sqrt_pi[j];

#ifdef DEBUG_RATE_MATRIX
      // check reversibility of rate matrix
      if (i != j) {
	assert (S(i,j) >= 0);
	double p12 = Q(i,j)*pi[i];
	double p21 = Q(j,i)*pi[j];
	assert (abs(p12-p21) < 1.0e-12*(1.0+abs(p12)));
	if (i > j)
	  assert( abs(S(i,j) - S(j,i)) < 1.0e-13 );
      }
      else
	assert (Q(i,j) <= 0);
#endif
    }

  //---------------- Compute eigensystem ------------------//
  return object_ptr<const EigenValues>(new EigenValues(S));
}


extern "C" closure builtin_function_get_equilibrium_rate(OperationArgs& Args)
{
  object_ptr<const alphabet> a_ = Args.evaluate_as<alphabet>(0);
  const alphabet& a = *a_;

  object_ptr<const Vector<unsigned> > smap_ = Args.evaluate_as< Vector<unsigned> >(1);
  const vector<unsigned>& smap = *smap_;

  object_ptr<const Box<Matrix> > Q_ = Args.evaluate_as< Box<Matrix> >(2);
  const Matrix& Q = *Q_;

  object_ptr<const Vector<double> > pi_ = Args.evaluate_as< Vector<double> >(3);
  const vector<double> pi = *pi_;

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


extern "C" closure builtin_function_singlet_to_triplet_exchange(OperationArgs& Args)
{
  object_ptr<const Triplets> T_ = Args.evaluate_as<Triplets>(0);
  const Triplets& T = *T_;

  object_ptr<const Box<Matrix>> S_ = Args.evaluate_as<Box<Matrix>>(1);
  const Matrix& S2 = *S_;

  int N = T.size();

  object_ptr<Box<Matrix>> R ( new Box<Matrix>(N,N) );

  Matrix& S = *R;

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
	
      S(j,i) = S(i,j) = 0;

      if (nmuts == 1) {

	int l1 = T.sub_nuc(i,pos);
	int l2 = T.sub_nuc(j,pos);
	assert(l1 != l2);

	S(j,i) = S(i,j) = S2(l1,l2);
      }
    }

  return R;
}

extern "C" closure builtin_function_muse_gaut_matrix(OperationArgs& Args)
{
  auto T = Args.evaluate_as<Triplets>(0);
  
  auto R1_ = Args.evaluate_as<Box<Matrix>>(1);
  const Matrix& R1 = *R1_;

  auto R2_ = Args.evaluate_as<Box<Matrix>>(2);
  const Matrix& R2 = *R2_;

  auto R3_ = Args.evaluate_as<Box<Matrix>>(3);
  const Matrix& R3 = *R3_;

  // The way alphabet is currently implemented, triplets must be triplets of nucleotides.
  assert(R1.size1() == 4);
  assert(R1.size2() == 4);
  assert(R2.size1() == 4);
  assert(R2.size2() == 4);
  assert(R3.size1() == 4);
  assert(R3.size2() == 4);

  const int n = T->size();

  object_ptr<Box<Matrix>> R( new Box<Matrix>(n,n) );

  for(int i=0;i<n;i++)
    for(int j=0;j<n;j++)
    {
      int nmuts=0;
      int from=-1;
      int to=-1;
      int pos=-1;
      for(int p=0;p<3;p++)
	if (T->sub_nuc(i,p) != T->sub_nuc(j,p)) {
	  nmuts++;
	  pos = p;
	  from = T->sub_nuc(i,p);
	  to = T->sub_nuc(j,p);
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
      (*R)(i,j) = r;
    }

  return R;
}


object_ptr<Object> SimpleExchangeFunction(double rho, int n)
{
  object_ptr<Box<Matrix>> R(new Box<Matrix>(n,n));

  for(int i=0;i<n;i++) {
    for(int j=0;j<n;j++)
      (*R)(i,j) = rho;

    (*R)(i,i) = 0;       // this is NOT a rate away.
  }

  return R;
}

object_ptr<const Object> EQU_Exchange_Function(int n)
{
  object_ptr<Box<Matrix>> R(new Box<Matrix>(n,n));

  // Calculate S matrix
  for(int i=0;i<n;i++)
    for(int j=0;j<n;j++)
      (*R)(i,j) = 1;

  return R;
}

extern "C" closure builtin_function_equ(OperationArgs& Args)
{
  int n = *Args.evaluate_as<Int>(0);
    
  return EQU_Exchange_Function(n);
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


object_ptr<const Object> Empirical_Exchange_Function(const alphabet& a, istream& ifile)
{
  int n = a.size();

  object_ptr<Box<Matrix>> R(new Box<Matrix>(n,n));
  
  for(int i=0;i<n;i++)
    for(int j=0;j<i;j++) {
      ifile>>(*R)(i,j);
      (*R)(j,i) = (*R)(i,j);
    }

  return object_ptr<const Object>(R);
}

object_ptr<const Object> Empirical_Exchange_Function(const alphabet& a, const String& filename)
{
  checked_ifstream ifile(filename, "empirical rate matrix file");
  return Empirical_Exchange_Function(a,ifile);
}

extern "C" closure builtin_function_empirical(OperationArgs& Args)
{
  object_ptr<const alphabet> a = Args.evaluate_as<alphabet>(0);
  object_ptr<const String> S = Args.evaluate_as<String>(1);
  return Empirical_Exchange_Function(*a, *S);
}

object_ptr<const Object> PAM_Exchange_Function(const alphabet& a)
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

extern "C" closure builtin_function_pam(OperationArgs& Args)
{
  object_ptr<const alphabet> a = Args.evaluate_as<alphabet>(0);
  return PAM_Exchange_Function(*a);
}

object_ptr<const Object> JTT_Exchange_Function(const alphabet& a)
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

extern "C" closure builtin_function_jtt(OperationArgs& Args)
{
  object_ptr<const alphabet> a = Args.evaluate_as<alphabet>(0);
  return JTT_Exchange_Function(*a);
}

object_ptr<const Object> WAG_Exchange_Function(const alphabet& a)
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

extern "C" closure builtin_function_wag(OperationArgs& Args)
{
  object_ptr<const alphabet> a = Args.evaluate_as<alphabet>(0);
  return WAG_Exchange_Function(*a);
}

object_ptr<const Object> LG_Exchange_Function(const alphabet& a)
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

extern "C" closure builtin_function_lg(OperationArgs& Args)
{
  object_ptr<const alphabet> a = Args.evaluate_as<alphabet>(0);
  return WAG_Exchange_Function(*a);
}

extern "C" closure builtin_function_f3x4_frequencies(OperationArgs& Args)
{
  auto T = Args.evaluate_as<Triplets>(0);
  // The way alphabet is currently implemented, triplets must be triplets of nucleotides.

  auto pi1 = Args.evaluate_as<Vector<double>>(1);

  auto pi2 = Args.evaluate_as<Vector<double>>(2);

  auto pi3 = Args.evaluate_as<Vector<double>>(3);

  Vector<double> pi;
  pi.resize(T->size());
  for(int i=0;i<T->size();i++)
    pi[i] = (*pi1)[T->sub_nuc(i,0)] * (*pi2)[T->sub_nuc(i,1)] * (*pi3)[T->sub_nuc(i,2)];

  // Some triplets may be missing from the triplet alphabet (e.g. stop codons).  So renormalize.

  double scale = 1.0/sum(pi);
  for(double& d : pi)
    d *= scale;

  assert(std::abs(sum(pi) - 1.0) < 1.0e-9);

  return pi;
}

extern "C" closure builtin_function_gtr(OperationArgs& Args)
{
  object_ptr<const Nucleotides> N = Args.evaluate_as<Nucleotides>(0);
  double AG = *Args.evaluate_as<Double>(1);
  double AT = *Args.evaluate_as<Double>(2);
  double AC = *Args.evaluate_as<Double>(3);
  double GT = *Args.evaluate_as<Double>(4);
  double GC = *Args.evaluate_as<Double>(5);
  double TC = *Args.evaluate_as<Double>(6);

  assert(N->size()==4);

  object_ptr<Box<Matrix>> R(new Box<Matrix>(N->size(),N->size()));

  double total = AG + AT + AC + GT + GC + TC;

  (*R)(1,0) = (*R)(0,1) = AG/total;
  (*R)(2,0) = (*R)(0,2) = AT/total;
  (*R)(3,0) = (*R)(0,3) = AC/total;

  (*R)(2,1) = (*R)(1,2) = GT/total;
  (*R)(3,1) = (*R)(1,3) = GC/total;

  (*R)(3,2) = (*R)(2,3) = TC/total;

  return R;
}

extern "C" closure builtin_function_m0(OperationArgs& Args)
{
  object_ptr<const Codons> C = Args.evaluate_as<Codons>(0);
  object_ptr<const Box<Matrix>> S = Args.evaluate_as<Box<Matrix>>(1);
  double omega = *Args.evaluate_as<Double>(2);

  int n = C->size();

  object_ptr<Box<Matrix>> R ( new Box<Matrix>(n,n) );

  for(int i=0;i<n;i++) 
  {
    for(int j=0;j<i;j++) {
      int nmuts=0;
      int pos=-1;
      for(int p=0;p<3;p++)
	if (C->sub_nuc(i,p) != C->sub_nuc(j,p)) {
	  nmuts++;
	  pos=p;
	}
      assert(nmuts>0);
      assert(pos >= 0 and pos < 3);

      double rate=0.0;

      if (nmuts == 1) 
      {
	int l1 = C->sub_nuc(i,pos);
	int l2 = C->sub_nuc(j,pos);
	assert(l1 != l2);

	rate = (*S)(l1,l2);

	if (C->translate(i) != C->translate(j))
	  rate *= omega;	
      }

      (*R)(i,j) = (*R)(j,i) = rate;
    }
  }

  return R;
}

extern "C" closure builtin_function_plus_gwF(OperationArgs& Args)
{
  const alphabet& a = *Args.evaluate_as<alphabet>(0);

  double f = *Args.evaluate_as<Double>(1);

  object_ptr< const Vector<double> > pi_ = Args.evaluate_as< Vector<double> >(2);

  const int n = a.size();

  object_ptr<Box<Matrix>> R( new Box<Matrix>(n,n) );

  // compute frequencies
  vector<double> pi = *pi_;
  normalize(pi);
  assert(a.size() == pi.size());
    
  // compute transition rates
  valarray<double> pi_f(n);
  for(int i=0;i<n;i++)
    pi_f[i] = pow(pi[i],f);

  for(int i=0;i<n;i++)
    for(int j=0;j<n;j++)
      (*R)(i,j) = pi_f[i]/pi[i] * pi_f[j];

  // diagonal entries should have no effect
  for(int i=0;i<n;i++)
    (*R)(i,i) = 0;

  return R;
}
