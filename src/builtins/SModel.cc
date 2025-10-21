#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
//#define DEBUG_RATE_MATRIX
#include "computation/machine/args.H"
#include "math/exponential.H"
#include "sequence/alphabet.H"
#include "sequence/doublets.H"
#include "sequence/codons.H"
#include "util/io.H"
#include <valarray>
#include "dp/2way.H"
#include "util/range.H"
#include <unsupported/Eigen/MatrixFunctions>
#include "substitution/parsimony.H"

using std::vector;
using std::pair;
using std::istringstream;
using std::istream;
using std::valarray;

using std::cerr;
using std::endl;
using std::abs;

using Alphabet = PtrBox<alphabet>;

extern "C" closure builtin_function_compute_stationary_freqs(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& Q = arg0.as_<Box<Matrix>>();

    auto vpi = compute_stationary_freqs(Q);
    int n = vpi.size();

    // 4. Copy back to an EVector double;
    EVector pi(n);
    for(int i=0;i<n;i++)
        pi[i] = vpi[i];

    return pi;
}

extern "C" closure builtin_function_compute_check_stationary_freqs(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& Q = arg0.as_<Box<Matrix>>();
    int n = Q.size1();
    assert(Q.size2() == n);

    // Set up equations Q pi = 0, sum(pi) = 1

    // 1. QQ = Q, but with an extra row of 1's
    Eigen::MatrixXd QQ(n+1,n);
    for(int i=0;i<n;i++)
        for(int j=0;j<n;j++)
            QQ(i,j) = Q(j,i); // transpose -- why?
    // This sets up the sum(pi)
    for(int j=0;j<n;j++)
        QQ(n,j) = 1;

    // 2. b = 0*n + 1
    Eigen::VectorXd b(n+1);
    for(int i=0;i<n;i++)
        b[i] = 0;
    // This sets up the sum(pi)
    b[n] = 1;

    // 3. Solve the equations
    // Eigen::VectorXd epi = QQ.ColPivHouseholderQr.solve(b);  Maybe faster?
    Eigen::VectorXd epi = QQ.fullPivLu().solve(b);

    // 4. Copy back to an EVector double;
    EVector pi(n);
    for(int i=0;i<n;i++)
        pi[i] = epi[i];

    double err = (QQ * epi - b).cwiseAbs().sum();

    // Compare with known pi

    auto arg1 = Args.evaluate(1);
    auto& pi0 = arg1.as_<EVector>();

    // 2. b = 0*n + 1
    Eigen::VectorXd epi0(n);
    for(int i=0;i<n;i++)
        epi0[i] = pi0[i].as_double();

    double err2 = (QQ * epi0 - b).cwiseAbs().sum();
    double err3 = (epi - epi0).cwiseAbs().sum();

    if (err > 1.0e-5 or err2 > 1.0e-5 or err3 > 1.0e-5)
    {
        std::cerr<<"err1 = "<<err<<"   err2 = "<<err2<<"   err3 = "<<err3<<"\n";
    }
    return pi;
}

extern "C" closure builtin_function_checkStationary(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& Q = arg0.as_<Box<Matrix>>();

    auto arg1 = Args.evaluate(1);
    auto pi = (vector<double>)arg1.as_<EVector>();

    return { checkStationary(Q,pi) };
}

extern "C" closure builtin_function_checkReversible(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& Q = arg0.as_<Box<Matrix>>();

    auto arg1 = Args.evaluate(1);
    auto pi = (vector<double>)arg1.as_<EVector>();

    return { checkReversible(Q,pi) };
}

extern "C" closure builtin_function_getEquilibriumRate(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const alphabet& a = *arg0.as_<Alphabet>();

    auto arg1 = Args.evaluate(1);
    auto& smap = arg1.as_< EVector >();

    auto arg2 = Args.evaluate(2);
    const Matrix& Q = arg2.as_< Box<Matrix> >();

    auto pi = vector<double> (Args.evaluate(3).as_<EVector>() );

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
		if (smap[s1].as_int() != smap[s2].as_int())
		    temp += Q(s1,s2);

	    scale += temp*pi[s1];
	}
    }

    double W = a.width();

    if (dynamic_cast<const RNAEdits*>(&a))
	W = 1;

    return {scale/W};
}

extern "C" closure builtin_function_rna_16a_exchange(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& D = *arg0.poly_as_<alphabet,Doublets>();

    // 2 changes: between matches/wobbles (both transitions)
    double alpha_D = Args.evaluate(2).as_double();
    // 2 changes: between matches/wobbles (both transversions)
    double beta = Args.evaluate(3).as_double();
    // 2 changes: mismatch and anything: ZERO

    // 1 change: between matches (must be transition between match and wobble)
    double alpha_S = Args.evaluate(1).as_double();
    // 1 change: between match and mismatch
    double gamma = Args.evaluate(4).as_double();
    // 1 change: between mismatch and mismatch
    double epsilon = Args.evaluate(5).as_double();

    const int n = D.size();

    object_ptr<Box<Matrix>> R( new Box<Matrix>(n,n) );

    for(int i=0;i<n;i++)
    {
	(*R)(i,i) = 0;
	for(int j=i+1;j<n;j++)
	{
	    if (i==j) continue;

	    bool m1 = D.is_watson_crick(i) or D.is_wobble_pair(i);
	    bool m2 = D.is_watson_crick(j) or D.is_wobble_pair(j);

	    double x = 0;
	    if (D.n_changes(i,j) == 2)
	    {
		if (m1 and m2)
		{
		    int i0 = D.sub_nuc(i,0);
		    int i1 = D.sub_nuc(i,1);

		    int j0 = D.sub_nuc(j,0);
		    int j1 = D.sub_nuc(j,1);

		    auto& N = D.getNucleotides();
		    if (N.transition(i0,j0))
		    {
			// double transition -> alpha_D
			x = alpha_D;
			assert(N.transition(i1,j1));
		    }
		    else
		    {
			// double transversion -> beta
			x = beta;
			assert(N.transversion(i1,j1));
		    }
		}
		else {
		    // double change between mismatch and something
		    x = 0;
		}
	    }
	    else if (D.n_changes(i,j) == 1)
	    {
		int n = (m1?1:0) + (m2?1:0);
		//    match <->    match = alpha_S
		if (n == 2)
		    x = alpha_S;
		// mismatch <->    match = gamma
		else if (n==1)
		    x = gamma;
		// mismatch <-> mismatch = epsilon
		else
		    x = epsilon;
	    }

	    (*R)(i,j) = (*R)(j,i) = x;
	}
    }

    return R;
}

extern "C" closure builtin_function_singlet_to_doublet_rates(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& D = *arg0.poly_as_<alphabet,Doublets>();

    auto arg1 = Args.evaluate(1);
    const Matrix& R1 = arg1.as_<Box<Matrix>>();

    auto arg2 = Args.evaluate(2);
    const Matrix& R2 = arg2.as_<Box<Matrix>>();

    // The way alphabet is currently implemented, doublets must be doublets of nucleotides.
    assert(R1.size1() == 4);
    assert(R1.size2() == 4);
    assert(R2.size1() == 4);
    assert(R2.size2() == 4);

    const int n = D.size();

    object_ptr<Box<Matrix>> R( new Box<Matrix>(n,n) );

    for(int i=0;i<n;i++)
    {
	double sum = 0;
	for(int j=0;j<n;j++)
	{
	    if (i==j) continue;
	    int nmuts=0;
	    int from=-1;
	    int to=-1;
	    int pos=-1;
	    for(int p=0;p<2;p++)
		if (D.sub_nuc(i,p) != D.sub_nuc(j,p))
		{
		    nmuts++;
		    pos = p;
		    from = D.sub_nuc(i,p);
		    to = D.sub_nuc(j,p);
		}

	    double r = 0;
	    if (nmuts == 1)
	    {
		if (pos == 0)
		    r = R1(from,to);
		else if (pos == 1)
		    r = R2(from,to);
		else
		    std::abort();
	    }
	    (*R)(i,j) = r;
	    sum += r;
	}
	(*R)(i,i) = -sum;
    }

    return R;
}


extern "C" closure builtin_function_singlet_to_triplet_rates(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& T = *arg0.poly_as_<alphabet,Triplets>();
  
    auto arg1 = Args.evaluate(1);
    const Matrix& R1 = arg1.as_<Box<Matrix>>();

    auto arg2 = Args.evaluate(2);
    const Matrix& R2 = arg2.as_<Box<Matrix>>();

    auto arg3 = Args.evaluate(3);
    const Matrix& R3 = arg3.as_<Box<Matrix>>();

    // The way alphabet is currently implemented, triplets must be triplets of nucleotides.
    assert(R1.size1() == 4);
    assert(R1.size2() == 4);
    assert(R2.size1() == 4);
    assert(R2.size2() == 4);
    assert(R3.size1() == 4);
    assert(R3.size2() == 4);

    const int n = T.size();

    object_ptr<Box<Matrix>> R( new Box<Matrix>(n,n) );

    for(int i=0;i<n;i++)
    {
	double sum = 0;
	for(int j=0;j<n;j++)
	{
	    if (i==j) continue;
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
	    (*R)(i,j) = r;
	    sum += r;
	}
	(*R)(i,i) = -sum;
    }

    return R;
}

// multiNucleotideMutationRates :: TripletAlphabet -> Double -> Double -> Matrix Double -> EVector Double -> Matrix Double
extern "C" closure builtin_function_multiNucleotideMutationRates(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& T = *arg0.poly_as_<alphabet,Triplets>();

    double v2 = Args.evaluate(1).as_double();
    double v3 = Args.evaluate(2).as_double();

    auto arg3 = Args.evaluate(3);
    const Matrix& R1 = arg3.as_<Box<Matrix>>();

    auto arg4 = Args.evaluate(4);
    auto pi1 = arg4.as_<EVector>();

    // Compute the average rate at equilibrium for R1.
//    double rate = 0;
//    for(int i=0;i<4;i++)
//    {
//        double sum = 0;
//        for(int j=0;j<4;j++)
//            if (i!=j) sum += R1(i,j);
//        rate += pi1[i].as_double() * sum;
//    }

    // The way alphabet is currently implemented, triplets must be triplets of nucleotides.
    assert(v2 >= 0);
    assert(v3 >= 0);

    assert(R1.size1() == 4);
    assert(R1.size2() == 4);

    assert(pi1.size() == 4);

    const int n = T.size();

    object_ptr<Box<Matrix>> R( new Box<Matrix>(n,n) );

    // Compute sums for unscaled 2-nuc and 3-nuc mutations.
    double sum2 = 0;
    double sum3 = 0;
    for(int i=0;i<n;i++)
    {
	for(int j=0;j<n;j++)
        {
	    if (i==j) continue;

	    int nmuts=0;
	    for(int p=0;p<3;p++)
		if (T.sub_nuc(i,p) != T.sub_nuc(j,p))
		    nmuts++;

            if (nmuts == 1) continue;

            double prod = 1;
            for(int p=0;p<3;p++)
            {
                if (T.sub_nuc(i,p) != T.sub_nuc(j,p))
                {
                    int to = T.sub_nuc(j,p);
                    prod *= pi1[to].as_double();
                }
            }

            if (nmuts == 2)
                sum2 += prod;
            else if (nmuts == 3)
                sum3 += prod;
        }
    }

    // Compute the pairwise rates.
    for(int i=0;i<n;i++)
    {
	double sum = 0;
	for(int j=0;j<n;j++)
	{
	    if (i==j) continue;

	    int nmuts=0;
	    for(int p=0;p<3;p++)
		if (T.sub_nuc(i,p) != T.sub_nuc(j,p))
		    nmuts++;

            std::optional<double> r;
	    if (nmuts == 1)
	    {
                for(int p=0;p<3;p++)
                {
                    if (T.sub_nuc(i,p) != T.sub_nuc(j,p))
                    {
                        int from = T.sub_nuc(i,p);
                        int to = T.sub_nuc(j,p);
                        r = R1(from,to);
                    }
		}
	    }
            else
            {
                double prod = 1;
                for(int p=0;p<3;p++)
                {
                    if (T.sub_nuc(i,p) != T.sub_nuc(j,p))
                    {
                        int to = T.sub_nuc(j,p);
                        prod *= pi1[to].as_double();
                    }
                }

                if (nmuts == 2)
                    r = v2 * prod/sum2;
                else if (nmuts == 3)
                    r = v3 * prod/sum3;
                else
                    std::abort();
            }

	    (*R)(i,j) = r.value();
	    sum += *r;
	}
	(*R)(i,i) = -sum;
    }

    return R;
}


vector<int> make_edit_map(const EVector& edit_pairs, int n)
{
    // 1. For each (i,j) in edits, we edit i -> j
    vector<int> edit(n, -1);
    for(auto& edit_pair: edit_pairs)
    {
        auto P = edit_pair.as_<EPair>();
        int i = P.first.as_int();
        int j = P.second.as_int();

        if (i < 0 or i >= n)
            throw myexception()<<"rna_editting_rates: nucleotide "<<i<<" not in range [0,"<<n<<")!";
        if (j < 0 or j >= n)
            throw myexception()<<"rna_editting_rates: nucleotide "<<j<<" not in range [0,"<<n<<")!";
        if (edit[i] != -1)
            throw myexception()<<"rna_editting_rates: nucleotide "<<i<<" mentioned twice!";

        edit[i] = j;
    }

    // 2. If i is not in edits_map, we edit i -> i.
    for(int i=0;i<n;i++)
    {
        if (edit[i] == -1)
            edit[i] = i;
    }

    return edit;
}

extern "C" closure builtin_function_rna_editting_rates(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& D = *arg0.poly_as_<alphabet,RNAEdits>();
    const int n = D.size();
    assert(D.getNucleotides().size() == 4);

    auto arg1 = Args.evaluate(1);
    const Matrix& Q_nuc = arg1.as_<Box<Matrix>>();
    // The way alphabet is currently implemented, doublets must be doublets of nucleotides.
    assert(Q_nuc.size1() == 4);
    assert(Q_nuc.size2() == 4);

    auto arg2 = Args.evaluate(2);
    const EVector& edit_pairs = arg2.as_<EVector>();
    vector<int> edit = make_edit_map(edit_pairs, 4);

    double rnaRate = Args.evaluate(3).as_double();
    assert(rnaRate >= 0);

    object_ptr<Box<Matrix>> Q( new Box<Matrix>(n,n) );

    for(int i=0;i<n;i++)
    {
        int i1 = D.sub_nuc(i,0);
        int i2 = D.sub_nuc(i,1);
        bool i_ok = (i2 == edit[i1]);

        double sum = 0;
	for(int j=0;j<n;j++)
	{
	    if (i==j) continue;

            int j1 = D.sub_nuc(j,0);
            int j2 = D.sub_nuc(j,1);

            bool j_ok = (j2 == edit[j1]);

	    double r = 0;

            if (i_ok and j_ok)
                r = Q_nuc(i1, j1);
                
	    if (i2 != j2)
		r *= rnaRate;

	    (*Q)(i,j) = r;
	    sum += r;
	}
	(*Q)(i,i) = -sum;
    }

    return Q;
}


extern "C" closure builtin_function_rna_editting_pi(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& D = *arg0.poly_as_<alphabet,RNAEdits>();
    const int n = D.size();

    auto arg1 = Args.evaluate(1);
    const auto& nuc_pi = arg1.as_<EVector>();
    // The way alphabet is currently implemented, doublets must be doublets of nucleotides.
    assert(nuc_pi.size() == 4);

    auto arg2 = Args.evaluate(2);
    const EVector& edit_pairs = arg2.as_<EVector>();
    vector<int> edit = make_edit_map(edit_pairs, 4);

    vector<double> pi( n );
    for(int i = 0; i < n; i++)
    {
        int i1 = D.sub_nuc(i,0);
        int i2 = D.sub_nuc(i,1);
        bool i_ok = (i2 == edit[i1]);

        if (i_ok)
            pi[i] = nuc_pi[i1].as_double();
        else
            pi[i] = 0;
    }

    assert(std::abs(sum(pi) - 1.0) < 1.0e-9);
    return EVector(pi);
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
    int n = Args.evaluate(0).as_int();
    
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


// Lets suppose that we have a list of `n_levels` different rate matrices with:
//    * rate matrices Qs[leven]
//    * pi vectors pis[level]
//    * state maps smaps[level]
// for k=1..K.

void inc_modulated_states_matrix(int& r, int& level, int& state, const EVector& Qs)
{
    r++;
    state++;
    if (state < Qs[level].as_<Box<Matrix>>().size1())
        ;
    else
    {
        level++;
        state=0;
    }
}

void inc_modulated_states_vec(int& r, int& level, int& state, const EVector& pis)
{
    r++;
    state++;
    if (state < pis[level].as_<EVector>().size())
        ;
    else
    {
        level++;
        state=0;
    }
}

// We switch between these different "levels" with KxK rate matrix `rates_between`.
// The equilibrium frequencies for each level in `R` are given by `level_frequencies`.
extern "C" closure builtin_function_modulated_markov_rates(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& Qs = arg0.as_<EVector>();
    int n_levels = Qs.size();

    auto arg1 = Args.evaluate(1);
    auto& rates_between = arg1.as_<Box<Matrix>>();
    assert(rates_between.size1() == n_levels);
    assert(rates_between.size2() == n_levels);

    int total_states = 0;
    for(int l = 0; l < n_levels; l++)
    {
        auto& Q = Qs[l].as_<Box<Matrix>>();
        int n_states_for_level = Q.size1();
        assert(Q.size2() == n_states_for_level);

        total_states += n_states_for_level;
    }

    auto R = new Box<Matrix>(total_states, total_states);
    for(int r1=0, l1=0, s1=0; r1 < total_states; inc_modulated_states_matrix(r1,l1,s1,Qs))
    {
        double sum = 0;
        for(int r2=0, l2=0, s2=0; r2 < total_states; inc_modulated_states_matrix(r2,l2,s2,Qs))
        {
            if (r1 == r2) continue;

            double rate = 0;
            if (l1 != l2 and s1 != s2)
                ;
            else if (l1 != l2)
                rate = rates_between(l1,l2);
            else
            {
                assert(l1 == l2);
                auto& Q = Qs[l1].as_<Box<Matrix>>();
                assert(s1 != s2);
                rate = Q(s1,s2);
            }
            assert(rate >= 0);
            (*R)(r1,r2) = rate;
            sum += rate;
        }
        (*R)(r1,r1) = -sum;
    }

    return R;
}

extern "C" closure builtin_function_modulated_markov_pi(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& pis = arg0.as_<EVector>();
    int n_levels = pis.size();

    auto arg1 = Args.evaluate(1);
    auto& level_probs = arg1.as_<EVector>();
    assert(level_probs.size() == n_levels);

    int total_states = 0;
    for(int l = 0; l < n_levels; l++)
    {
        auto& pi = pis[l].as_<EVector>();
        int n_states_for_level = pi.size();

        total_states += n_states_for_level;
    }

    vector<double> pi(total_states);
    for(int r=0, l=0, s=0; r < total_states; inc_modulated_states_vec(r,l,s,pis))
        pi[r] = level_probs[l].as_double() * pis[l].as_<EVector>()[s].as_double();

    assert(std::abs(sum(pi) - 1.0) < 1.0e-9);
    return EVector(pi);
}

extern "C" closure builtin_function_modulated_markov_smap(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& smaps = arg0.as_<EVector>();

    EVector new_smap;
    for(auto& smap: smaps)
        for(auto& x: smap.as_<EVector>())
            new_smap.push_back(x);

    return new_smap;
}

template <typename T>
T sum(const EVector& v);

template<> double sum<>(const EVector& v)
{
    double d=0;
    for(auto& vv: v)
	d += vv.as_double();
    return d;
}

void normalize(EVector& v)
{
    double scale = 1.0/sum<double>(v);

    for(auto& vv: v)
	vv = vv.as_double()*scale;
}


object_ptr<const Object> Empirical_Exchange_Function(const alphabet& a, istream& ifile)
{
    int n = a.size();

    object_ptr<Box<Matrix>> R(new Box<Matrix>(n,n));
  
    int k=0;
    for(int i=0;i<n;i++)
	for(int j=0;j<i;j++)
	{
	    if (ifile>>(*R)(i,j))
	    {
		(*R)(j,i) = (*R)(i,j);
		k++;
	    }
	    else
		throw myexception()<<"Read "<<k<<" empirical exchangabilities.";
	}

    return object_ptr<const Object>(R);
}

object_ptr<const Object> Empirical_Frequencies_Function(const alphabet& a, istream& ifile)
{
    int n = a.size();

    // Skip the exchangeabilities
    int k=0;
    for(int i=0;i<n;i++)
	for(int j=0;j<i;j++) {
	    double d;
	    if (ifile>>d)
		k++;
	    else
		throw myexception()<<"Read "<<k<<" empirical exchangabilities.";
	}

    // Get the frequencies
    object_ptr<EVector> F(new EVector(a.size()));

    for(int i=0;i<a.size();i++)
    {
	double d;
	if (ifile>>d)
	    (*F)[i] = d;
	else
	    throw myexception()<<"Read "<<i<<" empirical frequencies.";
    }

    normalize(*F);

    return object_ptr<const Object>(F);
}

object_ptr<const Object> Empirical_Exchange_Function(const alphabet& a, const String& filename)
{
    checked_ifstream ifile(filename.value(), "empirical rate matrix file");
    return Empirical_Exchange_Function(a,ifile);
}

extern "C" closure builtin_function_empirical(OperationArgs& Args)
{
    auto a = Args.evaluate(0);
    auto S = Args.evaluate(1);
    return Empirical_Exchange_Function(*a.as_<Alphabet>(), S.as_<String>());
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
    auto a = Args.evaluate(0);
    return PAM_Exchange_Function(*a.as_<Alphabet>());
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
    auto a = Args.evaluate(0);
    return JTT_Exchange_Function(*a.as_<Alphabet>());
}

const char* wag_string =
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
\
0.0866279 0.043972  0.0390894 0.0570451 0.0193078 0.0367281 0.0580589 0.0832518 0.0244313 0.048466  0.086209  0.0620286 0.0195027 0.0384319 0.0457631 0.0695179 0.0610127 0.0143859 0.0352742 0.0708956\
";


extern "C" closure builtin_function_wag(OperationArgs& Args)
{
    auto a = Args.evaluate(0);
    istringstream file(wag_string);
    return Empirical_Exchange_Function(*a.as_<Alphabet>(), file);
}

extern "C" closure builtin_function_wag_frequencies(OperationArgs& Args)
{
    auto a = Args.evaluate(0);
    istringstream file(wag_string);
    return Empirical_Frequencies_Function(*a.as_<Alphabet>(), file);
}


const char* lg_string =	"0.425093 \
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
0.079066 0.055941 0.041977 0.053052 0.012937 0.040767 0.071586 0.057337 0.022355 0.062157 0.099081 0.064600 0.022951 0.042302 0.044040 0.061197 0.053287 0.012066 0.034155 0.069147";

extern "C" closure builtin_function_lg(OperationArgs& Args)
{
    auto a = Args.evaluate(0);
    istringstream file(lg_string);
    return Empirical_Exchange_Function(*a.as_<Alphabet>(), file);
}

extern "C" closure builtin_function_lg_frequencies(OperationArgs& Args)
{
    auto a = Args.evaluate(0);
    istringstream file(lg_string);
    return Empirical_Frequencies_Function(*a.as_<Alphabet>(), file);
}

extern "C" closure builtin_function_f3x4_frequencies(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& T = *arg0.poly_as_<alphabet,Triplets>();
    // The way alphabet is currently implemented, triplets must be triplets of nucleotides.

    auto arg1 = Args.evaluate(1);
    auto pi1 = arg1.as_<EVector>();

    int nuc_size = T.getNucleotides().size();

    if (pi1.size() != nuc_size)
	throw myexception()<<"f3x4_frequencies:site 1:expected "<<nuc_size<<" frequencies, but got "<<pi1.size()<<"!";

    auto arg2 = Args.evaluate(2);
    auto pi2 = arg2.as_<EVector>();

    if (pi2.size() != nuc_size)
	throw myexception()<<"f3x4_frequencies:site 2:expected "<<nuc_size<<" frequencies, but got "<<pi2.size()<<"!";

    auto arg3 = Args.evaluate(3);
    auto pi3 = arg3.as_<EVector>();

    if (pi3.size() != nuc_size)
	throw myexception()<<"f3x4_frequencies:site 3:expected "<<nuc_size<<" frequencies, but got "<<pi3.size()<<"!";

    EVector pi;
    pi.resize(T.size());
    double sum = 0;
    for(int i=0;i<T.size();i++)
    {
	double x = pi1[T.sub_nuc(i,0)].as_double() * pi2[T.sub_nuc(i,1)].as_double() * pi3[T.sub_nuc(i,2)].as_double();
	pi[i] = x;
	sum += x;
    }

    // Some triplets may be missing from the triplet alphabet (e.g. stop codons).  So renormalize.

    double scale = 1.0/sum;
    for(auto& d : pi)
	d = d.as_double() * scale;

//    assert(std::abs(sum(pi) - 1.0) < 1.0e-9);

    return pi;
}

extern "C" closure builtin_function_f2x4_frequencies(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& D = *arg0.poly_as_<alphabet,Doublets>();
    // The way alphabet is currently implemented, triplets must be triplets of nucleotides.

    auto arg1 = Args.evaluate(1);
    auto pi1 = arg1.as_<EVector>();

    int nuc_size = D.getNucleotides().size();

    if (pi1.size() != nuc_size)
	throw myexception()<<"f2x4_frequencies:site 1:expected "<<nuc_size<<" frequencies, but got "<<pi1.size()<<"!";

    auto arg2 = Args.evaluate(2);
    auto pi2 = arg2.as_<EVector>();

    if (pi2.size() != nuc_size)
	throw myexception()<<"f2x4_frequencies:site 2:expected "<<nuc_size<<" frequencies, but got "<<pi2.size()<<"!";

    EVector pi;
    pi.resize(D.size());
    double sum = 0;
    for(int i=0;i<D.size();i++)
    {
	double x = pi1[D.sub_nuc(i,0)].as_double() * pi2[D.sub_nuc(i,1)].as_double();
	pi[i] = x;
	sum += x;
    }

    // Some triplets may be missing from the doublet alphabet (e.g. mismatches). So renormalize

    double scale = 1.0/sum;
    for(auto& d : pi)
	d = d.as_double() * scale;

//    assert(std::abs(sum(pi) - 1.0) < 1.0e-9);

    return pi;
}

extern "C" closure builtin_function_gtr_sym(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& S = arg0.as_<EVector>();
    int n = Args.evaluate(1).as_int();

    auto R = new Box<Matrix>(n,n);
    if (S.size() != n*(n-1)/2)
	throw myexception()<<"Matrix of size "<<n<<" x "<<n<<" should have "<<n*(n-1)/2<<" exchangeabilities, but got "<<S.size()<<"!";

    int k=0;
    for(int i=0;i<n;i++)
    {
	(*R)(i,i) = 0;
	for(int j=i+1;j<n;j++)
	{
	    double x = S[k++].as_double();
	    (*R)(i,j) = (*R)(j,i) = x;
	}
    }

    return {R};
}

extern "C" closure builtin_function_non_rev_from_vec(OperationArgs& Args)
{
    int n = Args.evaluate(0).as_int();

    auto arg0 = Args.evaluate(1);
    auto& S = arg0.as_<EVector>();

    auto R = new Box<Matrix>(n,n);
    if (S.size() != n*(n-1))
	throw myexception()<<"Matrix of size "<<n<<" x "<<n<<" should have "<<n*(n-1)<<" off-diagonal entries, but got "<<S.size()<<"!";

    int k=0;
    for(int i=0;i<n;i++)
    {
	(*R)(i,i) = 0;
	for(int j=0;j<n;j++)
	{
            if (i == j) continue;

	    (*R)(i,j) = S[k++].as_double();
	}
    }

    return {R};
}

// Currently we are assuming that one of these matrices is symmetric, so that we don't have to update the frequencies.
extern "C" closure builtin_function_fixupDiagonalRates(OperationArgs& Args)
{
    auto arg1 = Args.evaluate(0);
    const Matrix& m1 = arg1.as_<Box<Matrix>>();

    auto m2 = new Box<Matrix>(m1);

    int n = m2->size1();

    if (m2->size2() != n)
	throw myexception()<<"Rate matrix should be square, but has size ("<<n<<","<<m2->size2()<<")";

    for(int i=0;i<n;i++)
    {
	double sum = 0;
	for(int j=0;j<n;j++)
	    if (j != i)
		sum += (*m2)(i,j);
	(*m2)(i,i) = -sum;
    }

    return m2;
}

extern "C" closure builtin_function_dNdS_matrix(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const Codons& C = *arg0.poly_as_<alphabet,Codons>();

    double omega = Args.evaluate(1).as_double();

    int n = C.size();

    auto R = new Box<Matrix>(n,n);

    for(int i=0;i<n;i++)
	for(int j=0;j<n;j++)
	    (*R)(i,j) = (C.translate(i) == C.translate(j))?1.0:omega;

    return R;
}

extern "C" closure builtin_function_singletToTripletSym(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& C = *arg0.poly_as_<alphabet,Triplets>();

    auto arg1 = Args.evaluate(1);
    const Matrix& S = arg1.as_<Box<Matrix>>();

    int n = C.size();

    auto R = new Box<Matrix>(n,n);

    for(int i=0;i<n;i++) 
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

	    if (nmuts == 1) 
	    {
		int l1 = C.sub_nuc(i,pos);
		int l2 = C.sub_nuc(j,pos);
		assert(l1 != l2);

		rate = S(l1,l2);
	    }

	    (*R)(i,j) = (*R)(j,i) = rate;
	}
    }

    return R;
}

extern "C" closure builtin_function_plus_gwf_matrix(OperationArgs& Args)
{
    auto pi = vector<double>( Args.evaluate(0).as_<EVector>() );

    double f = Args.evaluate(1).as_double();

    int n = pi.size();

    auto R = new Box<Matrix>(n,n);

    // compute frequencies
    normalize(pi);
    
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

double bound(double low, double high, double x)
{
    if (x < low) return low;
    if (x > high) return high;
    return x;
}

// Q0 w
// Here S[I,J] = F[J] - F[I] = 2Nf[j] - 2NF[i] = 2N*s[i,j]
extern "C" closure builtin_function_mut_sel_q(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const Matrix& Q0 = arg0.as_< Box<Matrix> >();
    assert(Q0.size1() == Q0.size2());
    int n = Q0.size1();

    auto F   = vector<double>( Args.evaluate(1).as_< EVector >() );
    for(auto& f: F)
	f = bound(-20,20,f);

    assert(F.size() == n);

    auto Q_ = new Box<Matrix>(n,n);
    Matrix& Q = *Q_;

    for(int i=0;i<n;i++)
    {
	double sum = 0;
	for(int j=0;j<n;j++)
	{
	    if (i==j) continue;

	    double rate = Q0(i,j);

	    // x = wj/wi    log(x)/(1-1/x)
	    // y = wi/wj   -log(y)/(1-y)
	    // 1+z = y     -log(1+z)/-z = log1p(z)/z   z = y-1 = (wi/wj)-1
	    double S = F[j] - F[i];
	    if (std::abs(S) < 0.0001)
		rate *= ( 1.0 + S/2 + (S*S)/12 - (S*S*S*S)/720 );
	    else
		rate *= -S/expm1(-S);

	    Q(i,j) = rate;

	    sum += Q(i,j);
	}
	Q(i,i) = -sum;
    }

    return Q_;
}

// pi0 w
extern "C" closure builtin_function_mut_sel_pi(OperationArgs& Args)
{
    auto pi0 = vector<double>( Args.evaluate(0).as_< EVector >() );

    auto F   = vector<double>( Args.evaluate(1).as_< EVector >() );
    for(auto& f: F)
	f = bound(-20,20,f);

    assert(pi0.size() == F.size());

    // compute frequencies
    vector<double> pi = pi0;

    double Fmax = max(F);

    for(int i=0; i<pi.size(); i++)
	pi[i] *= exp(F[i]-Fmax);

    normalize(pi);
    return EVector(pi);
}

/*
// codon_a nuc_pi omega nuc_q nuc_pi
extern "C" closure builtin_function_fMutSel_q2(OperationArgs& Args)
{
object_ptr<const Codons> C_ = Args.evaluate().as_<Codons>(0);
const Codons& C = *C_;
int N = C.size();
  
object_ptr< const Vector<double> > codon_pi_ = Args.evaluate().as_< Vector<double> >(1);
const Vector<double>& codon_pi = *codon_pi_;
assert(codon_pi.size() == N);

vector<double> nuc_log_pi;
for(int i=0;i<n;i++)
nuc_log_pi[i] = log(nuc_log_pi[i]);

// codon_pi[i] = nuc_pi[i1] * nuc_pi[i2] * nuc_pi[i3] * exp(codon_w[i]) * C;
// codon_w[i] - log(C) = log(codon_pi[i]) - log(nuc_pi[i1]) - log(nuc_pi[i2]) - log(nuc_pi[i3]
vector<double> codon_w(N);
for(int i=0;i<N;i++)
{
double x = log(codon_pi[i]);
for(int j=0;j<3;j++)
x -= nuc_log_pi[C.sub_nuc(i,j)];
codon_w[i] = x;
}
  
double omega = Args.evaluate(2).as_double();

object_ptr<const Box<Matrix>> nuc_Q_ = Args.evaluate().as_<Box<Matrix>>(3);
const Matrix& nuc_Q = *nuc_Q_;
assert(nuc_Q.size1() == nuc_Q.size2());
assert(nuc_Q.size1() == C.getNucleotides().size());

vector<double> log_codon_w(N);
for(int i=0;i<N;i++)
log_codon_w[i] = log(codon_w[i]);
  
object_ptr<Box<Matrix>> Q_(new Box<Matrix>(N,N));
Matrix& Q = *Q_;

for(int i=0;i<N;i++)
{
double sum = 0;
for(int j=0;j<N;j++)
{
if (i==j) continue;

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

if (nmuts == 1) 
{
int l1 = C.sub_nuc(i,pos);
int l2 = C.sub_nuc(j,pos);
assert(l1 != l2);

rate = nuc_Q(l1,l2);

rate *= (log_codon_w[j] - log_codon_w[i])/(codon_w[j] - codon_w[i])*codon_w[j];

if (C.translate(i) != C.translate(j))
rate *= omega;	
}

Q(i,j) = rate;
      
sum += Q(i,j);
}
Q(i,i) = -sum;
}

return Q_;
}
*/

extern "C" closure builtin_function_average_frequency(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const auto& WF = arg0.as_<Box<Matrix>>();

    const int n_models = WF.size1();
    const int n_states = WF.size2();

    auto* ave_f = new EVector(n_states);
    for(int s=0;s<n_states;s++)
    {
	double total = 0;
	for(int m=0;m<n_models;m++)
	    total += WF(m,s);
	(*ave_f)[s] = total;
    }

    return ave_f;
}

extern "C" closure builtin_function_weightedFrequencyMatrixRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const auto& D = arg0.as_<EVector>();

    auto arg1 = Args.evaluate(1);
    const auto& F = arg1.as_<EVector>();
    // cache matrix of frequencies

    assert(D.size() == F.size());

    const int n_models = F.size();
    const int n_states = F[0].as_<EVector>().size();

    auto *WF = new Box<Matrix>(n_models, n_states);

    for(int m=0;m<n_models;m++) {
	double p = D[m].as_double();
	const auto& f = F[m].as_<EVector>();
	for(int s=0;s<n_states;s++) 
	    (*WF)(m,s) = p*f[s].as_double();
    }
    return WF;
}

extern "C" closure builtin_function_frequencyMatrixRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const auto& F = arg0.as_<EVector>();
    // cache matrix of frequencies

    const int n_models = F.size();
    const int n_states = F[0].as_<EVector>().size();

    auto *FF = new Box<Matrix>(n_models, n_states);

    for(int m=0;m<n_models;m++) {
	const auto& f = F[m].as_<EVector>();
	for(int s=0;s<n_states;s++) 
	    (*FF)(m,s) = f[s].as_double();
    }
    return FF;
}

