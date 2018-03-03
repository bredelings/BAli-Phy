#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/computation.H"
#include "dp/2way.H"
#include "imodel/imodel.H"
#include "computation/expression/expression.H"
#include <boost/dynamic_bitset.hpp>
#include "alignment/alignment.H"

extern "C" closure builtin_function_pairwise_alignment_probability_from_counts(OperationArgs& Args)
{
    const matrix<int>& counts = Args.evaluate(0).as_<Box<matrix<int>>>();
    const indel::PairHMM& Q = Args.evaluate(1).as_<indel::PairHMM>();

    using namespace A2;

    log_double_t P=1;

    // Account for S-? start probability
    for(int i=0;i<Q.size2();i++)
	if (counts(states::S,i))
	    P *= Q.start(i);

    // Account for the mass of transitions
    for(int i=0;i<3;i++)
	for(int j=0;j<3;j++) {
	    log_double_t Qij = Q(i,j);
	    // FIXME - if we propose really bad indel parameters, we can get log(Q_ij) where Qij == 0
	    if (counts(i,j))
		P *= pow(Qij,counts(i,j));
	}
  
    // Account for ?-E end probability
    if (not counts(states::S,states::E))
	for(int i=0;i<Q.size1();i++)
	    if (counts(i,states::E))
		P *= Q(i,states::E);

    return {P};
}

extern "C" closure builtin_function_pairwise_alignment_length1(OperationArgs& Args)
{
    return {Args.evaluate(0).as_<pairwise_alignment_t>().length1()};
}

extern "C" closure builtin_function_pairwise_alignment_length2(OperationArgs& Args)
{
    return {Args.evaluate(0).as_<pairwise_alignment_t>().length2()};
}

extern "C" closure builtin_function_transition_counts(OperationArgs& Args)
{
    const pairwise_alignment_t& A = Args.evaluate(0).as_<pairwise_alignment_t>();

    Box<matrix<int>> counts(5,5,0);

    using namespace A2;

    int prev = states::S;
    for(int column=0;column<A.size();column++)
    {
	counts(prev,A.get_state(column))++;
	prev = A.get_state(column);
    }
    counts(prev, states::E)++;

    return counts;
}

using std::vector;

extern "C" closure builtin_function_rs07_lengthp(OperationArgs& Args)
{
    double e = Args.evaluate(0).as_double();
    if (e < 0.0)
	throw myexception()<<"Error: mean indel length cannot be < 1, but was set to "<<1.0/(1.0-e)<<"!";

    int l = Args.evaluate(1).as_int();

    if (l < 0)
	return {0.0};
    else if (l==0)
	return {1.0};
    else
	return {1.0-e};
}

extern "C" closure builtin_function_rs07_branch_HMM(OperationArgs& Args)
{
    double e = Args.evaluate(0).as_double();
    if (e < 0.0)
	throw myexception()<<"Error: mean indel length cannot be < 1, but was set to "<<1.0/(1.0-e)<<"!";

    double D = Args.evaluate(1).as_double();
    double heat = Args.evaluate(2).as_double();
    constructor in_training_c = Args.evaluate(3).head().as_<constructor>();
    bool in_training = true;
    if (in_training_c.f_name == "Prelude.False")
	in_training = false;

    using namespace A2::states;

    // Here D = rate * t

    // Return a model with all probabilities zero if e==1.
    // Scaling time by 1/(1.0-e) doesn't work if e==1.
    if (e >= 1)
	return indel::PairHMM();

    // (1-e) * delta / (1-delta) = P(indel)
    // But move the (1-e) into the RATE to make things work
    double mu = D/(1.0-e);
    double P_indel = 1.0 - exp(-mu);
    double A = P_indel;

    if (in_training) A = std::min(A,0.005);

    double delta = A/(1+A);

    // Note: If the branch is disconnected, then t < -0.5
    //  if (t < -0.5) delta = 0.5;

    double f = 0.1; //unaligned fraction
    delta = pow(delta, heat) * pow(f/(1+f),1-heat);
    e = 1.0 - pow(1.0 - e, heat);

    if (1 - 2*delta <0)
	throw myexception()<<"indel model: we need (delta <= 0.5), but delta = "<<delta;

    if (e > 1)
	throw myexception()<<"indel model: we need (epsilon <= 1), but epsilon = "<<e;
    
    assert(delta >= 0 and delta <= 1);
    assert(e >= 0 and e < 1);

    // transition probabilities default to *zero*
    indel::PairHMM Q;

    Q(S ,S ) = 0;
    Q(S ,M ) = 1 - 2*delta;
    Q(S ,G1) = delta;
    Q(S ,G2) = delta;
    Q(S ,E ) = 1 - delta;

    Q(M ,S ) = 1;
    Q(G1,S ) = 1;
    Q(G2,S ) = 1;

    // turn the model into a fragment model
    fragmentize(Q,e);

    remove_one_state(Q,S);

    Q.start_pi(S)  = 0;
    Q.start_pi(M)  = 1;
    Q.start_pi(G1) = 0;
    Q.start_pi(G2) = 0;
    Q.start_pi(E)  = 0;

    return Q;
}

extern "C" closure builtin_function_rs05_branch_HMM(OperationArgs& Args)
{
    double e = Args.evaluate(0).as_double();
    double delta = Args.evaluate(1).as_double();
    double t = Args.evaluate(2).as_double();
    double heat = Args.evaluate(3).as_double();
    constructor in_training_c = Args.evaluate(4).head().as_<constructor>();
    bool in_training = true;
    if (in_training_c.f_name == "Prelude.False")
	in_training = false;

    if (in_training) delta = std::min(delta,0.005);

  // Return a model with all probabilities zero if e==1.
    if (e >= 1)
	return indel::PairHMM();

    double f = 0.1; //unaligned fraction
    delta = pow(delta, heat) * pow(f/(1+f),1-heat);
    e = 1.0 - pow(1.0 - e, heat);

    if (delta > 0.5)
	throw myexception()<<"RS05_branch_HMM: we need (delta <= 0.5), but delta = "<<delta;

    if (e > 1.0)
	throw myexception()<<"RS05_branch_HMM: we need (epsilon <= 1), but epsilon = "<<e;

    assert(delta >= 0.0 and delta <= 1.0);
    assert(e >= 0.0 and e < 1.0);

    indel::PairHMM Q;
    using namespace A2::states;

    Q(S ,S ) = 0;
    Q(S ,M ) = 1 - 2*delta;
    Q(S ,G1) = delta;
    Q(S ,G2) = delta;
    Q(S ,E ) = 0;

    Q(M ,S ) = 1;
    Q(G1,S ) = 1;
    Q(G2,S ) = 1;

    // For the states G1, G2 fragment lengths are Geometric(e)
    fragmentize(Q,e,G1);
    fragmentize(Q,e,G2);

    // For the states M, G1, G2 we might exit with probability t
    exitize(Q,t,M ,E);
    exitize(Q,t,G1,E);
    exitize(Q,t,G2,E);

    // When moving from another state, continue until we are not in S
    remove_one_state(Q,S);

    Q.start_pi(S)  = 0;
    Q.start_pi(M)  = 1;
    Q.start_pi(G1) = 0;
    Q.start_pi(G2) = 0;
    Q.start_pi(E)  = 0;

    return Q;
}

// f_M(s) = [ ME  + s(MGxGE - MExGG) ] / [ 1 - s(GG + MM) + s^2(MMxGG - MGxGM) ]

extern "C" closure builtin_function_rs05_lengthp(OperationArgs& Args)
{
    indel::PairHMM QE = Args.evaluate(0).as_<indel::PairHMM>();
    int l = Args.evaluate(1).as_int();

    using namespace A2::states;

    remove_one_state(QE,G2);

    //--------------- Remove the 'G2' State ----------------------//
    double MM = QE(M,M);
    double MG = QE(M,G1);
    double ME = QE(M,E);

    double GM = QE(G1,M);
    double GG = QE(G1,G1);
    double GE = QE(G1,E);

    //----- Calculate roots of q(s); we assume its quadratic -----//
    double C = 1;
    double B = -(GG + MM);
    double A = MM*GG - MG*GM;
    if (A == 0) return {0.0};
    
    double sqr_det = sqrt(B*B-4.0*A*C);
    double r1 = (-B - sqr_det)/(2*A);
    double r2 = (-B + sqr_det)/(2*A);

    //------------ Calculate the coefficients f_M[l] ------------//
    double P;
    if (l==0)
	P = ME;
    else {
	double P1 = pow(r1,-l-1);
	double P2 = pow(r2,-l-1);

	// Calculate q[l] and q[l-i] (I've proved that all q[i]>=0)
	double q_l   = 1.0/ (A*(r2-r1)) * (P1 - P2);
	double q_lm1 = 1.0/ (A*(r2-r1)) * (P1*r1 - P2*r2);

	// Calculate f_M[l] from the q[i] (*IS* this always positive?)
	P = ME*q_l + (MG*GE - ME*GG)*q_lm1;
    }
    return {P};
}

extern "C" closure builtin_function_bitmask_from_alignment(OperationArgs& Args)
{
    using boost::dynamic_bitset;

    auto arg0 = Args.evaluate(0);
    const auto& A = arg0. as_<alignment>();

    int seq = Args.evaluate(1).as_int();

    int L = A.length();
    
    object_ptr<Box<dynamic_bitset<>>> mask_(new Box<dynamic_bitset<>>(L));
    auto& mask = *mask_;

    for(int i=0;i<L;i++)
	if (A.character(i,seq))
	    mask.flip(i);

    return mask_;
}

extern "C" closure builtin_function_load_alignment(OperationArgs& Args)
{
    std::string filename = Args.evaluate(0).as_<String>();

    object_ptr<alignment> A(new alignment(DNA(),filename));

    return A;
}
