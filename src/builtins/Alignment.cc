#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "dp/2way.H"
#include "imodel/imodel.H"
#include "computation/expression/bool.H"
#include "computation/expression/constructor.H"
#include <boost/dynamic_bitset.hpp>
#include "sequence/doublets.H"
#include "alignment/alignment.H"
#include "sequence/sequence-format.H"
#include "dp/2way.H"
#include "models/site-compression.H"
#include "util/cmdline.H"

using std::string;
using std::vector;
using std::pair;

using Alphabet = PtrBox<alphabet>;

// #include "alignment/alignment-util.H"
vector<int> alignment_row_counts(const alignment& A, int i, const vector<int>& counts);


extern "C" closure builtin_function_flip_alignment(OperationArgs& Args)
{
    return Box<pairwise_alignment_t>(Args.evaluate(0).as_<Box<pairwise_alignment_t>>().flipped());
}

extern "C" closure builtin_function_unaligned_pairwise_alignment(OperationArgs& Args)
{
    int l1 = Args.evaluate(0).as_int();
    int l2 = Args.evaluate(1).as_int();

    return Box<pairwise_alignment_t>(make_unaligned_pairwise_alignment(l1,l2));
}

extern "C" closure builtin_function_left_aligned_pairwise_alignment(OperationArgs& Args)
{
    int l1 = Args.evaluate(0).as_int();
    int l2 = Args.evaluate(1).as_int();

    return Box<pairwise_alignment_t>(make_left_aligned_pairwise_alignment(l1,l2));
}

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

extern "C" closure builtin_function_numInsert(OperationArgs& Args)
{
    return {Args.evaluate(0).as_<Box<pairwise_alignment_t>>().count_insert()};
}

extern "C" closure builtin_function_numMatch(OperationArgs& Args)
{
    return {Args.evaluate(0).as_<Box<pairwise_alignment_t>>().count_match()};
}

extern "C" closure builtin_function_numDelete(OperationArgs& Args)
{
    return {Args.evaluate(0).as_<Box<pairwise_alignment_t>>().count_delete()};
}

extern "C" closure builtin_function_numIndels(OperationArgs& Args)
{
    return {Args.evaluate(0).as_<Box<pairwise_alignment_t>>().count_indels()};
}

extern "C" closure builtin_function_lengthIndels(OperationArgs& Args)
{
    auto& a = Args.evaluate(0).as_<Box<pairwise_alignment_t>>();
    return {a.count_delete() + a.count_insert()};
}

extern "C" closure builtin_function_pairwise_alignment_length1(OperationArgs& Args)
{
    return {Args.evaluate(0).as_<Box<pairwise_alignment_t>>().length1()};
}

extern "C" closure builtin_function_pairwise_alignment_length2(OperationArgs& Args)
{
    return {Args.evaluate(0).as_<Box<pairwise_alignment_t>>().length2()};
}

extern "C" closure builtin_function_alignment_length(OperationArgs& Args)
{
    return {Args.evaluate(0).as_<Box<alignment>>().length()};
}

extern "C" closure builtin_function_transition_counts(OperationArgs& Args)
{
    const auto& A = Args.evaluate(0).as_<Box<pairwise_alignment_t>>().value();

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
    bool in_training = is_bool_true(Args.evaluate(3));

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

extern "C" closure builtin_function_compress_alignment(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& A1 = arg0.as_checked<alignment>();

    int n = A1.n_sequences();

    auto [A,counts,mapping] = compress_alignment(A1,n);

    object_ptr<EPair> tmp23(new EPair);
    tmp23->first = EVector(counts);
    tmp23->second = EVector(mapping); 

    object_ptr<EPair> tmp123(new EPair);
    tmp123->first = Box<alignment>(A);
    tmp123->second = tmp23;

    return tmp123;
}

extern "C" closure builtin_function_uncompress_alignment(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& A1 = arg0.as_checked<alignment>();

    auto arg1 = Args.evaluate(1);
    auto& mapping = arg1.as_<EVector>();

    return object_ptr<Box<alignment>>(new Box<alignment>(uncompress_alignment(A1, (vector<int>)mapping)));
}

extern "C" closure builtin_function_leaf_sequence_counts(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& A = arg0.as_checked<alignment>();

    int n = Args.evaluate(1).as_int();

    auto arg2 = Args.evaluate(2);
    auto counts = (vector<int>)arg2.as_checked<EVector>();

    EVector all_counts;
    for(int i=0; i < n; i++)
        all_counts.push_back(EVector(alignment_row_counts(A,i,counts)));

    return all_counts;
}

extern "C" closure builtin_function_load_alignment(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& a = *arg0.as_checked<Alphabet>();

    string filename = Args.evaluate(1).as_checked<String>();

    object_ptr<Box<alignment>> A(new Box<alignment>(a,filename));

    return A;
}

extern "C" closure builtin_function_load_sequences(OperationArgs& Args)
{
    string filename = Args.evaluate(0).as_checked<String>();

    auto sequences_ = sequence_format::load_from_file(filename);

    EVector sequences(sequences_.size());
    for(int i=0;i<sequences.size();i++)
        sequences[i] = new Box<sequence>(sequences_[i]);

    return sequences;
}

extern "C" closure builtin_function_select_range(OperationArgs& Args)
{
    string range = Args.evaluate(0).as_checked<String>();

    auto arg1 = Args.evaluate(1);
    auto& sequences = arg1.as_<EVector>();

    // 1. Get maximum length
    int L = 0;
    for(auto& s: sequences)
	L = std::max<int>(L, s.as_<Box<sequence>>().size());

    // 2. Find columns
    vector<int> columns = parse_multi_range(range, L);

    EVector sequences2;
    for(auto& e: sequences)
    {
        auto& s = e.as_<Box<sequence>>();
        // unshare
        auto s2 = new Box<sequence>(s);
        // make empty
        s2->string::operator=("");
	for(int col : columns)
            if (col < s.size())
                (*s2) += s[col];
        sequences2.push_back(s2);
    }

    return sequences2;
}

extern "C" closure builtin_function_alignment_from_sequences(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& a = *arg0.as_checked<Alphabet>();

    auto arg1 = Args.evaluate(1);
    auto& sequences_ = arg1.as_<EVector>();

    vector<sequence> sequences;
    for(auto& s: sequences_)
        sequences.push_back(s.as_<Box<sequence>>());

    object_ptr<Box<alignment>> A(new Box<alignment>(a));

    A->load(sequences);

    return A;
}

extern "C" closure builtin_function_sequence_name(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& s = arg0.as_checked<Box<sequence>>();

    return new String(s.name);
}

extern "C" closure builtin_function_sequenceDataRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& s = arg0.as_checked<Box<sequence>>();

    return new String(s);
}

// This is the no-gaps version...
extern "C" closure builtin_function_sequence_to_indices(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& a = *arg0.as_checked<Alphabet>();

    auto arg1 = Args.evaluate(1);
    auto& s = arg1.as_checked<Box<sequence>>();

    auto letters = a(s);
    vector<int> letters2;
    for(auto letter: letters)
        if (a.is_feature(letter))
            letters2.push_back(letter);

    return new EVector(letters2);
}

// This is the with-gaps version...
extern "C" closure builtin_function_sequenceToAlignedIndices(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& a = *arg0.as_checked<Alphabet>();

    auto arg1 = Args.evaluate(1);
    auto& s = arg1.as_checked<Box<sequence>>();

    auto letters = a(s);
    vector<int> letters2;
    for(auto letter: letters)
        letters2.push_back(letter);

    return new EVector(letters2);
}

extern "C" closure builtin_function_sequenceToTextRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& a = *arg0.as_checked<Alphabet>();

    auto arg1 = Args.evaluate(1);
    auto& sequence = arg1.as_<EVector>();

    auto result = object_ptr<String>(new String);
    auto& text = *result;

    for(auto& letter: sequence)
        text += a.lookup(letter.as_int());

    return result;
}

extern "C" closure builtin_function_sequences_from_alignment(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& A = arg0.as_<Box<alignment>>().value();

    EVector sequences;
    for(int i=0;i<A.n_sequences();i++)
    {
	EVector seq;
	for(int col=0;col<A.length();col++)
	    seq.push_back( A(col,i) );

	sequences.push_back(std::move(seq));
    }

    return sequences;
}

extern "C" closure builtin_function_sequence_names(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& A = arg0.as_<Box<alignment>>().value();

    EVector sequence_names;
    for(int i=0;i<A.n_sequences();i++)
	sequence_names.push_back(String(A.seq(i).name));

    return sequence_names;
}

extern "C" closure builtin_function_reorder_alignment(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& names = arg0.as_<EVector>();

    auto arg1 = Args.evaluate(1);
    auto& A1 = arg1.as_<Box<alignment>>().value();

    vector<string> sequence_names;
    for(auto& name: names)
        sequence_names.push_back(name.as_<String>());

    object_ptr<Box<alignment>> A2(new Box<alignment>(reorder_sequences(A1, sequence_names)));

    return A2;
}

extern "C" closure builtin_function_select_alignment_columns(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& A0 = arg0.as_<Box<alignment>>().value();

    auto arg1 = Args.evaluate(1);
    auto& sites = arg1.as_<EVector>();

    int N = A0.n_sequences();
    int L = sites.size();
    object_ptr<Box<alignment>> A1(new Box<alignment>(A0.get_alphabet(), N, L));

    for(int i=0;i<sites.size();i++)
    {
        int site = sites[i].as_int();
        for(int j=0;j<N;j++)
        {
            int letter = A0(site,j);
            A1->set_value(i, j, letter);
        }
    }

    return A1;
}

extern "C" closure builtin_function_select_alignment_pairs(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& A0 = arg0.as_<Box<alignment>>().value();

    auto arg1 = Args.evaluate(1);
    auto& sites = arg1.as_<EVector>();

    auto arg2 = Args.evaluate(2);
    auto d = arg2.poly_cast<alphabet,Doublets>();
    if (not d)
        throw myexception()<<"select_alignment_pairs: not a doublet alphabet!";
    const Doublets& doublets = *d;

    int N = A0.n_sequences();
    int L = sites.size();
    object_ptr<Box<alignment>> A1(new Box<alignment>(A0.get_alphabet(), N, L));

    for(int i=0;i<sites.size();i++)
    {
        auto [site1,site2] = (pair<int,int>)sites[i].as_<EPair>();
        for(int j=0;j<N;j++)
        {
            int nuc1 = A0(site1,j);
            int nuc2 = A0(site2,j);
            int doublet = doublets.get_doublet(nuc1,nuc2);
            A1->set_value(i, j, doublet);
        }
    }

    return A1;
}

extern "C" closure builtin_function_ancestral_sequence_alignment(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& A0 = arg0.as_<Box<alignment>>().value();

    auto& arg1 = Args.evaluate(1);
    auto& states = arg1.as_<EVector>();

    auto& arg2 = Args.evaluate(2);
    auto& smap = arg2.as_<EVector>();

    int n = states.size();
    int L = states[0].as_<Vector<pair<int,int>>>().size();

    object_ptr<Box<alignment>> A_(new Box<alignment>(A0.get_alphabet(), n, L));
    auto& A = *A_;

    assert(A0.length() == A.length());

    for(int i=0;i<A.n_sequences();i++)
    {
        auto& node_states = states[i].as_<Vector<pair<int,int>>>();

        if (i < A0.n_sequences())
        {
            A.seq(i) = A0.seq(i);
            for(int c=0;c<A.length();c++)
                A.set_value(c, i, A0(c,i));
        }
        else
        {
            A.seq(i).name = "A"+std::to_string(i);
            for(int c=0;c<A.length();c++)
            {
                int state = node_states[c].second;
                int letter = (state == -1)?-1:smap[state].as_int();
                A.set_value(c, i, letter);
            }
        }
    }

    // FIXME - minimally-connect leaf characters in the machine!
    // It might be a bit slower if we did that, though.

    return A_;
}

extern "C" closure builtin_function_get_sequence_from_states(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& states = arg0.as_<Vector<pair<int,int>>>();

    EVector sequence(states.size());
    for(int i=0; i<sequence.size(); i++)
        sequence[i] = states[i].second;

    return sequence;
}

