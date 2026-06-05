#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "dp/2way.H"
#include "imodel/imodel.H"
#include "computation/expression/bool.H"
#include "computation/expression/constructor.H"
#include "computation/runtime/ast.H"
#include <boost/dynamic_bitset.hpp>
#include "sequence/doublets.H"
#include "alignment/alignment.H"
#include "sequence/sequence-format.H"
#include "dp/2way.H"
#include "util/cmdline.H"
#include "util/range.H"
#include "util/rng.H"
#include <regex>                                  // to use in getTaxonNamesRaw

using std::string;
using std::vector;
using std::pair;

using std::optional;
using std::map;
using std::tuple;

using Alphabet = PtrBox<alphabet>;

// #include "alignment/alignment-util.H"
vector<int> alignment_row_counts(const alignment& A, int i, const vector<int>& counts);

bool is_runtime_bool_true(const R::Exp& value)
{
    if (auto c = value.to<R::Constructor>())
        return is_bool_true(c->value);

    return false;
}

object_ptr<R::RVector> runtime_vector_from_value(const R::Exp& value)
{
    object_ptr<R::RVector> result(new R::RVector);

    if (const auto* runtime_vector = value.to<R::RVector>())
    {
        result->assign(runtime_vector->begin(), runtime_vector->end());
        return result;
    }

    throw myexception()<<"Expected alignment child list to be an RVector or R::RVector, but got "<<value.print();
}


extern "C" closure builtin_function_flip_alignment(OperationArgs& Args)
{
    return Box<pairwise_alignment_t>(Args.evaluate_slot_to_value(0).as_<Box<pairwise_alignment_t>>().flipped());
}

extern "C" closure builtin_function_unaligned_pairwise_alignment(OperationArgs& Args)
{
    int l1 = Args.evaluate_slot_to_value(0).as_int();
    int l2 = Args.evaluate_slot_to_value(1).as_int();

    return Box<pairwise_alignment_t>(make_unaligned_pairwise_alignment(l1,l2));
}

extern "C" closure builtin_function_left_aligned_pairwise_alignment(OperationArgs& Args)
{
    int l1 = Args.evaluate_slot_to_value(0).as_int();
    int l2 = Args.evaluate_slot_to_value(1).as_int();

    return Box<pairwise_alignment_t>(make_left_aligned_pairwise_alignment(l1,l2));
}

extern "C" closure builtin_function_pairwise_alignment_probability_from_counts(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    const matrix<int>& counts = arg0.as_<Box<matrix<int>>>();
    auto arg1 = Args.evaluate_slot_to_value(1);
    const indel::PairHMM& Q = arg1.as_<indel::PairHMM>();

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
    return {Args.evaluate_slot_to_value(0).as_<Box<pairwise_alignment_t>>().count_insert()};
}

extern "C" closure builtin_function_numMatch(OperationArgs& Args)
{
    return {Args.evaluate_slot_to_value(0).as_<Box<pairwise_alignment_t>>().count_match()};
}

extern "C" closure builtin_function_numDelete(OperationArgs& Args)
{
    return {Args.evaluate_slot_to_value(0).as_<Box<pairwise_alignment_t>>().count_delete()};
}

extern "C" closure builtin_function_numIndels(OperationArgs& Args)
{
    return {Args.evaluate_slot_to_value(0).as_<Box<pairwise_alignment_t>>().count_indels()};
}

extern "C" closure builtin_function_lengthIndels(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& a = arg0.as_<Box<pairwise_alignment_t>>();
    return {a.count_delete() + a.count_insert()};
}

extern "C" R::Exp simple_function_pairwise_alignment_length1(vector<R::Exp>& args)
{
    
    return get_arg(args).as_<Box<pairwise_alignment_t>>().length1();
}

extern "C" R::Exp simple_function_pairwise_alignment_length2(vector<R::Exp>& args)
{
    
    return get_arg(args).as_<Box<pairwise_alignment_t>>().length2();
}

extern "C" R::Exp simple_function_alignment_length(vector<R::Exp>& args)
{
    return get_arg(args).as_<Box<alignment>>().length();
}

extern "C" closure builtin_function_transition_counts(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    const auto& A = arg0.as_<Box<pairwise_alignment_t>>().value();

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
    double e = Args.evaluate_slot_to_value(0).as_double();
    if (e < 0.0)
	throw myexception()<<"Error: mean indel length cannot be < 1, but was set to "<<1.0/(1.0-e)<<"!";

    int l = Args.evaluate_slot_to_value(1).as_int();

    if (l < 0)
	return {0.0};
    else if (l==0)
	return {1.0};
    else
	return {1.0-e};
}

extern "C" closure builtin_function_rs07_branch_HMM(OperationArgs& Args)
{
    double e = Args.evaluate_slot_to_value(0).as_double();
    if (e < 0.0)
	throw myexception()<<"Error: mean indel length cannot be < 1, but was set to "<<1.0/(1.0-e)<<"!";

    double D = Args.evaluate_slot_to_value(1).as_double();
    double heat = Args.evaluate_slot_to_value(2).as_double();
    bool in_training = is_runtime_bool_true(Args.evaluate_slot_to_value(3));

    using namespace A2::states;

    // Here D = rate * t

    // Return a model with all probabilities zero if e==1.
    // Scaling time by 1/(1.0-e) doesn't work if e==1.
    if (e >= 1)
	return indel::PairHMM();

    // (1-e) * delta / (1-delta) = P(indel)
    // But move the (1-e) into the RATE to make things work
    double mu = D/(1.0-e);
    double P_indel = -expm1(-mu);
    double P_non_indel = exp(-mu);
    double A = P_indel;

    if (in_training) A = std::min(A,0.005);

    // delta = 1-exp(-mu)/(1 + 1-exp(-mu)) = (1 - exp(-mu))/(2 - exp(-mu))

    // 1-2*delta = (2 - e^-mu)/(2 - e^-mu) - (2 - 2e^-mu)/(2 - e^-mu)
    //           = e^-mu/(2 - e^-mu)
    //           = P_non_indel / (2 - P_non_indel)

    // 1 - delta = (2 - e^-mu)/(2 - e^-mu) - (1 - e^-mu)/(2 - e^-mu)
    //           = 1/(2 - e^-mu)
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
    Q(S ,M ) = (P_indel < 0.5) ? (1 - 2*delta) : (P_non_indel/(2 - P_non_indel));
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

extern "C" closure builtin_function_multi_rs07_branch_HMM(OperationArgs& Args)
{
    double e = Args.evaluate_slot_to_value(0).as_double();
    if (e < 0.0)
	throw myexception()<<"Error: mean indel length cannot be < 1, but was set to "<<1.0/(1.0-e)<<"!";

    double fraction1 = Args.evaluate_slot_to_value(1).as_double();
    double rate1 = Args.evaluate_slot_to_value(2).as_double();
    double rate2 = Args.evaluate_slot_to_value(3).as_double();
    double distance = Args.evaluate_slot_to_value(4).as_double();
    double heat = Args.evaluate_slot_to_value(5).as_double();
    bool in_training = is_runtime_bool_true(Args.evaluate_slot_to_value(6));

    using namespace A2::states;

    // Here D = rate * t

    // Return a model with all probabilities zero if e==1.
    // Scaling time by 1/(1.0-e) doesn't work if e==1.
    if (e >= 1)
	return indel::PairHMM();

    // (1-e) * delta / (1-delta) = P(indel)
    // But move the (1-e) into the RATE to make things work
    rate1 /= (1.0 - e);
    rate2 /= (1.0 - e);

    double fraction2 = 1 - fraction1;
    double P_indel = fraction1 * (-expm1(-rate1 * distance)) + fraction2 * (-expm1(-rate2 * distance));
    double P_non_indel = fraction1 * exp(-rate1 * distance) + fraction2 * exp(-rate2 * distance);
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
    Q(S ,M ) = (P_indel < 0.5) ? (1 - 2*delta) : (P_non_indel/(2 - P_non_indel));
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
    double e = Args.evaluate_slot_to_value(0).as_double();
    double delta = Args.evaluate_slot_to_value(1).as_double();
    double t = Args.evaluate_slot_to_value(2).as_double();
    double heat = Args.evaluate_slot_to_value(3).as_double();
    bool in_training = is_runtime_bool_true(Args.evaluate_slot_to_value(4));

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
    indel::PairHMM QE = Args.evaluate_slot_to_value(0).as_<indel::PairHMM>();
    int l = Args.evaluate_slot_to_value(1).as_int();

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

struct column_map
{
    optional<int> value;
    map<int, column_map> key_first;

    optional<int>& insert(const vector<int>& key, int index=0)
    {
        if (index >= key.size()) return value;
        int x = key[index];
        return key_first[x].insert(key, index+1);
    }
};

vector<int> site_pattern(const R::RVector& A, int c)
{
    int n = A.size();
    vector<int> pattern(n);
    for(int j=0;j<n;j++)
    {
        pattern[j] = A[j].as_<R::RPair>().second.as_<R::RVector>()[c].as_int();
    }
    return pattern;
}

vector<int> site_pattern_var_nonvar(const R::RVector& A, int c)
{
    int n = A.size();
    vector<int> pattern(n);
    for(int j=0;j<n;j++)
    {
        int x = A[j].as_<R::RPair>().second.as_<R::RVector>()[c].as_int();
        if (x < 0)
            x = alphabet::gap;
        else
            x = alphabet::not_gap;
        pattern[j] = x;
    }
    return pattern;
}

int find_add_column(column_map& M, const vector<int>& column, int next)
{
    auto& result = M.insert(column);
    if (not result)
        result = next;
    return *result;
}

int add_column(column_map& M, const vector<int>& column, vector<vector<int>>& cols, vector<int>& counts)
{
    assert(cols.size() == counts.size());
    int c = find_add_column(M, column, cols.size());
    if (c == cols.size())
    {
        cols.push_back(column);
        counts.push_back(1);
    }
    else
        counts[c]++;
    return c;
}

tuple<vector<vector<int>>,vector<int>,vector<int>> compress_site_patterns(const R::RVector& A)
{
    int L = A[0].as_<R::RPair>().second.as_<R::RVector>().size();

    // Check that all the sequences have the same length
    for(int i=1;i<A.size();i++)
	assert(L == A[i].as_<R::RPair>().second.as_<R::RVector>().size());

    column_map M;
    vector<vector<int>> columns;
    vector<int> counts;
    vector<int> mapping(L);
    for(int c=0;c<L;c++)
        mapping[c] = add_column(M, site_pattern(A,c), columns, counts);

    assert(counts.size() == columns.size());
    return {columns, counts, mapping};
}

tuple<vector<vector<int>>,vector<int>,vector<int>> compress_site_patterns_var_nonvar(const R::RVector& A, const alphabet& /*a*/)
{
    int L = A[0].as_<R::RPair>().second.as_<R::RVector>().size();

    // Check that all the sequences have the same length
    for(int i=1;i<A.size();i++)
	assert(L == A[i].as_<R::RPair>().second.as_<R::RVector>().size());

    column_map M;
    vector<vector<int>> columns;
    vector<int> counts;
    vector<int> mapping(L);
    for(int c=0;c<L;c++)
        mapping[c] = add_column(M, site_pattern_var_nonvar(A,c), columns, counts);

    assert(counts.size() == columns.size());
    return {columns, counts, mapping};
}

R::RVector alignment_from_patterns(const R::RVector& old, const vector<vector<int>>& patterns)
{
    int n_leaves = old.size();

    assert(n_leaves == patterns[0].size());

    int L = patterns.size();

    R::RVector A(n_leaves);

    for(int i=0;i<n_leaves;i++)
    {
	R::RVector row(L);
        for(int c=0;c<L;c++)
            row[c] = patterns[c][i];
	const auto& name = old[i].as_<R::RPair>().first.as_string();
	A[i] = R::RPair(name, row);
    }

    return A;
}

// This version only returns an alignment with only n sequences (i.e. n is the number of leaf sequence).
std::tuple<R::RVector, vector<int>, vector<int>> compress_alignment(const R::RVector& A)
{
    if (A.size() == 0)
        return {{},{},{}};

    auto [patterns, counts, mapping] = compress_site_patterns(A);
    return {alignment_from_patterns(A, patterns), counts, mapping};
}


// This version only returns an alignment with only n sequences (i.e. n is the number of leaf sequence).
std::tuple<R::RVector, vector<int>> compress_alignment_var_nonvar(const R::RVector& A, const alphabet& a)
{
    if (A.size() == 0)
        return {{},{}};

    // extract patterns
    auto [patterns, counts, mapping] = compress_site_patterns_var_nonvar(A, a);

    // construct constant patterns
    vector<vector<int>> constant_patterns;
    for(auto& pattern: patterns)
    {
	for(int letter=0;letter<a.size();letter++)
	{
	    auto constant_pattern = pattern;
	    for(auto& entry: constant_pattern)
	    {
		assert(entry == alphabet::not_gap or entry == alphabet::gap);
		if (entry == alphabet::not_gap)
		    entry = letter;
	    }
	    constant_patterns.push_back(constant_pattern);
	}
    }

    assert(constant_patterns.size() == patterns.size() * a.size());

    return {alignment_from_patterns(A, constant_patterns), counts};
}


extern "C" closure builtin_function_compress_alignment(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& A1 = arg0.as_checked<R::RVector>();

    auto [A,counts,mapping] = compress_alignment(A1);

    return R::RPair(A, R::RPair(R::RVector(counts), R::RVector(mapping)));
}

extern "C" closure builtin_function_compress_alignment_var_nonvar(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& A0 = arg0.as_checked<R::RVector>();

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& a = *arg1.as_checked<Alphabet>();

    auto [A,counts] = compress_alignment_var_nonvar(A0, a);

    return R::RPair(A, R::RVector(counts));
}

extern "C" closure builtin_function_leaf_sequence_counts(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& A = arg0.as_checked<Box<alignment>>();

    int n = Args.evaluate_slot_to_value(1).as_int();

    auto arg2 = Args.evaluate_slot_to_value(2);
    auto counts = (vector<int>)arg2.as_checked<R::RVector>();

    R::RVector all_counts;
    for(int i=0; i < n; i++)
        all_counts.push_back(R::RVector(alignment_row_counts(A,i,counts)));

    return all_counts;
}

extern "C" closure builtin_function_load_alignment(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& a = *arg0.as_checked<Alphabet>();

    string filename = Args.evaluate_slot_to_value(1).as_string();

    object_ptr<Box<alignment>> A(new Box<alignment>(a,filename));

    return A;
}

extern "C" closure builtin_function_loadSequences(OperationArgs& Args)
{
    string filename = Args.evaluate_slot_to_value(0).as_string();

    auto sequences_ = sequence_format::load_from_file(filename);

    object_ptr<R::RVector> sequences(new R::RVector(sequences_.size()));
    for(int i=0;i<sequences->size();i++)
        (*sequences)[i] = new Box<sequence>(sequences_[i]);

    return sequences;
}

extern "C" closure builtin_function_getRange(OperationArgs& Args)
{
    string range = Args.evaluate_slot_to_value(0).as_string();
    int L = Args.evaluate_slot_to_value(1).as_int();

    // 2. Find columns
    vector<int> columns = parse_multi_range(range, L);

    return R::RVector(columns);
}

extern "C" closure builtin_function_selectRangeRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& columns = arg0.as_<R::RVector>();

    auto arg1 = Args.evaluate_slot_to_value(1);
    const auto& sequence = arg1.as_string();

    // unshare
    object_ptr<String> sequence2 = new String;
    // make empty
    sequence2->string::operator=("");
    for(auto& c : columns)
    {
        int col = c.as_int();
        if (col < sequence.size())
            (*sequence2) += sequence[col];
    }

    return sequence2;
}

extern "C" closure builtin_function_alignment_from_sequences(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& a = *arg0.as_checked<Alphabet>();

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& sequences_ = arg1.as_<R::RVector>();

    vector<sequence> sequences;
    for(auto& s: sequences_)
    {
        const string& name = s.as_<R::RPair>().first.as_string();
        const string& letters = s.as_<R::RPair>().second.as_string();
        sequence S(name,"");
        S.string::operator=(letters);
        sequences.push_back(S);
    }

    object_ptr<Box<alignment>> A(new Box<alignment>(a));

    A->load(sequences);

    return A;
}

extern "C" closure builtin_function_sequence_name(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& s = arg0.as_checked<Box<sequence>>();

    return new String(s.name);
}

extern "C" closure builtin_function_sequenceDataRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& s = arg0.as_checked<Box<sequence>>();

    return new String(s);
}

// This is the no-gaps version...
extern "C" closure builtin_function_sequence_to_indices(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& a = *arg0.as_checked<Alphabet>();

    auto arg1 = Args.evaluate_slot_to_value(1);
    const auto& s = arg1.as_string();

    auto letters = a(s);
    vector<int> letters2;
    for(auto letter: letters)
        if (a.is_feature(letter))
            letters2.push_back(letter);

    return new R::RVector(letters2);
}

// This is the with-gaps version...
extern "C" closure builtin_function_sequenceToAlignedIndices(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& a = *arg0.as_checked<Alphabet>();

    auto arg1 = Args.evaluate_slot_to_value(1);
    const auto& s = arg1.as_string();

    auto letters = a(s);
    vector<int> letters2;
    for(auto letter: letters)
        letters2.push_back(letter);

    return new R::RVector(letters2);
}

extern "C" closure builtin_function_statesToLetters(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& smap = arg0.as_<R::RVector>();

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& state_sequence = arg1.as_<R::RVector>();

    auto result = object_ptr<R::RVector>(new R::RVector(state_sequence.size()));
    auto& letter_sequence = *result;

    for(int i=0; i < state_sequence.size(); i++)
    {
        int s = state_sequence[i].as_int();
        if (s >= 0)
            letter_sequence[i] = smap[s].as_int();
        else
            letter_sequence[i] = s;
    }

    return result;
}

extern "C" closure builtin_function_sequences_from_alignment(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& A = arg0.as_<Box<alignment>>().value();

    R::RVector sequences;
    for(int i=0;i<A.n_sequences();i++)
    {
	R::RVector seq;
	for(int col=0;col<A.length();col++)
	    seq.push_back( A(col,i) );

	sequences.push_back(std::move(seq));
    }

    return sequences;
}

extern "C" closure builtin_function_sequence_names(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& A = arg0.as_<Box<alignment>>().value();

    R::RVector sequence_names;
    for(int i=0;i<A.n_sequences();i++)
	sequence_names.push_back(String(A.seq(i).name));

    return sequence_names;
}

extern "C" closure builtin_function_reorder_alignment(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& names = arg0.as_<R::RVector>();

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& A1 = arg1.as_<Box<alignment>>().value();

    vector<string> sequence_names;
    for(auto& name: names)
        sequence_names.push_back(name.as_string());

    object_ptr<Box<alignment>> A2(new Box<alignment>(reorder_sequences(A1, sequence_names)));

    return A2;
}

extern "C" closure builtin_function_select_alignment_columns(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& A0 = arg0.as_<Box<alignment>>().value();

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& sites = arg1.as_<R::RVector>();

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
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& A0 = arg0.as_<Box<alignment>>().value();

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& sites = arg1.as_<R::RVector>();

    auto arg2 = Args.evaluate_slot_to_value(2);
    auto d = arg2.poly_cast<alphabet,Doublets>();
    if (not d)
        throw myexception()<<"select_alignment_pairs: not a doublet alphabet!";
    const Doublets& doublets = *d;

    int N = A0.n_sequences();
    int L = sites.size();
    object_ptr<Box<alignment>> A1(new Box<alignment>(A0.get_alphabet(), N, L));

    for(int i=0;i<sites.size();i++)
    {
        int site1 = sites[i].as_<R::RPair>().first.as_int();
        int site2 = sites[i].as_<R::RPair>().second.as_int();
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
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& A0 = arg0.as_<Box<alignment>>().value();

    auto arg1 = Args.evaluate_slot_to_value(1);
    auto& states = arg1.as_<R::RVector>();

    auto arg2 = Args.evaluate_slot_to_value(2);
    auto& smap = arg2.as_<R::RVector>();

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

extern "C" closure builtin_function_extractStates(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& states = arg0.as_<Vector<pair<int,int>>>();

    R::RVector sequence(states.size());
    for(int i=0; i<sequence.size(); i++)
        sequence[i] = states[i].second;

    return sequence;
}

extern "C" closure builtin_function_mkNodeAlignment(OperationArgs& Args)
{
    int source_node = Args.evaluate_slot_to_value(0).as_int();
    int root_length = Args.evaluate_slot_to_value(1).as_int();
    auto branch_alignments = runtime_vector_from_value(Args.evaluate_slot_to_value(2));

    object_ptr<Box<pairwise_alignment_t>> pairwise_alignment(new Box<pairwise_alignment_t>(vector<int>(root_length,A2::states::G1)));

    return closure(R::Exp{R::App(R::ConstructorApp(constructor("NodeAlignment",3)),
                                 {R::Int(source_node), R::ObjectValue(pairwise_alignment),
                                  R::ObjectValue(branch_alignments)})});
}

extern "C" closure builtin_function_mkBranchAlignment(OperationArgs& Args)
{
    int target_node = Args.evaluate_slot_to_value(0).as_int();
    auto pairwise_alignment = Args.evaluate_slot_to_value(1);
    auto branch_alignments = runtime_vector_from_value(Args.evaluate_slot_to_value(2));

    return closure(R::Exp{R::App(R::ConstructorApp(constructor("BranchAlignment",3)),
                                 {R::Int(target_node), std::move(pairwise_alignment),
                                  R::ObjectValue(branch_alignments)})});
}

extern "C" closure builtin_function_constructPositionSequencesRaw(OperationArgs& Args)
{
    auto nodeAlignment = Args.evaluate_slot_to_value(0);

    return construct2(nodeAlignment);
}

extern "C" closure builtin_function_substituteLetters(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& letters = arg0.as_<R::RVector>();

    auto arg1 = Args.evaluate_slot_to_value(1);

    object_ptr<R::RVector> result(new R::RVector(arg1.as_<R::RVector>()));
    auto& row = *result;

    int j=0;
    for(int i=0;i<row.size();i++)
    {
        int pos = row[i].as_int();
        if (pos >=0 or pos == alphabet::not_gap)
        {
            assert(j < letters.size());
            row[i] = letters[j++];
        }
    }

    assert(j == letters.size());

    return result;
}

vector<int> insertion(const vector<int>& sequence, int pos, int length)
{
    int j=0;
    vector<int> sequence2(sequence.size() + length);

    for(int i=0;i<pos;i++)
        sequence2[j++] = sequence[i];

    for(int i=0;i<length;i++)
        sequence2[j++] = -1;

    for(int i=pos;i<sequence.size();i++)
        sequence2[j++] = sequence[i];

    assert(j == sequence2.size());

    return sequence2;
}

vector<int> deletion(const vector<int>& sequence, int pos, int length)
{
    int j=0;
    vector<int> sequence2(pos + std::max(0, int(sequence.size()) - (pos+length)));

    // OK, so we keep positions [0..pos).
    // Then skip positions [pos,pos+length)
    // Then we keep positions [pos+length, sequences.size())
    for(int i=0;i<pos;i++)
        sequence2[j++] = sequence[i];

    for(int i=pos+length;i<sequence.size();i++)
        sequence2[j++] = sequence[i];

    assert(j == sequence2.size());

    return sequence2;
}

// limited to a maximum of N.
int indelLengthGeometric(double mean_length)
{
    // mean = 1 + [(1-p)/p]  -- because the geometric starts at 1.
    // p* mean = p + (1-p)
    // p*mean  = 1
    // p = 1/mean
    //
    // If the length is one, then the success (exit) probability should be 1.
    return 1 + geometric(1/mean_length);
}

pairwise_alignment_t pairwise_alignment_from_characters(const vector<int>& sequence, int L1)
{
    pairwise_alignment_t alignment;

    // The next id to emit from the ancestral sequence.
    int next_id = 0;

    // Loop over ids in the descendent sequence.
    for(int id: sequence)
    {
        // Insertion
        if (id == -1)
            alignment.push_back_insert();
        // Match
        else
        {
            // Characters from the original sequence should be in [0,L-1]
            assert(id >=0 and id < L1);

            // The character ids should be increasing.
            assert(next_id <= id);

            // Original characters before `id` didn't survive
            while(next_id < id)
            {
                alignment.push_back_delete();
                next_id++;
            }
            assert(next_id == id);

            // But original character `id` survived.
            alignment.push_back_match();
            next_id++;

            // We've now emitted original characters up to and including `id`.
            assert(next_id == id+1);
        }
    }

    // End
    while(next_id < L1)
    {
        alignment.push_back_delete();
        next_id++;
    }

    // Check that we've emitted the entire sequence1.
    assert(next_id == L1);

    return alignment;
}

/*
 * Long Indels with Geometric lengths
 *
 * Lengths follow geometric with FAILURE probability r.
 *
 * 1. Probability of a deletion of length k at a particular position is
 *
 *    mu[k] = mu * (1-r)^2 * r^(k-1)
 *
 *    The total rate of deletion events at a particular position is mu * (1-r).
 *    The total rate of deleting a particular residue is mu.
 *
 * 2. If we assume an equilibrium, then
 *
 *    nu(l+k) * Pr(seq|l+k) * mu[k] = nu(l) * Pr(seq|l) * lambda[k] * Pr(insertion|k)
 *
 *    nu(l+k) * mu[k] = nu(l) * lambda[k]
 *
 *    nu(l+k)/nu(l) = mu[k]/lambda[k] = gamma^k
 *
 *    So nu(l) = (1-gamma) * gamma^l
 *
 * 3. Then we have that
 *
 *    lambda[k] = mu[k] * gamma^k
 *
 *              = mu * (1-r)^2 * r^(k-1) * gamma^k
 *
 *              = gamma * mu * (1-r)^2 * (r * gamma) ^ (k-1)
 *
 *
 *    The total insertion rate at a given position is mu * gamma * (1 - r)^2 / (1 - gamma * r).
 *
 */



/*
 * The probability of a deletion event occurring that includes the first letter is
 *
 *    del_rate * (Pr(L>=1) + Pr(L>=2) + Pr(L>=3) + Pr(L>=4) + ...)
 *
 *    = del_rate * (E L)
 *
 * The probability of a deletion event including the first letter but not starting at position 0 is:
 *
 *    del_rate * (Pr(L>=2) + Pr(L>=3) + Pr(L>=4) + ...)
 *
 *    = del_rate * [(E L) - Pr(L>=1)] = del_rate * [(E L) - 1]
 *
 * Therefore the rate of deletions that include the first letter is del_rate * mean_length.
 *
 * Since the distribution is geometric, its memoryless, and we can ignore where such deletions actually
 * start.
 *
 */

extern "C" closure builtin_function_simulateLongIndelsGeometric(OperationArgs& Args)
{
    double del_rate = Args.evaluate_slot_to_value(0).as_double();
    double ins_rate = Args.evaluate_slot_to_value(1).as_double();
    double mean_length = Args.evaluate_slot_to_value(2).as_double();
    double total_time = Args.evaluate_slot_to_value(3).as_double();
    int L0 = Args.evaluate_slot_to_value(4).as_int();

    if (mean_length < 1)
        throw myexception()<<"simulateLongIndelsGeometric: mean_length = "<<mean_length<<", but should be at least 1";

    auto sequence = iota<int>(L0);

    double t = 0;
    while(true)
    {
        int N = sequence.size();

        // Insertions can occur before every position (N) or after the last position (1).
        double total_ins_rate = ins_rate * (N + 1);
        // Deletions can start before every position (N) or to the left of that (mean_length - 1).
        double total_del_rate = del_rate * (N + (mean_length-1));

        double total_rate = total_ins_rate + total_del_rate;

        double waiting_time = exponential(1/total_rate);

        t += waiting_time;

        if (t > total_time) break;

        int length = indelLengthGeometric(mean_length);
        assert(length > 0);

        // An insertion
        if (uniform() < total_ins_rate/total_rate)
        {
            // An insertion BEFORE position pos.
            int pos = uniform_int(0, N);

            sequence = insertion(sequence, pos, length);
        }
        // A deletion
        else
        {
            int pos= -1;
            // Deletion starts in left flanking sequence.
            if (uniform() < (mean_length-1)/(N+(mean_length-1)))
                pos = 0;
            // Deletion starts to the left of an existing character.
            else
                pos = uniform_int(0, N-1);

            sequence = deletion(sequence, pos, length);
        }
    }

    // Construct the pairwise alignment from the surviving characters.
    object_ptr<Box<pairwise_alignment_t>> result;
    result = new Box<pairwise_alignment_t>(pairwise_alignment_from_characters(sequence, L0));

    assert(result->length1() == L0);

    return result;
}

extern "C" closure builtin_function_showPairwiseAlignmentRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    auto& A = arg0.as_<Box<pairwise_alignment_t>>();

    String s;
    for(int i=0;i<A.size();i++)
    {
        char c = ' ';
        if (A.is_match(i))
            c = 'M';
        else if (A.is_delete(i))
            c = 'D';
        else if (A.is_insert(i))
            c = 'I';
        else if (A.get_state(c) == A2::states::E)
            c = 'E';
        else if (A.get_state(c) == A2::states::S)
            c = 'S';
        else
            throw myexception()<<"showPairwiseAlignmentRaw: I don't recognize state "<<int(c)<<" at position "<<i<<"/"<<A.size();
        s += c;
    }

    return s;
}

// taxonAgesRaw :: R::RVector CPPString -> CPPString -> Int (0/1) -> R::RVector Double
extern "C" closure builtin_function_getTaxonAgesRaw(OperationArgs& Args)
{
    auto labels = Args.evaluate_slot_to_value(0).as_<R::RVector>();
    int n = labels.size();

    string pattern = Args.evaluate_slot_to_value(1).as_string();
    std::regex rpattern(pattern);

    // 0 = forward
    // 1 = backward
    int direction = Args.evaluate_slot_to_value(2).as_int();

    std::vector<double> times(n,0);

    for(int i=0;i<n;i++)
    {
	std::smatch m;

	const string& label = labels[i].as_string();
	if (std::regex_search(label, m, rpattern))
	{
	    string mvalue = m[1];
	    auto value = can_be_converted_to<double>(mvalue);
	    if (not value)
		throw myexception()<<"Label '"<<label<<"' yields time '"<<mvalue<<"' which is not a number!";
	    times[i] = *value;
	}
	else
	    throw myexception()<<"Label '"<<label<<"' does not match regex '"<<pattern<<"'";
    }

    if (direction == 0)
    {
	// We need to convert these to ages (backwards in time).
	double oldest_time = max(times);
	for(auto& time: times)
	    time = oldest_time - time;
    }
    else
    {
	// What if we want to normalize things to a non-zero age?
	double youngest_time = min(times);
	for(auto& time: times)
	    time = time - youngest_time;
    }

    R::RVector result(n);
    for(int i=0;i<n;i++)
	result[i] = times[i];

    return result;
}
