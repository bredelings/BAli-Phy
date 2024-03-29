#include "substitution/parsimony.H"

#include <limits>
#include "dp/hmm.H"
#include "dp/2way.H"
#include "util/range.H"

using std::vector;
using boost::dynamic_bitset;

int max_element(const matrix<int>& M)
{
    auto data = M.begin();
    int S = M.size();
    int m = data[0];
    for(int i=1;i<S;i++)
	m = std::max(m, data[i]);
    return m;
}


// cost for l1 -> l2, where l1 is a letter class
int letter_class1_cost(const alphabet& a, int l1, int l2, const matrix<int>& cost, int max_cost)
{
    assert(a.is_letter(l2));
    assert(a.is_letter_class(l1));

    int n_letters = a.size();

    int c = max_cost;

    for(int l=0;l<n_letters;l++)
        if (a.matches(l,l2))
            c = std::min(c, cost(l,l1));

    return c;
}

// cost for l1 -> l2, where l2 is a letter class
int letter_class2_cost(const alphabet& a, int l1, int l2, const matrix<int>& cost, int max_cost)
{
    assert(a.is_letter(l1));
    assert(a.is_letter_class(l2));

    int n_letters = a.size();

    int c = max_cost;

    for(int l=0;l<n_letters;l++)
        if (a.matches(l,l2))
            c = std::min(c, cost(l1,l));

    return c;
}

void peel_muts(const int* n_muts1, int* n_muts2, int n_letters, const matrix<int>& cost)
{
    for(int l2=0;l2<n_letters;l2++)
    {
	int c = cost(l2,0) + n_muts1[0];
	for(int l1=1;l1<n_letters;l1++)
	    c = std::min(c, cost(l2,l1) + n_muts1[l1]);
	n_muts2[l2] += c;
    }
}

object_ptr<ParsimonyCacheBranch>
peel_muts(const EVector& sequences,
	  const alphabet& a,
	  const EVector& A_,
	  const EVector& n_muts_,
	  const matrix<int>& cost)
{
    int n_branches_in = n_muts_.size();
    assert(n_muts_.size() == n_branches_in);
    assert(A_.size() == n_branches_in);

    auto A = [&](int i) -> auto& {return A_[i].as_<Box<pairwise_alignment_t>>();};
    auto sequence = [&](int i) -> auto& {return sequences[i].as_<EVector>();};
    auto n_muts = [&](int i) -> auto& {return n_muts_[i].as_<ParsimonyCacheBranch>();};

    assert(not sequences.empty() or not A_.empty());
    int L = (sequences.empty())?A(0).length2() : sequence(0).size();

    int n_letters = cost.size1();

#ifndef NDEBUG
    for(int i=0; i<n_branches_in; i++)
    {
	assert(n_muts(i).n_letters == n_letters);
	assert(A(i).length2() == L);
//	assert(A(i).length1() == n_muts(i).n_columns());
    }
#endif

    auto result = object_ptr<ParsimonyCacheBranch>(new ParsimonyCacheBranch(n_letters, L));
    auto& n_muts_out = *result;

    int other_subst = 0;
    for(int j=0; j < n_branches_in;j++)
	other_subst += n_muts(j).other_subst;

    // index into source sequences and n_muts
    vector<int> s(n_branches_in, 0);
    // index into pairwise alignments
    vector<int> i(n_branches_in, 0);

    for(int s_node=0;;s_node++)
    {
	for(int j=0; j < n_branches_in; j++)
	{
	    auto& a = A(j);
	    auto& ij = i[j];
	    while(ij < a.size() and not a.has_character2(ij))
	    {
		assert(a.has_character1(ij));
		other_subst += n_muts(j).min(s[j]);
		ij++;
		s[j]++;
	    }
	}

        if (s_node == L)
        {
	    for(int j=0; j<n_branches_in; j++)
		assert(i[j] == A(j).size());

            break;
        }
        else
        {
	    for(int j=0; j<n_branches_in; j++)
	    {
		assert(i[j] < A(j).size());
		assert(A(j).has_character2(i[j]));
	    }
        }

	int* S = &n_muts_out(s_node,0);

	for(int l=0;l<n_letters;l++)
	    S[l] = 0;

	for(int j=0;j<n_branches_in;j++)
	{
	    if (A(j).has_character1(i[j]))
	    {
		peel_muts(&n_muts(j)(s[j],0), S, n_letters, cost);
		s[j]++;
	    }
	    i[j]++;
	}

	for(auto& esequence: sequences)
	{
	    auto& sequence = esequence.as_<EVector>();
	    int letter = sequence[s_node].as_int();
	    if (letter >= 0)
	    {
		auto& ok = a.letter_mask(letter);
		for(int l=0;l<n_letters;l++)
		    if (not ok[l])
			S[l] = std::numeric_limits<int>::max()/2;
	    }
	}
    }

    result->other_subst = other_subst;
    return result;
}

int muts_root(const EVector& sequences,
	      const alphabet& a,
	      const EVector& A_,
	      const EVector& n_muts_,
              const matrix<int>& cost)
{
    int n_branches_in = n_muts_.size();
    assert(n_muts_.size() == n_branches_in);
    assert(A_.size() == n_branches_in);

    auto A = [&](int i) -> auto& {return A_[i].as_<Box<pairwise_alignment_t>>();};
    auto sequence = [&](int i) -> auto& {return sequences[i].as_<EVector>();};
    auto n_muts = [&](int i) -> auto& {return n_muts_[i].as_<ParsimonyCacheBranch>();};

    assert(not sequences.empty() or not A_.empty());
    int L = (sequences.empty()) ? A(0).length2() : sequence(0).size();

    int n_letters = n_muts(0).n_letters;

#ifndef NDEBUG
    for(int i=0;i<n_branches_in;i++)
    {
	assert(n_muts(i).n_letters == n_letters);
	assert(A(i).length2() == L);
    }
#endif

    int total = 0;
    vector<int> AL(n_branches_in);
    for(int j=0; j < n_branches_in;j++)
    {
	total += n_muts(j).other_subst;
	AL[j] = A(j).size();
    }

    vector<int> S(n_letters);

    // index into source sequences and n_muts
    vector<int> s(n_branches_in, 0);
    // index into pairwise alignments
    vector<int> i(n_branches_in, 0);

    for(int s_node=0;;s_node++)
    {
	for(int j=0; j < n_branches_in; j++)
	{
	    auto& a = A(j);
	    auto& ij = i[j];
	    while(ij < AL[j] and not a.has_character2(ij))
	    {
		assert(a.has_character1(ij));
		total += n_muts(j).min(s[j]);
		ij++;
		s[j]++;
	    }
	}

        if (s_node == L)
        {
	    for(int j=0; j<n_branches_in; j++)
		assert(i[j] == AL[j]);

            break;
        }
        else
        {
	    for(int j=0; j<n_branches_in; j++)
	    {
		assert(i[j] < AL[j]);
		assert(A(j).has_character2(i[j]));
	    }
        }

        for(auto& s: S)
            s = 0;

	for(int j=0;j<n_branches_in;j++)
	{
	    if (A(j).has_character1(i[j]))
	    {
		peel_muts(&n_muts(j)(s[j],0), &S[0], n_letters, cost);
		s[j]++;
	    }
	    i[j]++;
	}

	for(auto& esequence: sequences)
	{
	    auto& sequence = esequence.as_<EVector>();
	    int letter = sequence[s_node].as_int();
	    if (letter >= 0)
	    {
		auto& ok = a.letter_mask(letter);
		for(int l=0;l<n_letters;l++)
		    if (not ok[l])
			S[l] = std::numeric_limits<int>::max()/2;
	    }
	}

        total += min(S);
    }

    return total;
}

object_ptr<const ParsimonyCacheBranch>
peel_muts_fixed_A(const EVector& sequences,
		  const alphabet& a,
		  const EVector& n_muts_,
		  const matrix<int>& cost)
{
    int n_letters = cost.size1();

    auto sequence = [&](int i) -> auto& { return sequences[i].as_<EPair>().first.as_<EVector>(); };
    auto mask = [&](int i) -> auto& { return sequences[i].as_<EPair>().second.as_<Box<boost::dynamic_bitset<>>>(); };
    auto n_muts = [&](int i) -> auto& {return n_muts_[i].as_<ParsimonyCacheBranch>();};

    int n_branches_in = n_muts_.size();
    assert(not sequences.empty() or not n_muts_.empty());
    int L = (sequences.empty()) ? n_muts(0).bits.size() : mask(0).size();

    int n_sequences = sequences.size();

#ifndef NDEBUG
    for(int i=0; i<n_branches_in; i++)
    {
	assert(n_muts(i).n_letters == n_letters);
	assert(n_muts(i).bits.size() == L);
    }
    for(int i=0; i<n_sequences; i++)
    {
	assert(mask(i).size() == L);
    }
#endif

    dynamic_bitset<> bits;
    bits.resize(L);
    for(int i=0;i<n_sequences;i++)
	bits |= mask(i);
    for(int i=0;i<n_branches_in;i++)
	bits |= n_muts(i).bits;

    auto result = object_ptr<ParsimonyCacheBranch>(new ParsimonyCacheBranch(n_letters, bits.count(), L));
    result->bits = bits;
    auto& n_muts_out = *result;

    // index into sequences
    vector<int> i(n_sequences, 0);
    // index into LCBs
    vector<int> s(n_branches_in, 0);
    // index into LCB_OUT
    int s_node = 0;

    for(int c=0;c<L;c++)
    {
	if (not n_muts_out.bits.test(c)) continue;

	int *S = &n_muts_out(s_node,0);

	for(int j=0;j<n_branches_in;j++)
	{
	    if (n_muts(j).bits.test(c))
	    {
		peel_muts(&n_muts(j)(s[j],0), S, n_letters, cost);
		s[j]++;
	    }
	}

	for(int j=0;j<n_sequences;j++)
	{
	    if (not mask(j).test(c)) continue;

	    int letter = sequence(j)[i[j]].as_int();
	    i[j]++;

	    if (letter >= 0)
	    {
		auto& ok = a.letter_mask(letter);
		for(int l=0;l<n_letters;l++)
		    if (not ok[l])
			S[l] = std::numeric_limits<int>::max()/2;
	    }
	}

	s_node++;
    }

    return result;
}

int muts_root_fixed_A(const EVector& sequences,
		      const alphabet& a,
		      const EVector& n_muts_,
		      const matrix<int>& costs,
		      const EVector& counts)
{
    int n_letters = costs.size1();

    auto sequence = [&](int i) -> auto& { return sequences[i].as_<EPair>().first.as_<EVector>(); };
    auto mask = [&](int i) -> auto& { return sequences[i].as_<EPair>().second.as_<Box<boost::dynamic_bitset<>>>(); };
    auto n_muts = [&](int i) -> auto& {return n_muts_[i].as_<ParsimonyCacheBranch>();};

    int n_branches_in = n_muts_.size();
    assert(not sequences.empty() or not n_muts_.empty());
    int L = (sequences.empty()) ? n_muts(0).bits.size() : mask(0).size();

    int n_sequences = sequences.size();

#ifndef NDEBUG
    for(int i=0; i<n_branches_in; i++)
    {
	assert(n_muts(i).n_letters == n_letters);
	assert(n_muts(i).bits.size() == L);
    }
    for(int i=0; i<n_sequences; i++)
    {
	assert(mask(i).size() == L);
    }
#endif

    dynamic_bitset<> bits;
    bits.resize(L);
    for(int i=0;i<n_sequences;i++)
	bits |= mask(i);
    for(int i=0;i<n_branches_in;i++)
	bits |= n_muts(i).bits;

    int total = 0;

    vector<int> S(n_letters);

    // index into sequences
    vector<int> i(n_sequences, 0);
    // index into LCBs
    vector<int> s(n_branches_in, 0);
    // index into LCB_OUT
    int s_node = 0;

    for(int c=0;c<L;c++)
    {
	if (not bits.test(c)) continue;

	for(auto& s: S)
	    s = 0;

	for(int j=0;j<n_branches_in;j++)
	{
	    if (n_muts(j).bits.test(c))
	    {
		peel_muts(&n_muts(j)(s[j],0), &S[0], n_letters, costs);
		s[j]++;
	    }
	}

	for(int j=0;j<n_sequences;j++)
	{
	    if (not mask(j).test(c)) continue;

	    int letter = sequence(j)[i[j]].as_int();
	    i[j]++;

	    if (letter >= 0)
	    {
		auto& ok = a.letter_mask(letter);
		for(int l=0;l<n_letters;l++)
		    if (not ok[l])
			S[l] = std::numeric_limits<int>::max()/2;
	    }
	}

        total += min(S) * counts[s_node].as_int();

	s_node++;
    }

    return total;
}


int muts_root_fixed_A(const ParsimonyCacheBranch& n_muts0, const ParsimonyCacheBranch& n_muts1, const ParsimonyCacheBranch& n_muts2,
                      const matrix<int>& costs, const EVector& counts)
{

    int n_letters = n_muts0.n_letters;
    assert(n_muts1.n_letters == n_letters);
    assert(n_muts2.n_letters == n_letters);

    int L = n_muts0.alignment_length;
    assert(n_muts1.alignment_length == L);
    assert(n_muts2.alignment_length == L);

    assert(counts.size() == L);

    /*-------------------- Do the peeling part------------- --------------------*/
    int total = 0;

    int i0=0;
    int i1=0;
    int i2=0;

    vector<int> S(n_letters);

    for(int c=0;c<L;c++)
    {
        if (not n_muts0.bits.test(c) and not n_muts1.bits.test(c) and not n_muts2.bits.test(c))
            continue;

        for(auto& s: S)
            s = 0;

	if (n_muts0.bits.test(c))
	    peel_muts(&n_muts0[i0*n_letters], &S[0], n_letters, costs);

	if (n_muts1.bits.test(c))
	    peel_muts(&n_muts1[i1*n_letters], &S[0], n_letters, costs);

	if (n_muts2.bits.test(c))
	    peel_muts(&n_muts2[i2*n_letters], &S[0], n_letters, costs);

        if (n_muts0.bits.test(c)) i0++;
        if (n_muts1.bits.test(c)) i1++;
        if (n_muts2.bits.test(c)) i2++;

        int count = counts[c].as_int();
        assert(count > 0);

        total += count * min(S);
    }

    return total;
}

