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

object_ptr<const ParsimonyCacheBranch> peel_muts_leaf_branch(const alphabet& a, const EVector& letters, const matrix<int>& cost)
{
    int max_cost = max_element(cost)+1;

    int n_letters = a.size();

    int L = letters.size();

    auto result = object_ptr<ParsimonyCacheBranch>(new ParsimonyCacheBranch(n_letters, L));
    auto& n_muts = *result;
    assert(letters.size() == L);

    for(int i=0;i<L;i++)
    {
	int l1 = letters[i].as_int();
	if (a.is_letter(l1))
	    for(int l2=0;l2<n_letters;l2++)
		n_muts(i, l2) = cost(l1,l2);
	else if (a.is_letter_class(l1))
	    for(int l2=0;l2<n_letters;l2++)
	    {
		int c = max_cost;
		for(int l=0;l<n_letters;l++)
		    if (a.matches(l,l1))
			c = std::min(c, cost(l,l2));
		n_muts(i, l2) = c;
	    }
	else  // wildcard
	{
	    assert(l1 == alphabet::not_gap);
	    for(int l2=0;l2<n_letters;l2++)
		n_muts(i, l2) = 0;
	}
    }
    return result;
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
peel_muts_internal_branch(const pairwise_alignment_t& A0,
                          const pairwise_alignment_t& A1,
                          const ParsimonyCacheBranch n_muts0,
                          const ParsimonyCacheBranch n_muts1,
                          const matrix<int>& cost)
{
    assert(n_muts0.n_letters == n_muts1.n_letters);
    int n_letters = n_muts0.n_letters;

    auto a0 = convert_to_bits(A0, 0, 2);
    auto a1 = convert_to_bits(A1, 1, 2);
    auto a012 = Glue_A(a0, a1);

    // get the relationships with the sub-alignments for the (two) branches behind b0
    matrix<int> index = get_indices_from_bitpath_w(a012, {0,1}, 1<<2);
    int L = index.size1();
      
    auto result = object_ptr<ParsimonyCacheBranch>(new ParsimonyCacheBranch(n_letters, L));
    auto& n_muts = *result;

    /*-------------------- Do the peeling part------------- --------------------*/
    for(int i=0;i<L;i++)
    {
	for(int l2=0;l2<n_letters;l2++)
	    n_muts[i*n_letters + l2] = 0;

	int i0 = index(i,0);
	if (i0 != alphabet::gap)
	    peel_muts(&n_muts0[i0*n_letters], &n_muts[i*n_letters], n_letters, cost);

	int i1 = index(i,1);
	if (i1 != alphabet::gap)
	    peel_muts(&n_muts1[i1*n_letters], &n_muts[i*n_letters], n_letters, cost);
    }

    /*-------------------- Do the other_subst collection part -------------------*/
    // min_i (min_j (cost(i,j) + n_muts[j])) <= min_i ( 0 + n_muts[i] )
    // min_i (min_j (cost(i,j) + n_muts[j])) >= min_i ( min_j ( n_muts[j] ) )
    // min_i (min_j (cost(i,j) + n_muts[j])) == min_i ( n_muts[i] )
    n_muts.other_subst = n_muts0.other_subst + n_muts1.other_subst;
    matrix<int> index_collect = get_indices_from_bitpath_wo(a012, {0,1}, 1<<2);
    for(int i=0;i<index_collect.size1();i++)
    {
	int i0 = index_collect(i,0);
	int i1 = index_collect(i,1);

	if (i0 != alphabet::gap)
	{
	    assert(i1 == alphabet::gap);
            n_muts.other_subst += n_muts0.min(i0);
	}
	else if (i1 != alphabet::gap)
	{
	    assert(i0 == alphabet::gap);
            n_muts.other_subst += n_muts1.min(i1);
	}
    }

    return result;
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

int muts_root(const pairwise_alignment_t& A0,
              const pairwise_alignment_t& A1,
              const pairwise_alignment_t& A2,
              const ParsimonyCacheBranch& n_muts1,
              const ParsimonyCacheBranch& n_muts2,
              const ParsimonyCacheBranch& n_muts3,
              const matrix<int>& cost)
{
    assert(n_muts1.n_letters == n_muts2.n_letters);
    assert(n_muts1.n_letters == n_muts3.n_letters);
    int n_letters = n_muts1.n_letters;

    int total = n_muts1.other_subst + n_muts2.other_subst + n_muts3.other_subst;
    int s0=0, s1=0, s2=0, s3=0;
    const int AL0 = A0.size();
    const int AL1 = A1.size();
    const int AL2 = A2.size();
    assert(A0.length2() == A1.length2());
    assert(A0.length2() == A2.length2());

    vector<int> S(n_letters);
    
    // i<j> are indices into the pairwise alignments, while s<j> are indices into the A<i>.length1().
    for(int i0=0,i1=0,i2=0;;)
    {
        while(i0 < AL0 and not A0.has_character2(i0))
        {
            assert(A0.has_character1(i0));
            total += n_muts1.min(s0);
            i0++;
            s0++;
        }
        while (i1 < AL1 and not A1.has_character2(i1))
        {
            assert(A1.has_character1(i1));
            total += n_muts2.min(s1);
            i1++;
            s1++;
        }
        while (i2 < AL2 and not A2.has_character2(i2))
        {
            assert(A2.has_character1(i2));
            total += n_muts3.min(s2);
            i2++;
            s2++;
        }

        if (i2 >= AL2)
        {
            assert(i0 == AL0);
            assert(i1 == AL1);
            break;
        }
        else
        {
            assert(i0 < AL0 and i1 < AL1 and i2 < AL2);
            assert(A0.has_character2(i0) and A1.has_character2(i1) and A2.has_character2(i2));
        }

        bool not_gap0 = A0.has_character1(i0);
        bool not_gap1 = A1.has_character1(i1);
        bool not_gap2 = A2.has_character1(i2);
        i0++;
        i1++;
        i2++;

        for(auto& s: S)
            s = 0;

        if (not_gap0)
        {
            peel_muts(&n_muts1(s0,0), &S[0], n_letters, cost);
            s0++;
        }
        if (not_gap1)
        {
            peel_muts(&n_muts2(s1,0), &S[0], n_letters, cost);
            s1++;
        }
        if (not_gap2)
        {
            peel_muts(&n_muts3(s2,0), &S[0], n_letters, cost);
            s2++;
        }

        total += min(S);
        
        s3++;
    }

    return total;
}

int accumulate_root_leaf(const alphabet& a, const EVector& letters, const pairwise_alignment_t& A, const matrix<int>& cost, const ParsimonyCacheBranch& n_muts)
{
    int n_letters = a.size();

    int max_cost = max_element(cost)+1;

    auto a01 = convert_to_bits(A, 0, 1);

    matrix<int> index = get_indices_from_bitpath_w(a01, {0,1}, 1<<0);

    int total = n_muts.other_subst;
    for(int i=0;i<index.size1();i++)
    {
	int i0 = index(i,0);
	int i1 = index(i,1);
	if (i1 == alphabet::gap)
	{
	    total += n_muts.min(i0);
	    continue;
	}
        else if (i0 == alphabet::gap)
        {
            continue;
        }

	int l1 = letters[i1].as_int();
	if (a.is_letter(l1))
	{
	    int c = cost(l1,0) + n_muts(i0, 0);
	    for(int l2=1; l2<n_letters; l2++)
		c = std::min(c, cost(l1,l2) + n_muts(i0, l2));
	    total += c;
	}
	else if (a.is_letter_class(l1))
	{
	    int c = max_cost + n_muts.max(i0);
	    for(int l2=0; l2<n_letters; l2++)
		for(int l=0; l<n_letters; l++)
		    if (a.matches(l,l1))
			c = std::min(c, cost(l,l2) + n_muts(i0, l2));
	    total += c;
	}
    }
    return total;
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
			S[l] = std::numeric_limits<int>::max();
	    }
	}

        total += min(S);
    }

    return total;
}

int n_mutations_variable_A(const data_partition& P, const matrix<int>& cost)
{
    auto t = P.t();

    if (t.n_nodes() < 2) return 0;

    int root = 0;
    for(int n=0;n < t.n_nodes(); n++)
        if (t.degree(root) != 3 and t.degree(n) == 3)
        {
            root = n;
            break;
        }

    vector<object_ptr<const ParsimonyCacheBranch>> cache(t.n_branches() * 2);

    const auto branches = t.all_branches_toward_node(root);
    auto a = P.get_alphabet();
    for(int b: branches)
    {
        int source = t.source(b);

	if (t.is_leaf_node(source))
        {
            auto letters_ptr = P.get_sequence(source);
            auto& letters = *letters_ptr;
	    cache[b] = peel_muts_leaf_branch(*a, letters, cost);
        }
	else
        {
            vector<int> B = t.branches_before(b);
            auto& A0 = P.get_pairwise_alignment(B[0]);
            auto& A1 = P.get_pairwise_alignment(B[1]);

            auto& n_muts0 = *cache[B[0]];
            auto& n_muts1 = *cache[B[1]];

	    cache[b] = peel_muts_internal_branch(A0, A1, n_muts0, n_muts1, cost);
        }
    }

    int b_root = branches.back();
    assert(t.target(b_root) == root);

    if (t.degree(root) == 1)
    {
        auto letters_ptr = P.get_sequence(root);
        auto& A = P.get_pairwise_alignment(b_root);
        return accumulate_root_leaf(*a, *letters_ptr, A, cost, *cache[b_root]);
    }
    else if (t.degree(root) == 3)
    {
        vector<int> B = t.branches_in(root);

        auto& A0 = P.get_pairwise_alignment(B[0]);
        auto& A1 = P.get_pairwise_alignment(B[1]);
        auto& A2 = P.get_pairwise_alignment(B[2]);

        auto& n_muts1 = *cache[B[0]];
        auto& n_muts2 = *cache[B[1]];
        auto& n_muts3 = *cache[B[2]];

        return muts_root(A0, A1, A2, n_muts1, n_muts2, n_muts3, cost);
    }
    else
        std::abort();
}



object_ptr<const ParsimonyCacheBranch>
peel_muts_leaf_branch_fixed_A(const alphabet& a, const EVector& seq, const dynamic_bitset<>& mask, const matrix<int>& cost)
{
    int max_cost = max_element(cost)+1;

    int n_letters = a.size();

    assert(seq.size() == mask.count());
    auto n_muts_ptr = object_ptr<ParsimonyCacheBranch>(new ParsimonyCacheBranch(n_letters, seq.size(), mask.size()));
    auto& n_muts = *n_muts_ptr;

    n_muts.bits = mask;

    for(int i=0; i< seq.size(); i++)
    {
	int l2 = seq[i].as_int();

	if (a.is_letter(l2))
	    for(int l1=0;l1<n_letters;l1++)
		n_muts(i,l1) = cost(l1,l2);

	else if (a.is_letter_class(l2))
	    for(int l1=0;l1<n_letters;l1++)
		n_muts(i,l1) = letter_class2_cost(a,l1,l2,cost,max_cost);
	else  // wildcard
	{
	    assert(l2 == alphabet::not_gap);
	    for(int l1=0;l1<n_letters;l1++)
		n_muts(i,l1) = 0;
	}
    }

    return n_muts_ptr;
}

object_ptr<const ParsimonyCacheBranch> peel_muts_internal_branch_fixed_A(const ParsimonyCacheBranch& n_muts0, const ParsimonyCacheBranch& n_muts1, const matrix<int>& cost)
{
    int n_letters = n_muts0.n_letters;
    assert(n_muts1.n_letters == n_letters);

    auto n_muts_ptr = object_ptr<ParsimonyCacheBranch>(new ParsimonyCacheBranch(n_letters, n_muts0.bits, n_muts1.bits));
    auto& n_muts = *n_muts_ptr;

    int L = n_muts0.alignment_length;
    assert(n_muts1.alignment_length == L);

    /*-------------------- Do the peeling part------------- --------------------*/
    int i0=0;
    int i1=0;
    int i=0;
    for(int c=0;c<L;c++)
    {
        if (not n_muts0.bits.test(c) and not n_muts1.bits.test(c))
            continue;

        n_muts.bits.set(c);

	if (n_muts0.bits.test(c))
	    peel_muts(&n_muts0[i0*n_letters], &n_muts[i*n_letters], n_letters, cost);

	if (n_muts1.bits.test(c))
	    peel_muts(&n_muts1[i1*n_letters], &n_muts[i*n_letters], n_letters, cost);

        if (n_muts0.bits.test(c)) i0++;
        if (n_muts1.bits.test(c)) i1++;

        i++;
    }

    return n_muts_ptr;
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

int accumulate_root_leaf_fixed_A(const alphabet& a, const EVector& root_seq, const dynamic_bitset<>& root_mask, const ParsimonyCacheBranch& n_muts,
                                 const matrix<int>& costs, const EVector& counts)
{
    int n_letters = a.size();

    int max_cost = max_element(costs)+1;

    int total = 0;

    int i0 = 0;
    int i1 = 0;

    for(int c=0; c<n_muts.alignment_length; c++)
    {
        bool root_gap = not root_mask[c];
        bool node_gap = not n_muts.bits.test(c);

        if (root_gap and node_gap)
            continue;

        int cost = 0;
        int count = counts[c].as_int();
        assert(count > 0);

        if (root_gap)
	{
	    cost = n_muts.min(i0);
	}
        else if (node_gap)
        {
            cost = 0;
        }
        else
        {
            int l1 = root_seq[i1].as_int();

            if (a.is_letter(l1))
            {
                int c = costs(l1,0) + n_muts(i0,0);
                for(int l2=1; l2<n_letters; l2++)
                    c = std::min(c, costs(l1,l2) + n_muts(i0,l2));
                cost = c;
            }
            else if (a.is_letter_class(l1))
            {
                int c = max_cost + n_muts.max(i0);
                for(int l2=0; l2<n_letters; l2++)
                    c = std::min(c, letter_class1_cost(a, l1, l2, costs, max_cost) + n_muts(i0,l2));
                cost = c;
            }
            else
                cost = n_muts.min(i0);
        }

        total += count * cost;

        if (not node_gap)
            i0++;
        if (not root_gap)
            i1++;
    }
    assert(i0 == n_muts.bits.count());
    assert(i1 == root_seq.size());
    return total;
}
