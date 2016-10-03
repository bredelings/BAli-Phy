#include "substitution/parsimony.H"
#include "dp/hmm.H"

using std::vector;

int max_element(const matrix<int>& M)
{
    auto data = M.begin();
    int S = M.size();
    int m = data[0];
    for(int i=1;i<S;i++)
	m = std::max(m, data[i]);
    return m;
}

inline int min(int* x, int size)
{
    int c = x[0];
    for(int i=1;i<size;i++)
	c = std::min(c,x[i]);
    return c;
}

inline int max(int* x, int size)
{
    int c = x[0];
    for(int i=1;i<size;i++)
	c = std::max(c,x[i]);
    return c;
}

int* peel_muts_leaf_branch(int b, const data_partition& P, const matrix<int>& cost)
{
    int max_cost = max_element(cost)+1;

    auto& a = P.get_alphabet();
    int n_letters = a.size();

    int source = P.t().source(b);
    const vector<int>& letters = P.get_sequence(source);
    int L = P.seqlength(source);

    auto n_muts = new int[L * n_letters];
    assert(letters.size() == L);

    for(int i=0;i<L;i++)
    {
	int l1 = letters[i];
	if (a.is_letter_class(L))
	    for(int l2=0;l2<n_letters;l2++)
	    {
		int c = max_cost;
		for(int l=0;l<n_letters;l++)
		    if (a.matches(l,l1))
			c = std::min(c, cost(l,l2));
		n_muts[n_letters*i + l2] = c;
	    }
	else if (a.is_letter(l1))
	    for(int l2=0;l2<n_letters;l2++)
		n_muts[n_letters*i + l2] = cost(l1,l2);
	else  // wildcard
	{
	    assert(l1 == alphabet::not_gap);
	    for(int l2=0;l2<n_letters;l2++)
		n_muts[n_letters*i + l2] = 0;
	}
    }
    return n_muts;
}

void peel_muts(int* n_muts1, int* n_muts2, int n_letters, const matrix<int>& cost)
{
    for(int l2=0;l2<n_letters;l2++)
    {
	int c = cost(l2,0) + n_muts1[0];
	for(int l1=1;l1<n_letters;l1++)
	    c = std::min(c, cost(l2,l1) + n_muts1[l1]);
	n_muts2[l2] += c;
    }
}

int* peel_muts_internal_branch(int b, const data_partition& P, const matrix<int>& cost,
			       const vector<int*>& cache, int& total)
{
    auto& a = P.get_alphabet();
    int n_letters = a.size();

    auto t = P.t();
    int source = t.source(b);
    int L = P.seqlength(source);

    auto n_muts = new int[L * n_letters];

    // find the names of the (two) branches behind b
    vector<int> B = t.branches_before(b);
      
    auto a0 = convert_to_bits(P.get_pairwise_alignment(B[0]), 0, 2);
    auto a1 = convert_to_bits(P.get_pairwise_alignment(B[1]), 1, 2);
    auto a012 = Glue_A(a0, a1);

    // get the relationships with the sub-alignments for the (two) branches behind b0
    matrix<int> index = get_indices_from_bitpath_w(a012, {0,1}, 1<<2);
    assert(index.size1() == L);
      
    auto n_muts0 = cache[B[0]];
    auto n_muts1 = cache[B[1]];

    /*-------------------- Do the peeling part------------- --------------------*/
    for(int i=0;i<L;i++)
    {
	for(int l2=0;l2<n_letters;l2++)
	    n_muts[i*n_letters + l2] = 0;

	int i0 = index(i,0);
	if (i0 != alphabet::gap)
	    peel_muts(n_muts0 + i0*n_letters, n_muts + i*n_letters, n_letters, cost);

	int i1 = index(i,1);
	if (i1 != alphabet::gap)
	    peel_muts(n_muts1 + i1*n_letters, n_muts + i*n_letters, n_letters, cost);
    }

    /*-------------------- Do the other_subst collection part -------------------*/
    // min_i (min_j (cost(i,j) + n_muts[j])) <= min_i ( 0 + n_muts[i] )
    // min_i (min_j (cost(i,j) + n_muts[j])) >= min_i ( min_j ( n_muts[j] ) )
    // min_i (min_j (cost(i,j) + n_muts[j])) == min_i ( n_muts[i] )
    matrix<int> index_collect = get_indices_from_bitpath_wo(a012, {0,1}, 1<<2);
    for(int i=0;i<index_collect.size1();i++)
    {
	int i0 = index_collect(i,0);
	int i1 = index_collect(i,1);

	if (i0 != alphabet::gap)
	{
	    assert(i1 == alphabet::gap);
	    total += min(n_muts0 + i0*n_letters, n_letters);
	}
	else if (i1 != alphabet::gap)
	{
	    assert(i0 == alphabet::gap);
	    total += min(n_muts1 + i1*n_letters, n_letters);
	}
    }

    return n_muts;
}

int accumulate_root_leaf(int b, const data_partition& P, const matrix<int>& cost, int* n_muts)
{
    int root = P.t().target(b);
    assert(P.t().is_leaf_node(root));
    const auto& letters = P.get_sequence(root);

    auto& a = P.get_alphabet();
    int n_letters = a.size();

    int max_cost = max_element(cost)+1;

    auto a01 = convert_to_bits(P.get_pairwise_alignment(b), 0, 1);

    matrix<int> index = get_indices_from_bitpath_w(a01, {0,1}, 1<<0);

    int total = 0;
    for(int i=0;i<index.size1();i++)
    {
	int i0 = index(i,0);
	int i1 = index(i,1);
	if (i1 == alphabet::gap or i1 == alphabet::not_gap)
	{
	    total += min(n_muts + i0*n_letters, n_letters);
	    continue;
	}

	int l1 = letters[i1];
	if (a.is_letter(l1))
	{
	    int c = cost(l1,0) + n_muts[i0*n_letters + 0];
	    for(int l2=1; l2<n_letters; l2++)
		c = std::min(c, cost(l1,l2) + n_muts[i0*n_letters + l2]);
	    total += c;
	}
	else if (a.is_letter_class(l1))
	{
	    int c = max_cost + max(n_muts + i0*n_letters, n_letters);
	    for(int l2=0; l2<n_letters; l2++)
		for(int l=0; l<n_letters; l++)
		    if (a.matches(l,l1))
			c = std::min(c, cost(l,l2) + n_muts[i0*n_letters + l2]);
	    total += c;
	}
    }
    return total;
}

int n_mutations(const data_partition& P, const matrix<int>& cost)
{
    int total = 0;
  
    int root = 0;
    auto t = P.t();

    if (t.n_nodes() < 2) return 0;

    vector<int*> cache(t.n_branches() * 2);

    const auto branches = t.all_branches_toward_node(root);
    for(int b: branches)
    {
	if (t.is_leaf_node(t.source(b)))
	    cache[b] = peel_muts_leaf_branch(b, P, cost);
	else
	    cache[b] = peel_muts_internal_branch(b, P, cost, cache, total);
    }

    int b_root = branches.back();
    assert(t.target(b_root) == root);

    total += accumulate_root_leaf(branches.back(), P, cost, cache[b_root]);

    for(auto p: cache)
	delete[] p;

    return total;
}

int n_mutations(const data_partition& P)
{
    return n_mutations(P, unit_cost_matrix(P.get_alphabet()));
}
