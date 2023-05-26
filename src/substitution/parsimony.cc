#include "substitution/parsimony.H"
#include "dp/hmm.H"
#include "dp/2way.H"

using std::vector;
using boost::dynamic_bitset;

struct ParsimonyCacheBranch: public Object
{
    int n_letters;
    boost::dynamic_bitset<> bits;
    int sequence_length;
    int alignment_length;
    std::vector<int> n_muts;

    int other_subst = 0;

    ParsimonyCacheBranch* clone() const {return new ParsimonyCacheBranch(*this);}

          int& operator[](int i)       {return n_muts[i];}
    const int& operator[](int i) const {return n_muts[i];}

          int& operator()(int c,int l)       {return n_muts[c*n_letters+l];}
    const int& operator()(int c,int l) const {return n_muts[c*n_letters+l];}

    int min(int c) const
    {
        int x = (*this)(c,0);
        for(int i=1;i<n_letters;i++)
            x = std::min(x, (*this)(c,i));
        return x;
    }

    int max(int c) const
    {
        int x = (*this)(c,0);
        for(int i=1;i<n_letters;i++)
            x = std::max(x, (*this)(c,i));
        return x;
    }

    ParsimonyCacheBranch(int n, int l1)
        :n_letters(n),
         sequence_length(l1),
         n_muts(n_letters*sequence_length)
        {
            assert(n > 0);
        }

    ParsimonyCacheBranch(int n, int l1, int l2)
        :n_letters(n),
         bits(l2),
         sequence_length(l1),
         alignment_length(l2),
         n_muts(n_letters*sequence_length)
        {
            assert(n > 0);
            assert(l1 >= 0);
            assert(l2 >= 0);
        }

    ParsimonyCacheBranch(int n, const dynamic_bitset<>& bits1, const dynamic_bitset<>& bits2)
        :n_letters(n),
         bits(bits1 | bits2),
         sequence_length(bits.count()),
         alignment_length(bits1.size()),
         n_muts(n_letters*sequence_length)
        {
            assert(n > 0);
            assert(bits1.size() == bits2.size());
        }
};

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
	if (a.is_letter_class(l1))
	    for(int l2=0;l2<n_letters;l2++)
	    {
		int c = max_cost;
		for(int l=0;l<n_letters;l++)
		    if (a.matches(l,l1))
			c = std::min(c, cost(l,l2));
		n_muts(i, l2) = c;
	    }
	else if (a.is_letter(l1))
	    for(int l2=0;l2<n_letters;l2++)
		n_muts(i, l2) = cost(l1,l2);
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
peel_muts_internal_branch(const alphabet& a,
                          const pairwise_alignment_t& A0, const pairwise_alignment_t& A1,
                          const ParsimonyCacheBranch n_muts0,
                          const ParsimonyCacheBranch n_muts1,
                          const matrix<int>& cost)
{
    int n_letters = a.size();

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

int accumulate_root_leaf(int b, const data_partition& P, const matrix<int>& cost, const ParsimonyCacheBranch& n_muts)
{
    int root = P.t().target(b);
    assert(P.t().is_leaf_node(root));
    const auto letters_ptr = P.get_sequence(root);
    const auto& letters = *letters_ptr;

    auto a = P.get_alphabet();
    int n_letters = a->size();

    int max_cost = max_element(cost)+1;

    auto a01 = convert_to_bits(P.get_pairwise_alignment(b), 0, 1);

    matrix<int> index = get_indices_from_bitpath_w(a01, {0,1}, 1<<0);

    int total = n_muts.other_subst;
    for(int i=0;i<index.size1();i++)
    {
	int i0 = index(i,0);
	int i1 = index(i,1);
	if (i1 == alphabet::gap or i1 == alphabet::not_gap)
	{
	    total += n_muts.min(i0);
	    continue;
	}

	int l1 = letters[i1].as_int();
	if (a->is_letter(l1))
	{
	    int c = cost(l1,0) + n_muts(i0, 0);
	    for(int l2=1; l2<n_letters; l2++)
		c = std::min(c, cost(l1,l2) + n_muts(i0, l2));
	    total += c;
	}
	else if (a->is_letter_class(l1))
	{
	    int c = max_cost + n_muts.max(i0);
	    for(int l2=0; l2<n_letters; l2++)
		for(int l=0; l<n_letters; l++)
		    if (a->matches(l,l1))
			c = std::min(c, cost(l,l2) + n_muts(i0, l2));
	    total += c;
	}
    }
    return total;
}

int n_mutations_variable_A(const data_partition& P, const matrix<int>& cost)
{
    int root = 0;
    auto t = P.t();

    if (t.n_nodes() < 2) return 0;

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

	    cache[b] = peel_muts_internal_branch(*a, A0, A1, n_muts0, n_muts1, cost);
        }
    }

    int b_root = branches.back();
    assert(t.target(b_root) == root);

    return accumulate_root_leaf(branches.back(), P, cost, *cache[b_root]);
}



object_ptr<const ParsimonyCacheBranch>
peel_muts_leaf_branch_fixed_A(int source, const alignment& A, const matrix<int>& cost)
{
    int max_cost = max_element(cost)+1;

    auto& a = A.get_alphabet();
    int n_letters = a.size();

    auto n_muts_ptr = object_ptr<ParsimonyCacheBranch>(new ParsimonyCacheBranch(n_letters, A.seqlength(source), A.length()));
    auto& n_muts = *n_muts_ptr;

    int i=0;
    for(int c=0; c< A.length(); c++)
    {
        if (not A.character(c, source)) continue;

        n_muts.bits.set(c);

	int l2 = A(c, source);

	if (a.is_letter_class(l2))
	    for(int l1=0;l1<n_letters;l1++)
		n_muts(i,l1) = letter_class2_cost(a,l1,l2,cost,max_cost);

	else if (a.is_letter(l2))
	    for(int l1=0;l1<n_letters;l1++)
		n_muts(i,l1) = cost(l1,l2);

	else  // wildcard
	{
	    assert(l2 == alphabet::not_gap);
	    for(int l1=0;l1<n_letters;l1++)
		n_muts(i,l1) = 0;
	}

        i++;
    }
    assert(n_muts.sequence_length == i);

    return n_muts_ptr;
}

object_ptr<const ParsimonyCacheBranch> peel_muts_internal_branch_fixed_A(const ParsimonyCacheBranch& n_muts0, const ParsimonyCacheBranch& n_muts1, const alphabet& a, const matrix<int>& cost)
{
    int n_letters = a.size();

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

int accumulate_root_leaf_fixed_A(int root, const alignment& A, const ParsimonyCacheBranch& n_muts, const alphabet& a, const matrix<int>& cost)
{
    int n_letters = a.size();

    int max_cost = max_element(cost)+1;

    int total = 0;

    int i0 = 0;
    int i1 = 0;
    for(int c=0; c<n_muts.alignment_length; c++)
    {
        bool root_gap = not A.character(c,root);
        bool node_gap = not n_muts.bits.test(c);

        if (root_gap and node_gap)
            continue;

        if (root_gap)
	{
	    total += n_muts.min(i0);
	}
        else
        {
            int l1 = A(c,root);
            if (a.is_letter(l1))
            {
                int c = cost(l1,0) + n_muts(i0,0);
                for(int l2=1; l2<n_letters; l2++)
                    c = std::min(c, cost(l1,l2) + n_muts(i0,l2));
                total += c;
            }
            else if (a.is_letter_class(l1))
            {
                int c = max_cost + n_muts.max(i0);
                for(int l2=0; l2<n_letters; l2++)
                    c = std::min(c, letter_class1_cost(a,l1,l2,cost,max_cost) + n_muts(i0,l2));
                total += c;
            }
            else
                total += n_muts.min(i0);
        }

        if (not node_gap)
            i0++;
        if (not root_gap)
            i1++;
    }
    assert(i0 == n_muts.bits.count());
    assert(i1 == A.seqlength(root));
    return total;
}

int n_mutations_fixed_A(const data_partition& P, const matrix<int>& cost)
{
    auto t = P.t();

    if (t.n_nodes() < 2) return 0;

    auto A = P.ancestral_sequence_alignment().as_<Box<alignment>>();

    vector<object_ptr<const ParsimonyCacheBranch>> cache(t.n_branches() * 2, nullptr);

    int root = 0;
    const auto branches = t.all_branches_toward_node(root);

    for(int b: branches)
    {
	if (t.is_leaf_node(t.source(b)))
        {
            int source = P.t().source(b);

	    cache[b] = peel_muts_leaf_branch_fixed_A(source, A, cost);
        }
	else
        {
            // find the names of the (two) branches behind b
            auto B = t.branches_before(b);

            auto& n_muts0 = *cache[B[0]];
            auto& n_muts1 = *cache[B[1]];

	    cache[b] = peel_muts_internal_branch_fixed_A(n_muts0, n_muts1, *P.get_alphabet(), cost);

            cache[B[0]] = nullptr;
            cache[B[1]] = nullptr;
        }
    }

    int b_root = branches.back();
    assert(t.target(b_root) == root);

    return accumulate_root_leaf_fixed_A(root, A, *cache[b_root], *P.get_alphabet(), cost);
}

int n_mutations(const data_partition& P, const matrix<int>& cost)
{
    if (P.has_IModel())
        return n_mutations_variable_A(P,cost);
    else
        return n_mutations_fixed_A(P,cost);
}

int n_mutations(const data_partition& P)
{
    return n_mutations(P, unit_cost_matrix(*P.get_alphabet()));
}
