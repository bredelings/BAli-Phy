#include "link.H"

#include <vector>
#include <string>

#include "alignment/alignment-util.H"
#include "util/mapping.H"
#include "util/string/convert.H"

using std::vector;
using std::string;

/// \brief Re-index the leaves of tree \a T so that the labels have the same ordering as in \a A.
///
/// \param T The leaf-labelled tree.
/// \param A A multiple sequence alignment.
///
alignment remap_A_indices(alignment& A, const SequenceTree& T)
{
    vector<string> labels = T.get_labels();

    if (A.n_sequences() == T.n_leaves())
    {
	labels.resize(T.n_leaves());

    }
    else if (A.n_sequences() != T.n_nodes())
	throw myexception()<<"Cannot map alignment onto tree:\n  Alignment has "<<A.n_sequences()<<" sequences.\n  Tree has "<<T.n_leaves()<<" leaves and "<<T.n_nodes()<<" nodes.";

    for(int i=0;i<labels.size();i++)
	if (labels[i] == "")
	{
	    if (i<T.n_leaves())
		throw myexception()<<"Tree has empty label for a leaf node: not allowed!";
	    else
		throw myexception()<<"Alignment has internal node information, but tree has empty label for an internal node: not allowed!";
	}

    assert(A.n_sequences() == labels.size());

    //----- Remap leaf indices for T onto A's leaf sequence indices -----//
    try {
	vector<int> mapping = compute_mapping(labels, sequence_names(A));

	return reorder_sequences(A,mapping);
    }
    catch(const bad_mapping<string>& b)
    {
	bad_mapping<string> b2 = b;
	b2.clear();
	if (b.from == 0)
	    b2<<"Couldn't find sequence \""<<b2.missing<<"\" in alignment.";
	else
	    b2<<"Alignment sequence '"<<b2.missing<<"' not found in the tree.";
	throw b2;
    }
}

void add_internal_labels(SequenceTree& T)
{
    for(int i=0;i<T.n_nodes();i++)
	if (T.node(i).is_internal_node())
	{
	    if (T.get_label(i) == "")
		T.set_label(i, string("A") + convertToString(i));
	}
}

/// \brief  Remap the leaf indices of tree \a T to match the alignment \a A: check the result
///
/// \param A The alignment.
/// \param T The tree.
/// \param internal_sequences Should the resulting alignment have sequences for internal nodes on the tree?
///
void link(alignment& A,SequenceTree& T,bool internal_sequences) 
{
    check_names_unique(A);

    // Later, might we WANT sub-branches???
    if (has_sub_branches(T))
	remove_sub_branches(T);

    if (internal_sequences and not is_Cayley(T) and T.n_leaves() > 1) {
	assert(has_polytomy(T));
	throw myexception()<<"Cannot link a multifurcating tree to an alignment with internal sequences.";
    }

    //------ IF sequences < leaf nodes THEN complain ---------//
    if (A.n_sequences() < T.n_leaves())
	throw myexception()<<"Tree has "<<T.n_leaves()<<" leaves but Alignment only has "
			   <<A.n_sequences()<<" sequences.";

    //----- IF sequences = leaf nodes THEN maybe add internal sequences.
    else if (A.n_sequences() == T.n_leaves()) 
    {
	A = remap_A_indices(A,T);

	if (internal_sequences)
	{
	    add_internal_labels(T);
	    A = add_internal(A,T);
	    connect_leaf_characters(A,T);
	}
    }
    //----- IF sequences > leaf nodes THEN maybe complain -------//
    else if (A.n_sequences() > T.n_nodes())
	throw myexception()<<"More alignment sequences ("<<A.n_sequences()<<") than tree nodes ("<<T.n_nodes()<<")!";
    else if (A.n_sequences() < T.n_nodes())
	throw myexception()<<"Fewer alignment sequences ("<<A.n_sequences()<<") than tree nodes ("<<T.n_nodes()<<")!";
    else
    {
	A = remap_A_indices(A,T);
  
	if (not internal_sequences) 
	    A = chop_internal(A);
    }
  
    //---------- double-check that we have the right number of sequences ---------//
    if (internal_sequences)
	assert(A.n_sequences() == T.n_nodes());
    else
	assert(A.n_sequences() == T.n_leaves());

    //----- Check that each alignment sequence maps to a corresponding name in the tree -----//
    for(int i=0;i<A.n_sequences();i++)
	assert(T.get_label(i) == A.seq(i).name);

    //---- Check to see that internal nodes satisfy constraints ----//
    check_alignment(A,T,internal_sequences);
}

