#include "link.H"

#include <vector>
#include <string>

#include "alignment/alignment-util.H"
#include "util/mapping.H"
#include "util/string/convert.H"

#include "alignment/alignment-util2.H"
#include "models/TreeInterface.H"

using std::vector;
using std::string;

/// \brief Re-index the leaves of tree \a T so that the labels have the same ordering as in \a A.
///
/// \param T The leaf-labelled tree.
/// \param A A multiple sequence alignment.
///

alignment link_A(alignment A, const vector<string>& labels, const TreeInterface& T, bool internal_sequences)
{
    //------ IF sequences < leaf nodes THEN complain ---------//
    if (A.n_sequences() < T.n_leaves())
	throw myexception()<<"Tree has "<<T.n_leaves()<<" leaves but Alignment only has "
			   <<A.n_sequences()<<" sequences.";

    //----- IF sequences = leaf nodes THEN maybe add internal sequences.
    else if (A.n_sequences() == T.n_leaves()) 
    {
	A = remap_A_indices(A, labels, T.n_leaves(), T.n_nodes());

	if (internal_sequences)
	{
	    A = add_internal(A,labels,T);
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
	A = remap_A_indices(A, labels, T.n_leaves(), T.n_nodes());
  
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
	assert(labels[i] == A.seq(i).name);

    //---- Check to see that internal nodes satisfy constraints ----//
    check_alignment(A,T,internal_sequences);

    return A;
}
