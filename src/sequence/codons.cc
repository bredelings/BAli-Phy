#include "codons.H"

using std::string;

void Codons::setup_table() 
{
    const AminoAcidsWithStop& GAA = G->get_amino_acids();

    // Remove codons/letters in (*this) do not map to amino acids in *A.
    for(int i=size()-1; i>=0; i--) 
    {
	int n1 = sub_nuc(i,0);
	int n2 = sub_nuc(i,1);
	int n3 = sub_nuc(i,2);

	string aa_letter = GAA.letter(G->translate(n1,n2,n3));
	if (not A->contains(aa_letter))
	    remove(i);
    }

    translation_table.resize( size() );
    setup_sub_nuc_table();
    setup_letter_classes();

    // Compute the indices for the remaining ones
    for(int i=0;i<size();i++) 
    {
	int n1 = sub_nuc(i,0);
	int n2 = sub_nuc(i,1);
	int n3 = sub_nuc(i,2);

	translation_table[i] = (*A)[ GAA.letter(G->translate(n1,n2,n3)) ];
    }
}

/// What amino acid does codon map to?
int Codons::translate(int codon) const
{
    if (codon == alphabet::gap or codon == alphabet::not_gap)
	return codon;

    assert(codon >= 0 and codon < translation_table.size() );

    int aa = -1;
    if (is_letter(codon))
	aa = translation_table[codon];
    else if (is_letter_class(codon))
    {
	for(int i=0;i<size();i++)
	    if (matches(i,codon)) {
		int aa_ = translate(i);
		if (aa != -1 and aa != aa_)
		    return alphabet::not_gap;
		aa = aa_;
	    }
    }
    return aa;
}


Codons::Codons(const Nucleotides& N1,const AminoAcids& A1, const Genetic_Code& G_)
    :Triplets(N1),A(A1),G(G_)
{
    setup_table();
    setup_sub_nuc_table();
    setup_letter_classes();

    name = string("Codons(") + getNucleotides().name + ","+ G->name() + ")";
}
