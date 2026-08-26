#include "codons.hh"
#include "ambiguity.hh"

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

    // Compute the indices for the remaining ones
    for(int i=0;i<size();i++) 
    {
	int n1 = sub_nuc(i,0);
	int n2 = sub_nuc(i,1);
	int n3 = sub_nuc(i,2);

	translation_table[i] = A->find_letter(GAA.letter(G->translate(n1,n2,n3)));
    }
}

/// What amino acid does codon map to?
int Codons::translate(int codon) const
{
    if (codon == alphabet::gap or codon == alphabet::not_gap or codon == alphabet::unknown)
	return codon;

    assert(is_letter(codon));
    assert(codon < translation_table.size());
    return translation_table[codon];
}

// An ambiguity has a definite translation exactly when all selected codons
// map to the same amino acid; otherwise it remains unconstrained non-gap.
int Codons::translate_observation(int codon, const ambiguity_database& ambiguities) const
{
    assert(ambiguities.n_states() == n_letters());

    if (not is_ambiguity(codon))
        return translate(codon);

    int amino_acid = -1;
    const auto& mask = ambiguities.mask(codon);
    for (auto state = mask.find_first(); state != bitmask_t::npos; state = mask.find_next(state))
    {
        int translated = translate(state);
        if (amino_acid != -1 and amino_acid != translated)
            return alphabet::not_gap;
        amino_acid = translated;
    }
    assert(amino_acid != -1);
    return amino_acid;
}


Codons::Codons(const Nucleotides& N1,const AminoAcids& A1, const Genetic_Code& G_)
    :Triplets(N1),A(A1),G(G_)
{
    setup_table();

    name = string("Codons(") + getNucleotides().name + ","+ G->name() + ")";
}
