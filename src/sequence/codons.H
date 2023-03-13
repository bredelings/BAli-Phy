#ifndef CODONS_H
#define CODONS_H

#include "alphabet.H"

/// A Triplets alphabet with a mapping to an AminoAcids alphabet
class Codons: public Triplets {
protected:
    owned_ptr<AminoAcids> A;
    owned_ptr<Genetic_Code> G;
    std::vector<int> translation_table;

    void setup_table();

public:
    virtual Codons* clone() const {return new Codons(*this);}

    virtual std::string letter_name() const {return "codon";};

    /// Is letter codon a stop codon?
    bool stop_codon(int codon) const {
	assert(0 <= codon && codon <translation_table.size() );
	int aa = translation_table[codon];
	return A->is_stop(aa);
    }

    /// What amino acid does codon map to?
    int translate(int codon) const; 

    /// What AminoAcids alphabet are we using?
    const AminoAcids& getAminoAcids() const {return *A;}

    const Genetic_Code& getGenetic_Code() const {return *G;}

    /// Construct a Codons alphabet from Nucleotides, AminoAcids, and a Genetic Code
    Codons(const Nucleotides& N1,const AminoAcids& A1, const Genetic_Code& G);
};


#endif
