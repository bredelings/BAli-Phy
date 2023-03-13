#ifndef TRIPLETS_H
#define TRIPLETS_H

#include <vector>
#include "alphabet.H"
#include "util/owned-ptr.H"
#include "util/assert.hh"

/// An alphabet of triplets of nucleotides
class Triplets: public alphabet {
protected:
    owned_ptr<Nucleotides> N;

    /// (codon,position) -> nucleotide
    std::vector<std::vector<int> > sub_nuc_table;
    /// (n1,n2,n3) -> codon
    std::vector<std::vector<std::vector<int> > > codon_table;

    void setup_sub_nuc_table();
    void setup_letter_classes();
public:
    virtual Triplets* clone() const {return new Triplets(*this);}

    virtual std::string letter_name() const {return "triplet";};

    /// The alphabet of nucleotides that we construct triplets from
    const Nucleotides& getNucleotides() const {return *N;}

    /// The alphabet of nucleotides that we construct triplets from
    int sub_nuc(int codon,int pos) const;

    /// Find the triplet made of nucleotides n1, n2, and n3
    int get_triplet(int n1, int n2, int n3) const;

    std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=1.0) const;

    std::vector<int> operator()(const std::string&) const;

    Triplets(const Nucleotides& N);
    Triplets(const std::string& s,const Nucleotides& N);
};


/// Compute nucleotide counts from codon counts
std::valarray<double> get_nucleotide_counts_from_codon_counts(const Triplets& C,const std::valarray<double>& C_counts);

/// Compute codon frequences from nucleotide frequencies if the nucleotide positions in the codon are independent
std::valarray<double> get_codon_frequencies_from_independent_nucleotide_frequencies(const Triplets& C,const std::valarray<double>& fN );

#endif
