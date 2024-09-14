#ifndef RNAEDITS_H
#define RNAEDITS_H

#include <vector>
#include "alphabet.H"
#include "util/owned-ptr.H"
#include "util/assert.hh"

/// An alphabet of triplets of nucleotides
class RNAEdits: public alphabet {
protected:
    owned_ptr<Nucleotides> N;

    /// (codon,position) -> nucleotide
    std::vector<std::vector<int> > sub_nuc_table;
    /// (n1,n2) -> double
    std::vector<std::vector<int> > RNAEdit_table;

    void setup_table();
    void setup_sub_nuc_table();
    void setup_letter_classes();
public:
    virtual RNAEdits* clone() const {return new RNAEdits(*this);}

    virtual std::string letter_name() const {return "RNAEdit";};

    /// The alphabet of nucleotides that we construct triplets from
    const Nucleotides& getNucleotides() const {return *N;}

    /// The alphabet of nucleotides that we construct triplets from
    int sub_nuc(int codon,int pos) const;

    /// Find the triplet made of nucleotides n1, n2
    int get_doublet(int n1, int n2) const;

    bool is_watson_crick(int i) const;
    bool is_mismatch(int i) const;
    bool is_wobble_pair(int i) const;

    int n_changes(int i, int j) const;

    std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=1.0) const;

    std::vector<int> operator()(const std::string&) const;

    RNAEdits(const Nucleotides& N);
    RNAEdits(const std::string& s,const Nucleotides& N);
};

#endif
