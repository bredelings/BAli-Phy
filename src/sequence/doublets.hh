#ifndef DOUBLETS_H
#define DOUBLETS_H

#include <vector>
#include "alphabet.hh"
#include "util/owned-ptr.hh"
#include "util/assert.hh"

/// An alphabet of triplets of nucleotides
class Doublets: public alphabet {
protected:
    owned_ptr<Nucleotides> N;

    /// (codon,position) -> nucleotide
    std::vector<std::vector<int> > sub_nuc_table;

    /// (first nucleotide, second nucleotide) -> exact doublet state.
    std::vector<std::vector<int>> doublet_for_components;

    void setup_sub_nuc_table();
public:
    using alphabet::lookup;

    virtual Doublets* clone() const {return new Doublets(*this);}

    virtual std::string letter_name() const {return "doublet";};

    /// The alphabet of nucleotides that we construct triplets from
    const Nucleotides& getNucleotides() const {return *N;}

    /// The alphabet of nucleotides that we construct triplets from
    int sub_nuc(int codon,int pos) const;

    /// Return the exact doublet state made from two exact nucleotide states.
    int get_doublet(int n1, int n2) const;

    bool is_watson_crick(int i) const;
    bool is_mismatch(int i) const;
    bool is_wobble_pair(int i) const;

    int n_changes(int i, int j) const;

    std::valarray<double> get_frequencies_from_counts(const std::valarray<double>&,double=1.0) const;

    std::optional<bitmask_t> mask_for_symbol(const std::string& symbol) const override;

    std::pair<std::string, bool> lookup(const bitmask_t& mask) const override;

    void validate_sequence(const std::string& sequence) const override;

    Doublets(const Nucleotides& N);
    Doublets(const std::string& s,const Nucleotides& N);
};

#endif
