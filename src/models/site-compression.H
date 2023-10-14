#ifndef SITE_COMPRESSION_H
#define SITE_COMPRESSION_H

#include <tuple>
#include <vector>
#include "tree/tree.H"
#include "alignment/alignment.H"

struct compressed_alignment
{
    alignment compressed;
    std::vector<int> counts;
    std::vector<int> mapping;

    /// The i-th sequence
    const sequence& seq(int i) const { return compressed.seq(i);}
    /// The i-th sequence
    sequence& seq(int i) { return compressed.seq(i);}

    const std::vector<sequence>& seqs() const {return compressed.seqs();}

    /// Does sequence i have a gap at position j ?
    bool gap(int i,int j) const {return compressed.gap(mapping[i],j);}
    /// Does sequence i have an unknown at position j ?
    bool unknown(int i,int j) const {return compressed.unknown(mapping[i],j);}
    /// Does sequence i have an character at position j ?
    bool character(int i,int j) const {return compressed.character(mapping[i],j);}

    int compressed_length() const {return compressed.length();}

    /// Number of columns
    int length() const {return mapping.size();}

    /// Total number of sequences
    int n_sequences() const {return compressed.n_sequences();}

    /// Access the alignment's alphabet.
    const alphabet& get_alphabet() const {return compressed.get_alphabet();}

    /// Access the alignment's alphabet.
    bool has_alphabet() const {return compressed.has_alphabet();}
};

#endif
