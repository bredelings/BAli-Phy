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
};

compressed_alignment compress_alignment(const alignment& A, const Tree& t);
compressed_alignment compress_alignment(const alignment& A, int n);

#endif
