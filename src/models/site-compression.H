#ifndef SITE_COMPRESSION_H
#define SITE_COMPRESSION_H

#include <tuple>
#include <vector>
#include "TreeInterface.H"
#include "alignment/alignment.H"

std::tuple<alignment, std::vector<int>, std::vector<int>>
compress_alignment(const alignment& A, const TreeInterface& t);


#endif
