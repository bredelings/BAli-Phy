#ifndef N_INDELS_H
#define N_INDELS_H

#include "tree.H"
#include "alignment.H"

std::vector<int> get_indel_lengths(const alignment& A,const Tree& T, int b);
unsigned total_length_indels(const alignment& A,const Tree& T,int b);
unsigned n_indels(const alignment& A,const Tree& T, int b);

unsigned total_length_indels(const alignment& A,const Tree& T);
unsigned n_indels(const alignment& A,const Tree& T);

#endif
