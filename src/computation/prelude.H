#ifndef PRELUDE_H
#define PRELUDE_H

#include "computation/program.H"

extern const expression_ref foldr;
extern const expression_ref foldl;
extern const expression_ref foldl_;
extern const expression_ref fmap;
extern const expression_ref fmap1;
extern const expression_ref fmap2;
extern const expression_ref average;
extern const expression_ref take;
extern const expression_ref iterate;
extern const expression_ref sum_i;
extern const expression_ref If;
extern const expression_ref fst;
extern const expression_ref snd;
extern const expression_ref get_list_index;
extern const expression_ref listArray;
extern const expression_ref length;
extern const expression_ref plusplus;
extern const expression_ref ExtendDiscreteDistribution;
extern const expression_ref MixDiscreteDistributions;
extern const expression_ref UniformDiscretize;

extern const expression_ref DiscreteDistribution;
extern const expression_ref UnwrapDD;

const Program& get_Prelude();


#endif
