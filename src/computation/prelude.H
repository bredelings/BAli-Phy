#ifndef PRELUDE_H
#define PRELUDE_H

#include "computation/program.H"

extern const expression_ref foldr;
extern const expression_ref foldl;
extern const expression_ref foldl_;
extern const expression_ref fmap;
extern const expression_ref fmap1;
extern const expression_ref fmap2;
extern const expression_ref take;
extern const expression_ref iterate;
extern const expression_ref sum_i;
extern const expression_ref If;
extern const expression_ref ExtendDiscreteDistribution;
extern const expression_ref MultiParameter;
extern const expression_ref fst;
extern const expression_ref snd;
extern const expression_ref get_list_index;
extern const expression_ref listArray;
extern const expression_ref length;
extern const expression_ref scale;
extern const expression_ref QExp;
extern const expression_ref RateMatrix;

extern const expression_ref MixtureModel;
extern const expression_ref n_base_models;

extern const Program Prelude;


#endif
