#ifndef TWOWAY_PTMH_
#define TWOWAY_PTM_H

#include "alignment.H"
#include "imodel.H"

ublas::matrix<int> get_FS_counts(const alignment& A,int n1, int n2, const indel::PairTransducer& PTM);
ublas::matrix<int> get_root_transition_counts(const alignment& A,int n,const TransducerIndelModel& T);
#endif
