#ifndef SAMPLE_ALIGNMENT_H
#define SAMPLE_ALIGNMENT_H

#include <optional>
#include "models/parameters.hh"
#include "dp/dp-matrix.hh"
#include "probability/availability.hh"
#include "util/math/ProbDensity.hh"

std::shared_ptr<DPmatrixSimple> sample_alignment_forward(data_partition P, const TreeInterface&,
                                                         const indel::PairHMM& hmm, int b,
                                                         std::optional<int> bandwidth);
std::pair<std::shared_ptr<DPmatrixSimple>,Availability<log_double_t>>
sample_alignment_base(mutable_data_partition P, const indel::PairHMM& hmm, int b,
                      std::optional<int> bandwidth);
std::pair<std::shared_ptr<DPmatrixSimple>,Availability<log_double_t>>
sample_alignment_base(mutable_data_partition P, int b, std::optional<int> bandwidth);
void sample_alignment(Parameters&, int b);
// The caller owns the working context and discards it on a table refusal. A completed
// calculation with no joint correction can still have resampled individual partitions.
// Trial lengths can give the incoming alignment zero probability. Skip before/after checks there.
Availability<ProbDensity> sample_alignment_with_ratio(Parameters&, int b, bool check_initial=true);

#endif
