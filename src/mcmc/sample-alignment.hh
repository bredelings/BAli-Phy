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
// Successful partitions remain resampled even if another partition is unavailable;
// absence means that no complete joint proposal correction can be returned.
Availability<ProbDensity> sample_alignment(Parameters& P, int b, bool initial_state_valid=true);

#endif
