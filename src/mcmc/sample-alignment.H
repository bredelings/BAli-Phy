#ifndef SAMPLE_ALIGNMENT_H
#define SAMPLE_ALIGNMENT_H

#include <optional>
#include "models/parameters.H"
#include "dp/dp-matrix.H"
#include "util/math/ProbDensity.H"

std::shared_ptr<DPmatrixSimple> sample_alignment_forward(data_partition P, const TreeInterface&, const indel::PairHMM& hmm, int b, std::optional<int> bandwidth);
std::pair<std::shared_ptr<DPmatrixSimple>,std::optional<log_double_t>> sample_alignment_base(mutable_data_partition P, const indel::PairHMM& hmm, int b, std::optional<int> bandwidth);
std::pair<std::shared_ptr<DPmatrixSimple>,std::optional<log_double_t>> sample_alignment_base(mutable_data_partition P, int b, std::optional<int> bandwidth);
ProbDensity sample_alignment(Parameters& P, int b, bool initial_state_valid=true);

#endif
