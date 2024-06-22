#ifndef LIKELIHOODSEV_H
#define LIKELIHOODSEV_H
#include <vector>
#include "util/matrix.H"
#include "util/math/log-double.H"
#include "substitution/cache.H"
#include "computation/expression/expression_ref.H"

class data_partition;

/// A namespace for functions related to calculating the substitution likelihood.
namespace substitution {
    log_double_t calc_probability_SEV(const Likelihood_Cache_Branch& LCB1,
                                           const Likelihood_Cache_Branch& LCB2,
                                           const Likelihood_Cache_Branch& LCB3,
                                           const Matrix& F,
					   const EVector& counts);

    log_double_t calc_at_deg2_probability_SEV(const Likelihood_Cache_Branch& LCB1,
                                                const Likelihood_Cache_Branch& LCB2,
                                                const Matrix& F,
                                                const EVector& counts);
    log_double_t calc_prob_at_root_SEV(const EVector& LCN,
				       const EVector& LCB,
				       const Matrix& F,
				       const EVector& counts);

    log_double_t calc_prob_SEV(const EVector& LCN,
				     const EVector& LCB,
				     const Matrix& FF,
				     const EVector& counts);

    object_ptr<const Likelihood_Cache_Branch>
    peel_leaf_branch_SEV(const Likelihood_Cache_Branch& nodeCLV, const EVector& transition_P);

    
    object_ptr<const Likelihood_Cache_Branch>
    peel_internal_branch_SEV(const Likelihood_Cache_Branch& LCB1,
                             const Likelihood_Cache_Branch& LCB2,
                             const EVector& transition_P);

    object_ptr<const Likelihood_Cache_Branch>
    peel_deg2_branch_SEV(const Likelihood_Cache_Branch& LCB1,
                         const EVector& transition_P);

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch_toward_root_SEV(const EVector& LCN,
                    const EVector& LCB,
                    const EVector& transition_P);

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch_away_from_root_SEV(const EVector& LCN,
				   const EVector& LCB,
				   const EVector& transition_P,
				   const Matrix& ff);

    object_ptr<const Likelihood_Cache_Branch>
    simple_sequence_likelihoods_SEV(const EPair& sequence_mask,
				    const alphabet& a,
				    const EVector& smap,
				    int n_models);

    object_ptr<const SparseLikelihoods>
    simple_sequence_likelihoods2_SEV(const EPair& sequence_mask,
				     const alphabet& a,
				     const EVector& smap,
				     int n_models);

    Vector<std::pair<int,int>> sample_root_sequence_SEV(const EVector& LCN,
							const EVector& LCB,
							const Matrix& F,
							const EVector& compressed_col_for_col);

    Vector<std::pair<int,int>> sample_sequence_SEV(const Vector<std::pair<int,int>>& parent_seq,
						   const EVector& LCN,
						   const EVector& transition_Ps,
						   const EVector& LCB,
						   const EVector& compressed_col_for_col);
}
#endif
