#ifndef LIKELIHOODSEV_H
#define LIKELIHOODSEV_H
#include <span>
#include <vector>
#include "substitution/component-state.H"
#include "util/dense-matrix.H"
#include "util/math/log-double.H"
#include "substitution/cache.H"
#include "computation/runtime/ast.H"

class data_partition;

/// A namespace for functions related to calculating the substitution likelihood.
namespace substitution {
    log_double_t calc_probability_SEV(const Likelihood_Cache_Branch& LCB1,
                                           const Likelihood_Cache_Branch& LCB2,
                                           const Likelihood_Cache_Branch& LCB3,
                                           const DenseMatrix<double>& F,
					   Eigen::Ref<const DenseVector<int>> counts);

    log_double_t calc_at_deg2_probability_SEV(const Likelihood_Cache_Branch& LCB1,
                                                const Likelihood_Cache_Branch& LCB2,
                                                const DenseMatrix<double>& F,
                                                Eigen::Ref<const DenseVector<int>> counts);
    log_double_t calc_prob_at_root_SEV(const R::RVector& LCN,
				       const R::RVector& LCB,
				       const DenseMatrix<double>& F,
				       Eigen::Ref<const DenseVector<int>> counts);

    log_double_t calc_prob_at_root_variable_SEV(const R::RVector& LCN,
						const R::RVector& LCB,
						const DenseMatrix<double>& F,
						Eigen::Ref<const DenseVector<int>> counts);

    log_double_t calc_prob_SEV(const R::RVector& LCN,
				     const R::RVector& LCB,
				     const DenseMatrix<double>& FF,
				     Eigen::Ref<const DenseVector<int>> counts);

    object_ptr<const Likelihood_Cache_Branch>
    peel_leaf_branch_SEV(const Likelihood_Cache_Branch& nodeCLV, const R::RVector& transition_P);

    
    object_ptr<const Likelihood_Cache_Branch>
    peel_internal_branch_SEV(const Likelihood_Cache_Branch& LCB1,
                             const Likelihood_Cache_Branch& LCB2,
                             const R::RVector& transition_P);

    object_ptr<const Likelihood_Cache_Branch>
    peel_deg2_branch_SEV(const Likelihood_Cache_Branch& LCB1,
                         const R::RVector& transition_P);

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch_toward_root_SEV(const R::RVector& LCN,
                    const R::RVector& LCB,
                    const R::RVector& transition_P);

    object_ptr<const Likelihood_Cache_Branch>
    peel_branch_away_from_root_SEV(const R::RVector& LCN,
				   const R::RVector& LCB,
				   const R::RVector& transition_P,
				   const DenseMatrix<double>& ff);

    object_ptr<const Likelihood_Cache_Branch>
    simple_sequence_likelihoods_SEV(const Runtime::Exp& sequence_mask,
				    const alphabet& a,
				    const R::RVector& smap,
				    int n_models);

    // This one is sparse
    object_ptr<const SparseLikelihoods>
    simple_sequence_likelihoods2_SEV(const Runtime::Exp& sequence_mask,
				     const alphabet& a,
				     const R::RVector& smap,
				     int n_models);

    ComponentStateVectors sample_root_sequence_SEV(const R::RVector& LCN,
							const R::RVector& LCB,
							const DenseMatrix<double>& F,
							std::span<const int> compressed_col_for_col);

    ComponentStateVectors sample_sequence_SEV(std::span<const int> parent_components,
						   std::span<const int> parent_states,
						   const R::RVector& LCN,
						   const R::RVector& transition_Ps,
						   const R::RVector& LCB,
						   std::span<const int> compressed_col_for_col);
}
#endif
