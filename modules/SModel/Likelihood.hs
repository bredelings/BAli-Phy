module SModel.Likelihood  where 

import Tree

-- peeling for connected-CLVs
builtin peel_leaf_branch 4 "peel_leaf_branch" "SModel"
builtin alignment_index2 2 "alignment_index2" "SModel"
builtin alignment_index3 3 "alignment_index3" "SModel"
builtin peel_internal_branch 6 "peel_internal_branch" "SModel"
builtin calc_root_probability 7 "calc_root_probability" "SModel"

-- peeling for SEV
builtin bitmask_from_alignment 2 "bitmask_from_alignment" "Alignment"
builtin peel_leaf_branch_SEV 4 "peel_leaf_branch_SEV" "SModel"
builtin peel_internal_branch_SEV 4 "peel_internal_branch_SEV" "SModel"
builtin calc_root_probability_SEV 5 "calc_root_probability_SEV" "SModel"

builtin peel_likelihood_1 3 "peel_likelihood_1" "SModel"
builtin peel_likelihood_2 6 "peel_likelihood_2" "SModel"

cached_conditional_likelihoods t seqs counts as alpha ps f = let lc    = mkArray (2*numBranches t) lcf
                                                                 lcf b = let bb = b `mod` (numBranches t)
                                                                         in case edgesBeforeEdge t b of 
                                                                              []      -> let n=sourceNode t b
                                                                                         in peel_leaf_branch (seqs!n) (counts!n) alpha (ps!bb)
                                                                              [b1,b2] -> peel_internal_branch (lc!b1) (lc!b2) (as!b1) (as!b2) (ps!bb) f
                                                             in lc

peel_likelihood t cl as f root = let likelihoods = mkArray (numNodes t) peel_likelihood'
                                     peel_likelihood' root = let branches_in = map (reverseEdge t) (edgesOutOfNode t root)in
                                                              case branches_in of [b1,b2,b3]-> calc_root_probability (cl!b1) (cl!b2) (cl!b3) (as!b1) (as!b2) (as!b3) f
                                 in likelihoods!root


cached_conditional_likelihoods_SEV t seqs alpha ps f a =
    let lc    = mkArray (2*numBranches t) lcf
        lcf b = let bb = b `mod` (numBranches t) in
                case edgesBeforeEdge t b of []      -> peel_leaf_branch_SEV (seqs!sourceNode t b) alpha (ps!bb) (bitmask_from_alignment a $ sourceNode t b)
                                            [b1,b2] -> peel_internal_branch_SEV (lc!b1) (lc!b2) (ps!bb) f
    in lc

peel_likelihood_SEV t cl f root counts = let branches_in = map (reverseEdge t) (edgesOutOfNode t root) in
                                         case branches_in of [b1,b2,b3]-> calc_root_probability_SEV (cl!b1) (cl!b2) (cl!b3) f counts

