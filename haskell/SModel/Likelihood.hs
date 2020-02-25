module SModel.Likelihood  where 

import Tree

-- peeling for connected-CLVs
builtin peel_leaf_branch 5 "peel_leaf_branch" "SModel"
builtin alignment_index2 2 "alignment_index2" "SModel"
builtin alignment_index3 3 "alignment_index3" "SModel"
builtin peel_internal_branch 6 "peel_internal_branch" "SModel"
builtin calc_root_probability 7 "calc_root_probability" "SModel"

-- ancestral sequence sampling for connected-CLVs
builtin sample_root_sequence 7 "sample_root_sequence" "SModel"
builtin sample_internal_sequence 8 "sample_internal_node_sequence" "SModel"
builtin sample_leaf_sequence 7 "sample_leaf_node_sequence" "SModel"

-- peeling for SEV
builtin bitmask_from_alignment 2 "bitmask_from_alignment" "Alignment"
builtin peel_leaf_branch_SEV 4 "peel_leaf_branch_SEV" "SModel"
builtin peel_internal_branch_SEV 4 "peel_internal_branch_SEV" "SModel"
builtin calc_root_probability_SEV 5 "calc_root_probability_SEV" "SModel"

builtin peel_likelihood_1 3 "peel_likelihood_1" "SModel"
builtin peel_likelihood_2 6 "peel_likelihood_2" "SModel"

cached_conditional_likelihoods t seqs counts as alpha ps f smap = let lc    = mkArray (2*numBranches t) lcf
                                                                      lcf b = let bb = b `mod` (numBranches t)
                                                                              in case edgesBeforeEdge t b of 
                                                                                   []      -> let n=sourceNode t b
                                                                                              in peel_leaf_branch (seqs!n) (counts!n) alpha (ps!bb) smap
                                                                                   [b1,b2] -> peel_internal_branch (lc!b1) (lc!b2) (as!b1) (as!b2) (ps!bb) f
                                                                  in lc

peel_likelihood t cl as f root = let likelihoods = mkArray (numNodes t) peel_likelihood'
                                     peel_likelihood' root = let branches_in = edgesTowardNode t root
                                                             in case branches_in of [b1,b2,b3]-> calc_root_probability (cl!b1) (cl!b2) (cl!b3) (as!b1) (as!b2) (as!b3) f
                                 in likelihoods!root


substitution_likelihood t root seqs as alpha ps f smap = let cl = cached_conditional_likelihoods t seqs counts as alpha ps f smap
                                                             counts = mkArray (numElements seqs) (\n -> list_to_vector $ replicate (vector_size $ seqs!n) 1)
                                                         in peel_likelihood t cl as f root

sample_ancestral_sequences t root seqs as alpha ps f cl smap =
    let rt = add_root t root
        ancestor_seqs = mkArray (numNodes t) ancestor_for_node
        ancestor_for_node n = ancestor_for_branch n (parentBranch rt n)
        ancestor_for_branch n Nothing = sample_root_sequence (cl!b0) (cl!b1) (cl!b2) (as!b0) (as!b1) (as!b2) f where [b0,b1,b2] = edgesTowardNode t n
        ancestor_for_branch n (Just to_p) = let p = targetNode t to_p
                                                parent_seq = ancestor_seqs!p
                                                b0 = reverseEdge t to_p
                                                ps_for_b0 = ps!(b0 `mod` (numBranches t))
                                                a0 = as!b0
                                            in case edgesBeforeEdge t to_p of
                                                 [] -> sample_leaf_sequence
                                                          parent_seq
                                                          ps_for_b0
                                                          (seqs!n)
                                                          alpha
                                                          smap
                                                          a0
                                                          f
                                                 [b1,b2] -> sample_internal_sequence
                                                               parent_seq
                                                               ps_for_b0
                                                               (cl!b1)
                                                               (cl!b2)
                                                               a0
                                                               (as!b1)
                                                               (as!b2)
                                                               f
    in ancestor_seqs

cached_conditional_likelihoods_SEV t seqs alpha ps f a smap =
    let lc    = mkArray (2*numBranches t) lcf
        lcf b = let bb = b `mod` (numBranches t) in
                case edgesBeforeEdge t b of []      -> peel_leaf_branch_SEV (seqs!sourceNode t b) alpha (ps!bb) (bitmask_from_alignment a $ sourceNode t b) smap
                                            [b1,b2] -> peel_internal_branch_SEV (lc!b1) (lc!b2) (ps!bb) f
    in lc

peel_likelihood_SEV t cl f root counts = let branches_in = map (reverseEdge t) (edgesOutOfNode t root) in
                                         case branches_in of [b1,b2,b3]-> calc_root_probability_SEV (cl!b1) (cl!b2) (cl!b3) f counts

sample_ancestral_sequences_SEV t root seqs alpha ps f cl smap counts = error "sample_leaf_sequences_SEV not implemented!"
