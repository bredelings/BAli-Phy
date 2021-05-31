module PopGen.Deploid where

import Probability
import Range

builtin builtin_sample_haplotype01_from_plaf 1 "sample_haplotype01_from_plaf" "SMC"
sample_haplotype01_from_plaf plafs = let raw_action = builtin_sample_haplotype01_from_plaf $ list_to_vector plafs
                                     in RandomStructure do_nothing modifiable_structure $ liftIO $ IOAction (\s->(s,raw_action))

builtin builtin_haplotype01_from_plaf_probability 2 "haplotype01_from_plaf_probability" "SMC"
haplotype01_from_plaf_probability plaf hap = builtin_haplotype01_from_plaf_probability (list_to_vector plaf) hap

haplotype01_from_plaf plafs = Distribution
                              "haplotype01_from_plaf"
                              (make_densities $ haplotype01_from_plaf_probability plafs)
                              (error "no quantile")
                              (sample_haplotype01_from_plaf plafs)
                              ()

-- This version does not use the builtins above, and also produces a list, not a vector.
--haplotype01_from_plaf plafs = independent [ bernoulli f | f <- plafs ]

builtin builtin_probability_of_reads01 5 "probability_of_reads01" "SMC"
probability_of_reads01 weights haplotypes error_rate c reads = builtin_probability_of_reads01 weights' haplotypes' reads' error_rate c
    where weights'    = list_to_vector weights
          haplotypes' = list_to_vector haplotypes
          reads'      = list_to_vector $ map (\(x,y) -> c_pair x y) reads

-- We can't sample from this because we are using the random data to tell us the coverage at each position.
reads01_from_haps weights haplotypes error_rate c = Distribution
                                                                 "reads01_from_haps"
                                                    (make_densities $ probability_of_reads01 weights haplotypes error_rate c)
                                                    (error "no quantile")
                                                    ()
                                                    ()

builtin propose_haplotype_from_plaf'' 10 "propose_haplotype_from_plaf" "SMC"

propose_haplotype_from_plaf' hap hap_index freqs w reads haps e c context io_state =
    propose_haplotype_from_plaf'' context io_state hap hap_index freqs' w' reads' haps' e c
           where
             freqs' = list_to_vector freqs
             w'     = list_to_vector w
             reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads
             haps'  = list_to_vector haps

propose_haplotype_from_plaf hap_index freqs w reads haps e c context =
    IOAction $ pair_from_c . propose_haplotype_from_plaf' (haps !! hap_index) hap_index freqs w reads haps e c context

---
builtin propose_two_haplotypes_from_plaf'' 12 "propose_two_haplotypes_from_plaf" "SMC"

propose_two_haplotypes_from_plaf' hap1 hap2 hap_index1 hap_index2 freqs w reads haps e c context io_state =
    propose_two_haplotypes_from_plaf'' context io_state hap1 hap2 hap_index1 hap_index2 freqs' w' reads' haps' e c
           where
             freqs' = list_to_vector freqs
             w'     = list_to_vector w
             reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads
             haps'  = list_to_vector haps

propose_two_haplotypes_from_plaf hap_index1 hap_index2 freqs w reads haps e c context =
    IOAction $ pair_from_c . propose_two_haplotypes_from_plaf' (haps !! hap_index1) (haps !! hap_index2) hap_index1 hap_index2 freqs w reads haps e c context

-- Currently the proposal evaluates freqs, w, reads, haps, inside the provided context.
-- But we are evaluating the proposal unchangeably -- so an IO action that evaluates (say) the number of haplotypes will not work.
-- Maybe the whole proposal needs to run in the specific context, instead of running unchangeably?
-- In that case, what does it mean for the context to change itself during the proposal??

---

builtin propose_weights_and_haplotype_from_plaf'' 11 "propose_weights_and_haplotype_from_plaf" "SMC"

propose_weights_and_haplotype_from_plaf' titre hap hap_index freqs w reads haps e c context io_state =
    propose_weights_and_haplotype_from_plaf'' context io_state titre hap hap_index freqs' w' reads' haps' e c
           where
             freqs' = list_to_vector freqs
             w'     = list_to_vector w
             reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads
             haps'  = list_to_vector haps

propose_weights_and_haplotype_from_plaf titres hap_index freqs w reads haps e c context =
    IOAction $ pair_from_c . propose_weights_and_haplotype_from_plaf' (titres !! hap_index) (haps !! hap_index) hap_index freqs w reads haps e c context

---

builtin propose_weights_and_two_haplotypes_from_plaf'' 14 "propose_weights_and_two_haplotypes_from_plaf" "SMC"

propose_weights_and_two_haplotypes_from_plaf' titre1 titre2 hap1 hap2 hap_index1 hap_index2 freqs w reads haps e c context io_state =
    propose_weights_and_two_haplotypes_from_plaf'' context io_state titre1 titre2 hap1 hap2 hap_index1 hap_index2 freqs' w' reads' haps' e c
           where
             freqs' = list_to_vector freqs
             w'     = list_to_vector w
             reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads
             haps'  = list_to_vector haps

propose_weights_and_two_haplotypes_from_plaf titres hap_index1 hap_index2 freqs w reads haps e c context =
    IOAction $ pair_from_c . propose_weights_and_two_haplotypes_from_plaf' (titres !! hap_index1) (titres !! hap_index2) (haps !! hap_index1) (haps !! hap_index2) hap_index1 hap_index2 freqs w reads haps e c context

---

builtin propose_weights_and_three_haplotypes_from_plaf'' 17 "propose_weights_and_three_haplotypes_from_plaf" "SMC"

propose_weights_and_three_haplotypes_from_plaf' titre1 titre2 titre3 hap1 hap2 hap3 hap_index1 hap_index2 hap_index3 freqs w reads haps e c context io_state =
    propose_weights_and_three_haplotypes_from_plaf'' context io_state titre1 titre2 titre3 hap1 hap2 hap3 hap_index1 hap_index2 hap_index3 freqs' w' reads' haps' e c
           where
             freqs' = list_to_vector freqs
             w'     = list_to_vector w
             reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads
             haps'  = list_to_vector haps

propose_weights_and_three_haplotypes_from_plaf titres hap_index1 hap_index2 hap_index3 freqs w reads haps e c context =
    IOAction $ pair_from_c . propose_weights_and_three_haplotypes_from_plaf'
                 (titres !! hap_index1) (titres !! hap_index2) (titres !! hap_index3)
                 (haps !! hap_index1) (haps !! hap_index2) (haps !! hap_index3)
                 hap_index1 hap_index2 hap_index3
                 freqs w reads haps e c context
