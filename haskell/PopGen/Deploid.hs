module PopGen.Deploid where

import Probability
import Range

builtin builtin_sample_haplotype01_from_plaf 1 "sample_haplotype01_from_plaf" "SMC"
sample_haplotype01_from_plaf plafs = let raw_action = builtin_sample_haplotype01_from_plaf $ list_to_vector plafs
                                     in RandomStructure do_nothing modifiable_structure $ liftIO $ IOAction (\s->(s,raw_action))

builtin builtin_haplotype01_from_plaf_probability 2 "haplotype01_from_plaf_probability" "SMC"
haplotype01_from_plaf_probability plaf hap = builtin_haplotype01_from_plaf_probability (list_to_vector plaf) hap

haplotype01_from_plaf plafs = Distribution
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

propose_haplotype_from_plaf hap hap_index freqs w reads haps e c context =
    IOAction $ pair_from_c . propose_haplotype_from_plaf' hap hap_index freqs w reads haps e c context
