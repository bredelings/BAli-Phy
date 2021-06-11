module PopGen.Deploid where

import Probability
import Range
import Data.CSV

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

builtin builtin_probability_of_reads01 6 "probability_of_reads01" "SMC"
probability_of_reads01 weights haplotypes error_rate c outlier_frac reads = builtin_probability_of_reads01 weights' haplotypes' reads' error_rate c outlier_frac
    where weights'    = list_to_vector weights
          haplotypes' = list_to_vector haplotypes
          reads'      = list_to_vector $ map (\(x,y) -> c_pair x y) reads

-- We can't sample from this because we are using the random data to tell us the coverage at each position.
reads01_from_haps weights haplotypes error_rate c outlier_frac = Distribution
                                                                 "reads01_from_haps"
                                                                 (make_densities $ probability_of_reads01 weights haplotypes error_rate c outlier_frac)
                                                                 (error "no quantile")
                                                                 ()
                                                                 ()

---
builtin propose_haplotypes_from_plaf'' 10 "propose_haplotypes_from_plaf" "SMC"

propose_haplotypes_from_plaf' indices haps freqs w reads e c outlier_frac context io_state =
    propose_haplotypes_from_plaf'' context io_state indices haps freqs' w' reads' e c outlier_frac
           where
             freqs' = list_to_vector freqs
             w'     = list_to_vector w
             reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads

propose_haplotypes_from_plaf indices freqs weights reads haplotypes error_rate c outlier_frac context =
    IOAction $ pair_from_c . propose_haplotypes_from_plaf' indices haplotypes freqs weights reads error_rate c outlier_frac context


---

builtin propose_weights_and_haplotypes_from_plaf'' 11 "propose_weights_and_haplotypes_from_plaf" "SMC"

propose_weights_and_haplotypes_from_plaf' indices titres haps freqs w reads e c outlier_frac context io_state =
    propose_weights_and_haplotypes_from_plaf'' context io_state indices titres haps freqs' w' reads' e c outlier_frac
           where
             freqs' = list_to_vector freqs
             w'     = list_to_vector w
             reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads

propose_weights_and_haplotypes_from_plaf indices titres freqs weights reads haplotypes error_rate c outlier_frac context =
    IOAction $ pair_from_c . propose_weights_and_haplotypes_from_plaf' indices titres haplotypes freqs weights reads error_rate c outlier_frac context

-- Currently, these ignore the "chromosome" column.
read_plaf filename = do
  plaf_table <- tail <$> read_tsv filename

  let plaf_sites = [ read_int $ row!!1 | row <- plaf_table]
      plaf_freq  = [ read_double $ row!!2 | row <- plaf_table]

  return (plaf_sites, plaf_freq)

read_panel filename = do
  panel_table <- tail <$> read_tsv filename
  let sites = [ read_int $ row!!1 | row <- panel_table]
      n_haplotypes = length (head panel_table) - 2
      get_ith_hap i = list_to_vector [ read_int $ row !! (i+2) | row <- panel_table]
      haplotypes = [ get_ith_hap i | i <- [0..n_haplotypes-1]]
  return (sites, haplotypes)

load_reads filename = do
  reads_table <- tail <$> read_tsv filename
  let sites = [ read_int $ row!!1 | row <- reads_table]
      ref = [ read_int $ row!!2 | row <- reads_table]
      alt = [ read_int $ row!!3 | row <- reads_table]
      reads = zip ref alt
  return (sites, reads)


builtin builtin_sample_haplotype01_from_panel 5 "sample_haplotype01_from_panel" "SMC"
sample_haplotype01_from_panel (p_sites,p_haps) switch_rate flip_prob = let raw_action s = builtin_sample_haplotype01_from_panel p_haps' p_sites' switch_rate flip_prob s
                                                                           p_haps' = list_to_vector p_haps
                                                                           p_sites' = list_to_vector p_sites
                                                                       in RandomStructure do_nothing modifiable_structure $ liftIO $ IOAction (\s->(s,raw_action s))

builtin builtin_haplotype01_from_panel_probability 5 "haplotype01_from_panel_probability" "SMC"
haplotype01_from_panel_probability (p_sites,p_haps) switch_rate flip_prob hap = builtin_haplotype01_from_panel_probability p_haps' p_sites' switch_rate flip_prob hap
    where p_haps' = list_to_vector p_haps
          p_sites' = list_to_vector p_sites

haplotype01_from_panel panel switch_rate flip_prob  = Distribution
                                                      "haplotype01_from_panel"
                                                      (make_densities $ haplotype01_from_panel_probability panel switch_rate flip_prob)
                                                      (error "no quantile")
                                                      (sample_haplotype01_from_panel panel switch_rate flip_prob)
                                                      ()
