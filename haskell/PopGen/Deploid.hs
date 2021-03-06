module PopGen.Deploid where

import Probability
import Range
import Data.CSV

builtin builtin_sample_haplotype01_from_plaf 1 "sample_haplotype01_from_plaf" "SMC"
sample_haplotype01_from_plaf plafs = let raw_action = builtin_sample_haplotype01_from_plaf $ list_to_vector plafs
                                     in RandomStructure do_nothing modifiable_structure $ liftIO $ IOAction (\s->(s,raw_action))

builtin builtin_haplotype01_from_plaf_probability 2 "haplotype01_from_plaf_probability" "SMC"
haplotype01_from_plaf_probability plaf hap = builtin_haplotype01_from_plaf_probability (list_to_vector plaf) hap


annotated_haplotype01_from_plaf_probability plafs haplotype = do
  in_edge "PLAFs" plafs
  return [haplotype01_from_plaf_probability plafs haplotype]

haplotype01_from_plaf plafs = Distribution
                              "haplotype01_from_plaf"
                              (annotated_haplotype01_from_plaf_probability plafs)
                              (error "no quantile")
                              (sample_haplotype01_from_plaf plafs)
                              ()

-- This version does not use the builtins above, and also produces a list, not a vector.
--haplotype01_from_plaf plafs = independent [ bernoulli f | f <- plafs ]

builtin builtin_sample_reads01 6 "sample_reads01" "SMC"
sample_reads01 counts weights haplotypes error_rate c outlier_frac =
    IOAction (\s->(s, f $ builtin_sample_reads01 counts' weights' haplotypes' error_rate c outlier_frac))
    where counts'     = list_to_vector counts
          weights'    = list_to_vector weights
          haplotypes' = list_to_vector haplotypes
          f reads = map pair_from_c $ list_from_vector reads

random_structure_reads01 counts weights haplotypes error_rate c outlier_frac =
    RandomStructure do_nothing modifiable_structure $ liftIO $ sample_reads01 counts weights haplotypes error_rate c outlier_frac

builtin builtin_probability_of_reads01 7 "probability_of_reads01" "SMC"
probability_of_reads01 counts weights haplotypes error_rate c outlier_frac reads = builtin_probability_of_reads01 counts' weights' haplotypes' error_rate c outlier_frac reads'
    where counts'     = list_to_vector counts
          weights'    = list_to_vector weights
          haplotypes' = list_to_vector haplotypes
          reads'      = list_to_vector $ map (\(x,y) -> c_pair x y) reads

-- We can't sample from this because we are using the random data to tell us the coverage at each position.
annotated_probability_of_reads01 counts weights haplotypes error_rate c outlier_frac reads = do
                          in_edge "counts" counts
                          in_edge "weights" weights
                          in_edge "haplotypes" haplotypes
                          in_edge "error_rate" error_rate
                          in_edge "c" c
                          in_edge "outlier_frac" outlier_frac
                          return [probability_of_reads01 counts weights haplotypes error_rate c outlier_frac reads]

reads01_from_haps counts weights haplotypes error_rate c outlier_frac = Distribution
                                                                 "reads01_from_haps"
                                                                 (annotated_probability_of_reads01 counts weights haplotypes error_rate c outlier_frac)
                                                                 (error "no quantile")
                                                                 (random_structure_reads01 counts weights haplotypes error_rate c outlier_frac)
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

annotated_haplotype01_from_panel_probability panel switch_rate miscopy_prob haplotype = do
  in_edge "panel" panel
  in_edge "switch_rate" switch_rate
  in_edge "miscopy_prob" miscopy_prob
  return [haplotype01_from_panel_probability panel switch_rate miscopy_prob haplotype]

haplotype01_from_panel panel switch_rate flip_prob  = Distribution
                                                      "haplotype01_from_panel"
                                                      (annotated_haplotype01_from_panel_probability panel switch_rate flip_prob)
                                                      (error "no quantile")
                                                      (sample_haplotype01_from_panel panel switch_rate flip_prob)
                                                      ()

builtin resample_haplotypes_from_panel'' 13 "resample_haplotypes_from_panel" "SMC"

resample_haplotypes_from_panel' indices (p_sites, p_haps) switch_rate flip_prob weights reads haplotypes error_rate c outlier_frac context io_state =
    resample_haplotypes_from_panel'' context io_state indices haplotypes p_haps p_sites' switch_rate flip_prob weights' reads' error_rate c outlier_frac
        where
          p_sites' = list_to_vector p_sites
          weights' = list_to_vector weights
          reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads

resample_haplotypes_from_panel indices panel switch_rate flip_prob weights reads haplotypes error_rate c outlier_frac context =
    IOAction $ pair_from_c . resample_haplotypes_from_panel' indices panel switch_rate flip_prob weights reads haplotypes error_rate c outlier_frac context

builtin resample_weights_and_haplotypes_from_panel'' 14 "resample_weights_and_haplotypes_from_panel" "SMC"

resample_weights_and_haplotypes_from_panel' indices titres haps (p_sites, p_haps) switching_rate miscopy_prob w reads e c outlier_frac context io_state =
    resample_weights_and_haplotypes_from_panel'' context io_state indices titres haps p_haps p_sites switching_rate miscopy_prob w' reads' e c outlier_frac
           where
             w'       = list_to_vector w
             reads'   = list_to_vector $ map (\(x,y) -> c_pair x y) reads

resample_weights_and_haplotypes_from_panel indices titres panel switching_rate miscopy_prob weights reads haplotypes error_rate c outlier_frac context =
    IOAction $ pair_from_c . resample_weights_and_haplotypes_from_panel' indices titres haplotypes panel switching_rate miscopy_prob weights reads error_rate c outlier_frac context
