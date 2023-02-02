module PopGen.Deploid where

import Probability
import Control.Monad.IO.Class
import Range
import Data.CSV
import Data.Matrix

type Haplotype = EVector Int
type Reads = EVector (EPair Int Int)
type Panel = EVector (EVector Int)
type Sites = EVector Int
type ContextIndex = Int

foreign import bpcall "SMC:sample_haplotype01_from_plaf" builtin_sample_haplotype01_from_plaf :: EVector Double -> EVector Int
sample_haplotype01_from_plaf plafs = let raw_action = builtin_sample_haplotype01_from_plaf $ list_to_vector plafs
                                     in RanAtomic do_nothing (IOAction (\s->(s,raw_action)))

foreign import bpcall "SMC:haplotype01_from_plaf_probability" builtin_haplotype01_from_plaf_probability :: EVector Double -> EVector Int -> LogDouble
haplotype01_from_plaf_probability plaf hap = builtin_haplotype01_from_plaf_probability (list_to_vector plaf) hap


annotated_haplotype01_from_plaf_probability plafs haplotype = do
  in_edge "PLAFs" plafs
  return [haplotype01_from_plaf_probability plafs haplotype]

haplotype01_from_plaf plafs = Distribution
                              "haplotype01_from_plaf"
                              (annotated_haplotype01_from_plaf_probability plafs)
                              (error "no quantile")
                              (sample_haplotype01_from_plaf plafs)
                              undefined

-- This version does not use the builtins above, and also produces a list, not a vector.
--haplotype01_from_plaf plafs = independent [ bernoulli f | f <- plafs ]

foreign import bpcall "SMC:sample_reads01" builtin_sample_reads01 :: EVector Int -> EVector Double -> EVector (EVector Int) -> Double -> Double -> Double -> EVector (EPair Int Int)
sample_reads01 counts weights haplotypes error_rate c outlier_frac =
    IOAction (\s->(s, f $ builtin_sample_reads01 counts' weights' haplotypes' error_rate c outlier_frac))
    where counts'     = list_to_vector counts
          weights'    = list_to_vector weights
          haplotypes' = list_to_vector haplotypes
          f reads = map pair_from_c $ list_from_vector reads

random_structure_reads01 counts weights haplotypes error_rate c outlier_frac =
    RanAtomic do_nothing $ sample_reads01 counts weights haplotypes error_rate c outlier_frac

foreign import bpcall "SMC:probability_of_reads01" builtin_probability_of_reads01 :: EVector Int -> EVector Double -> EVector (EVector Int) -> Double -> Double -> Double -> Reads -> LogDouble
probability_of_reads01 counts weights haplotypes error_rate c outlier_frac reads = builtin_probability_of_reads01 counts' weights' haplotypes' error_rate c outlier_frac reads'
    where counts'     = list_to_vector counts
          weights'    = list_to_vector weights
          haplotypes' = list_to_vector haplotypes
          reads'      = list_to_vector $ map (\(x,y) -> c_pair x y) reads


data Reads01Properties = Reads01Properties { emission_prs :: EVector Int -> Matrix LogDouble}
foreign import bpcall "SMC:emission_pr_for_reads01" builtin_emission_pr_for_reads01 :: ContextIndex -> EVector Int -> Reads -> [Haplotype] -> EVector Double -> Double -> Double -> Double -> LogDouble
emission_pr_for_reads01 ks reads haplotypes weights error_rate concentration outlier_frac context =
    builtin_emission_pr_for_reads01 context (list_to_vector ks) haplotypes weights error_rate concentration outlier_frac

-- We can't sample from this because we are using the random data to tell us the coverage at each position.
annotated_probability_of_reads01 counts weights haplotypes error_rate c outlier_frac reads = do
                          in_edge "counts" counts
                          in_edge "weights" weights
                          in_edge "haplotypes" haplotypes
                          in_edge "error_rate" error_rate
                          in_edge "c" c
                          in_edge "outlier_frac" outlier_frac
--                          let emit_pr_func ks = emission_prs ks reads haplotypes weights error_rate c outlier_frac
--                          property "properties" (Reads01Properties emit_pr_func)
                          return [probability_of_reads01 counts weights haplotypes error_rate c outlier_frac reads]

reads01_from_haps counts weights haplotypes error_rate c outlier_frac = Distribution
                                                                 "reads01_from_haps"
                                                                 (annotated_probability_of_reads01 counts weights haplotypes error_rate c outlier_frac)
                                                                 (error "no quantile")
                                                                 (random_structure_reads01 counts weights haplotypes error_rate c outlier_frac)
                                                                 NoRange

---
foreign import bpcall "SMC:propose_haplotypes_from_plaf" propose_haplotypes_from_plaf'' :: ContextIndex -> RealWorld -> [Int] -> [Haplotype] -> EVector Double -> EVector Double -> EVector (EPair Int Int) -> Double -> Double -> Double -> EPair RealWorld LogDouble

propose_haplotypes_from_plaf' indices haps freqs w reads e c outlier_frac context io_state =
    propose_haplotypes_from_plaf'' context io_state indices haps freqs' w' reads' e c outlier_frac
           where
             freqs' = list_to_vector freqs
             w'     = list_to_vector w
             reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads

propose_haplotypes_from_plaf indices freqs weights reads haplotypes error_rate c outlier_frac context =
    IOAction $ pair_from_c . propose_haplotypes_from_plaf' indices haplotypes freqs weights reads error_rate c outlier_frac context


---

foreign import bpcall "SMC:propose_weights_and_haplotypes_from_plaf" propose_weights_and_haplotypes_from_plaf'' :: ContextIndex -> RealWorld -> [Int] -> [Double] -> [Haplotype] -> EVector Double -> EVector Double -> Reads -> Double -> Double -> Double -> EPair RealWorld LogDouble

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

  let plaf_sites = [ read (row!!1) :: Int     | row <- plaf_table]
      plaf_freq  = [ read (row!!2) :: Double  | row <- plaf_table]

  return (plaf_sites, plaf_freq)

read_panel filename = do
  panel_table <- tail <$> read_tsv filename
  let sites = [ read  (row!!1) :: Int | row <- panel_table]
      n_haplotypes = length (head panel_table) - 2
      get_ith_hap i = list_to_vector [ read (row !! (i+2)) :: Int | row <- panel_table]
      haplotypes = [ get_ith_hap i | i <- [0..n_haplotypes-1]]
  return (sites, haplotypes)

load_reads filename = do
  reads_table <- tail <$> read_tsv filename
  let sites = [ read (row!!1) :: Int | row <- reads_table]
      ref = [ read (row!!2) :: Int | row <- reads_table]
      alt = [ read (row!!3) :: Int | row <- reads_table]
      reads = zip ref alt
  return (sites, reads)


foreign import bpcall "SMC:sample_haplotype01_from_panel" builtin_sample_haplotype01_from_panel :: Panel -> Sites -> Double -> Double -> RealWorld -> Haplotype
sample_haplotype01_from_panel (p_sites,p_haps) switch_rate flip_prob = let raw_action s = builtin_sample_haplotype01_from_panel p_haps' p_sites' switch_rate flip_prob s
                                                                           p_haps' = list_to_vector p_haps
                                                                           p_sites' = list_to_vector p_sites
                                                                       in RanAtomic do_nothing (IOAction (\s->(s,raw_action s)))

foreign import bpcall "SMC:haplotype01_from_panel_probability" builtin_haplotype01_from_panel_probability :: Panel -> Sites -> Double -> Double -> Haplotype -> LogDouble
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
                                                      NoRange

foreign import bpcall "SMC:resample_haplotypes_from_panel" resample_haplotypes_from_panel'' :: ContextIndex -> RealWorld -> [Int] -> [Haplotype] -> Panel -> Sites -> Double -> Double -> EVector Double -> Reads -> Double -> Double -> Double -> EPair RealWorld ()

resample_haplotypes_from_panel' indices (p_sites, p_haps) switch_rate flip_prob weights reads haplotypes error_rate c outlier_frac context io_state =
    resample_haplotypes_from_panel'' context io_state indices haplotypes p_haps p_sites' switch_rate flip_prob weights' reads' error_rate c outlier_frac
        where
          p_sites' = list_to_vector p_sites
          weights' = list_to_vector weights
          reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads

resample_haplotypes_from_panel indices panel switch_rate flip_prob weights reads haplotypes error_rate c outlier_frac context =
    IOAction $ pair_from_c . resample_haplotypes_from_panel' indices panel switch_rate flip_prob weights reads haplotypes error_rate c outlier_frac context

foreign import bpcall "SMC:resample_weights_and_haplotypes_from_panel" resample_weights_and_haplotypes_from_panel'' :: ContextIndex -> RealWorld -> [Int] -> [Double] -> [Haplotype] -> Panel -> Sites -> Double -> Double -> EVector Double -> Reads -> Double -> Double -> Double -> EPair RealWorld LogDouble

resample_weights_and_haplotypes_from_panel' indices titres haps (p_sites, p_haps) switching_rate miscopy_prob w reads e c outlier_frac context io_state =
    resample_weights_and_haplotypes_from_panel'' context io_state indices titres haps p_haps p_sites switching_rate miscopy_prob w' reads' e c outlier_frac
           where
             w'       = list_to_vector w
             reads'   = list_to_vector $ map (\(x,y) -> c_pair x y) reads

resample_weights_and_haplotypes_from_panel indices titres panel switching_rate miscopy_prob weights reads haplotypes error_rate c outlier_frac context =
    IOAction $ pair_from_c . resample_weights_and_haplotypes_from_panel' indices titres haplotypes panel switching_rate miscopy_prob weights reads error_rate c outlier_frac context
