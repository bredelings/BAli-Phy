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

foreign import bpcall "SMC:sample_haplotype01_from_plaf" builtin_sample_haplotype01_from_plaf :: EVector Double -> IO Haplotype
foreign import bpcall "SMC:haplotype01_from_plaf_probability" builtin_haplotype01_from_plaf_probability :: EVector Double -> Haplotype -> LogDouble
haplotype01_from_plaf_probability plaf hap = builtin_haplotype01_from_plaf_probability (list_to_vector plaf) hap

data Haplotype01FromPLAF = Haplotype01FromPLAF [Double]

instance Dist Haplotype01FromPLAF where
    type Result (Haplotype01FromPLAF) = Haplotype
    dist_name _ = "haplotype01_from_plaf"

instance IOSampleable Haplotype01FromPLAF where
    sampleIO (Haplotype01FromPLAF plafs) = builtin_sample_haplotype01_from_plaf (list_to_vector plafs)

instance HasAnnotatedPdf Haplotype01FromPLAF where
    annotated_densities dist@(Haplotype01FromPLAF plafs) haplotype = do
       in_edge "PLAFs" plafs
       return [haplotype01_from_plaf_probability plafs haplotype]

instance Sampleable Haplotype01FromPLAF where
    sample dist@(Haplotype01FromPLAF plafs) = RanDistribution2 dist do_nothing

haplotype01FromPLAFDist plafs = Haplotype01FromPLAF plafs

haplotype01_from_plaf plafs = sample $ haplotype01FromPLAFDist plafs

-----------------------------------------------

data Reads01FromHaps = Reads01FromHaps
                         [Int]          -- counts
                         [Double]       -- weights
                         [Haplotype]    -- haplotypes
                         Double         -- error_rate
                         Double         -- c
                         Double         -- outlier_frac

instance Dist Reads01FromHaps where
    type Result Reads01FromHaps = [(Int,Int)]
    dist_name _ = "reads01_from_haps"

instance IOSampleable Reads01FromHaps where
    sampleIO (Reads01FromHaps counts weights haplotypes error_rate c outlier_frac) = sample_reads01 counts weights haplotypes error_rate c outlier_frac

instance HasAnnotatedPdf Reads01FromHaps where
    annotated_densities dist@(Reads01FromHaps counts weights haplotypes error_rate c outlier_frac) reads = do
                          in_edge "counts" counts
                          in_edge "weights" weights
                          in_edge "haplotypes" haplotypes
                          in_edge "error_rate" error_rate
                          in_edge "c" c
                          in_edge "outlier_frac" outlier_frac
--                          let emit_pr_func ks = emission_prs ks reads haplotypes weights error_rate c outlier_frac
--                          property "properties" (Reads01Properties emit_pr_func)
                          error "Does this work?"
                          return [probability_of_reads01 counts weights haplotypes error_rate c outlier_frac reads]

instance Sampleable Reads01FromHaps where
    sample dist@(Reads01FromHaps counts weights haplotypes error_rate c outlier_frac) = RanDistribution2 dist do_nothing


-- This version does not use the builtins above, and also produces a list, not a vector.
--haplotype01_from_plaf plafs = independent [ bernoulli f | f <- plafs ]

foreign import bpcall "SMC:sample_reads01" builtin_sample_reads01 :: EVector Int -> EVector Double -> EVector Haplotype -> Double -> Double -> Double -> IO Reads
sample_reads01 counts weights haplotypes error_rate c outlier_frac =
    fmap f $ builtin_sample_reads01 counts' weights' haplotypes' error_rate c outlier_frac
    where counts'     = list_to_vector counts
          weights'    = list_to_vector weights
          haplotypes' = list_to_vector haplotypes
          f reads = map pair_from_c $ list_from_vector reads

foreign import bpcall "SMC:probability_of_reads01" builtin_probability_of_reads01 :: EVector Int -> EVector Double -> EVector Haplotype -> Double -> Double -> Double -> Reads -> LogDouble
probability_of_reads01 counts weights haplotypes error_rate c outlier_frac reads = builtin_probability_of_reads01 counts' weights' haplotypes' error_rate c outlier_frac reads'
    where counts'     = list_to_vector counts
          weights'    = list_to_vector weights
          haplotypes' = list_to_vector haplotypes
          reads'      = list_to_vector $ map (\(x,y) -> c_pair x y) reads

data Reads01Properties = Reads01Properties { emission_prs :: EVector Int -> Matrix LogDouble}
foreign import bpcall "SMC:emission_pr_for_reads01" builtin_emission_pr_for_reads01 :: ContextIndex -> EVector Int -> Reads -> [Haplotype] -> EVector Double -> Double -> Double -> Double -> LogDouble
emission_pr_for_reads01 ks reads haplotypes weights error_rate concentration outlier_frac context =
    builtin_emission_pr_for_reads01 context (list_to_vector ks) haplotypes weights error_rate concentration outlier_frac


reads01_from_haps_dist counts weights haplotypes error_rate c outlier_frac = Reads01FromHaps counts weights haplotypes error_rate c outlier_frac

reads01_from_haps counts weights haplotypes error_rate c outlier_frac = sample $ reads01_from_haps_dist counts weights haplotypes error_rate c outlier_frac


---
foreign import bpcall "SMC:propose_haplotypes_from_plaf" propose_haplotypes_from_plaf' :: ContextIndex -> [Int] -> [Haplotype] -> EVector Double -> EVector Double -> EVector (EPair Int Int) -> Double -> Double -> Double -> IO LogDouble

propose_haplotypes_from_plaf indices haps freqs w reads e c outlier_frac context =
    propose_haplotypes_from_plaf' context indices haps freqs' w' reads' e c outlier_frac 
           where
             freqs' = list_to_vector freqs
             w'     = list_to_vector w
             reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads
---

foreign import bpcall "SMC:propose_weights_and_haplotypes_from_plaf" propose_weights_and_haplotypes_from_plaf' :: ContextIndex -> [Int] -> [Double] -> [Haplotype] -> EVector Double -> EVector Double -> Reads -> Double -> Double -> Double -> IO LogDouble

propose_weights_and_haplotypes_from_plaf indices titres haps freqs w reads e c outlier_frac context =
    propose_weights_and_haplotypes_from_plaf' context indices titres haps freqs' w' reads' e c outlier_frac
           where
             freqs' = list_to_vector freqs
             w'     = list_to_vector w
             reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads

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


data Haplotype01FromPanel = Haplotype01FromPanel Panel (EVector Int) Double Double

instance Dist Haplotype01FromPanel where
    type Result Haplotype01FromPanel = Haplotype
    dist_name _ = "haplotype01_from_panel"

foreign import bpcall "SMC:sample_haplotype01_from_panel" builtin_sample_haplotype01_from_panel :: EVector Haplotype -> Sites -> Double -> Double -> IO Haplotype

instance IOSampleable Haplotype01FromPanel where
    sampleIO (Haplotype01FromPanel p_haps p_sites switch_rate flip_prob) = builtin_sample_haplotype01_from_panel p_haps p_sites switch_rate flip_prob

instance HasAnnotatedPdf Haplotype01FromPanel where
    annotated_densities dist@(Haplotype01FromPanel haps sites switch_rate miscopy_prob) haplotype = do
        in_edge "haplotypes" haps
        in_edge "sites" sites
        in_edge "switch_rate" switch_rate
        in_edge "miscopy_prob" miscopy_prob
        return [haplotype01_from_panel_probability haps sites switch_rate miscopy_prob haplotype]

instance Sampleable Haplotype01FromPanel where
    sample dist@(Haplotype01FromPanel p_haps p_sites switch_rate flip_prob) = RanDistribution2 dist do_nothing

foreign import bpcall "SMC:haplotype01_from_panel_probability" builtin_haplotype01_from_panel_probability :: Panel -> Sites -> Double -> Double -> Haplotype -> LogDouble
haplotype01_from_panel_probability p_haps p_sites switch_rate flip_prob hap = builtin_haplotype01_from_panel_probability p_haps p_sites switch_rate flip_prob hap

haplotype01_from_panel_dist (sites, haps) switch_rate flip_prob = Haplotype01FromPanel (list_to_vector haps) (list_to_vector sites) switch_rate flip_prob
haplotype01_from_panel panel switch_rate flip_prob = sample $ haplotype01_from_panel_dist panel switch_rate flip_prob

foreign import bpcall "SMC:resample_haplotypes_from_panel" resample_haplotypes_from_panel' :: ContextIndex -> [Int] -> [Haplotype] -> Panel -> Sites -> Double -> Double -> EVector Double -> Reads -> Double -> Double -> Double -> IO ()

resample_haplotypes_from_panel indices (p_sites, p_haps) switch_rate flip_prob weights reads haplotypes error_rate c outlier_frac context =
    resample_haplotypes_from_panel' context indices haplotypes p_haps p_sites' switch_rate flip_prob weights' reads' error_rate c outlier_frac
        where
          p_sites' = list_to_vector p_sites
          weights' = list_to_vector weights
          reads' = list_to_vector $ map (\(x,y) -> c_pair x y) reads

foreign import bpcall "SMC:resample_weights_and_haplotypes_from_panel" resample_weights_and_haplotypes_from_panel' :: ContextIndex -> [Int] -> [Double] -> [Haplotype] -> Panel -> Sites -> Double -> Double -> EVector Double -> Reads -> Double -> Double -> Double -> IO LogDouble

resample_weights_and_haplotypes_from_panel indices titres haps (p_sites, p_haps) switching_rate miscopy_prob w reads e c outlier_frac context =
    resample_weights_and_haplotypes_from_panel' context indices titres haps p_haps p_sites switching_rate miscopy_prob w' reads' e c outlier_frac
           where
             w'       = list_to_vector w
             reads'   = list_to_vector $ map (\(x,y) -> c_pair x y) reads
