module PopGen where

import Probability
import Range
import Bio.Alignment.Matrix

data VVI -- Vector<Vector<int>>

foreign import bpcall "PopGen:read_phase_file" builtin_read_phase_file :: CPPString -> EVector (EVector Int)
foreign import bpcall "PopGen:read_phase2_file" builtin_read_phase2_file :: CPPString -> EVector (EVector Int)
foreign import bpcall "PopGen:remove_2nd_allele" remove_2nd_allele :: EVector a -> EVector a
foreign import bpcall "PopGen:allele_frequency_spectrum" allele_frequency_spectrum :: EVector Int -> EVector Int
foreign import bpcall "PopGen:ewens_sampling_group_probability" ewens_sampling_group_probability :: Double -> VVI -> LogDouble
foreign import bpcall "PopGen:ewens_sampling_probability" ewens_sampling_probability :: Double -> EVector Int -> LogDouble
foreign import bpcall "PopGen:ewens_sampling_mixture_probability" builtin_ewens_sampling_mixture_probability :: EVector Double -> EVector Double -> VVI -> LogDouble
foreign import bpcall "PopGen:selfing_coalescence_probability" builtin_selfing_coalescence_probability :: Int -> Double -> EVector Int -> LogDouble
foreign import bpcall "SMC:li_stephens_2003_composite_likelihood" li_stephens_2003_composite_likelihood :: EVector Int -> Double -> AlignmentMatrix -> LogDouble

read_phase_file filename = IOAction (\s -> (s, map list_from_vector $ list_from_vector $ builtin_read_phase_file $ list_to_string filename))

read_phase2_file filename = IOAction (\s -> (s, map list_from_vector $ list_from_vector $ builtin_read_phase2_file $ list_to_string filename))

ewens_sampling_mixture_probability thetas ps x = builtin_ewens_sampling_mixture_probability (list_to_vector thetas) (list_to_vector ps) x

selfing_coalescence_probability n_loci s i = builtin_selfing_coalescence_probability n_loci s (list_to_vector i)

afs args = Distribution "afs" (make_densities $ ewens_sampling_probability args) (error "afs has no quantile") undefined undefined

afsGroup args = Distribution "afsGroup" (make_densities $ ewens_sampling_group_probability args) (error "afsGroup has no quantile") undefined undefined

afsMixture thetas ps = Distribution "afsMixture" (make_densities $ ewens_sampling_mixture_probability thetas ps) (error "afsMixture has no quantile") undefined undefined

selfing_coalescence n_loci s = Distribution "selfing_coalescence" (make_densities $ selfing_coalescence_probability n_loci s) (error "selfing_coalescence has no quantile") undefined NoRange

li_stephens_2003 locs rho = Distribution "li_stephens_2003" (make_densities $ li_stephens_2003_composite_likelihood locs' rho) (error "li_stephens_2003 has no quantile") undefined undefined
    where locs' = list_to_vector locs

