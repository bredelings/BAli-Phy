module PopGen where

import Probability
import Range

foreign import bpcall "PopGen:read_phase_file" builtin_read_phase_file :: () -> ()
foreign import bpcall "PopGen:read_phase2_file" builtin_read_phase2_file :: () -> ()
foreign import bpcall "PopGen:remove_2nd_allele" remove_2nd_allele :: () -> ()
foreign import bpcall "PopGen:allele_frequency_spectrum" allele_frequency_spectrum :: () -> ()
foreign import bpcall "PopGen:ewens_sampling_group_probability" ewens_sampling_group_probability :: () -> () -> ()
foreign import bpcall "PopGen:ewens_sampling_probability" ewens_sampling_probability :: () -> () -> ()
foreign import bpcall "PopGen:ewens_sampling_mixture_probability" builtin_ewens_sampling_mixture_probability :: () -> () -> () -> ()
foreign import bpcall "PopGen:selfing_coalescence_probability" builtin_selfing_coalescence_probability :: () -> () -> () -> ()
foreign import bpcall "SMC:li_stephens_2003_composite_likelihood" li_stephens_2003_composite_likelihood :: () -> () -> () -> ()

read_phase_file filename = map list_from_vector $ list_from_vector $ builtin_read_phase_file $ list_to_string filename

read_phase2_file filename = map list_from_vector $ list_from_vector $ builtin_read_phase2_file $ list_to_string filename

ewens_sampling_mixture_probability thetas ps x = builtin_ewens_sampling_mixture_probability (list_to_vector thetas) (list_to_vector ps) x

selfing_coalescence_probability n_loci s i = builtin_selfing_coalescence_probability n_loci s (list_to_vector i)

afs args = Distribution "afs" (make_densities $ ewens_sampling_probability args) (error "afs has no quantile") () ()

afsGroup args = Distribution "afsGroup" (make_densities $ ewens_sampling_group_probability args) (error "afsGroup has no quantile") () ()

afsMixture thetas ps = Distribution "afsMixture" (make_densities $ ewens_sampling_mixture_probability thetas ps) (error "afsMixture has no quantile") () ()

selfing_coalescence n_loci s = Distribution "selfing_coalescence" (make_densities $ selfing_coalescence_probability n_loci s) (error "selfing_coalescence has no quantile") (replicate n_loci False) (ListRange (replicate n_loci TrueFalseRange))

li_stephens_2003 locs rho = Distribution "li_stephens_2003" (make_densities $ li_stephens_2003_composite_likelihood locs' rho) (error "li_stephens_2003 has no quantile") () ()
    where locs' = list_to_vector locs

