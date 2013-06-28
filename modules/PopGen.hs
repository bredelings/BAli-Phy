module PopGen where
{
import Distributions;
import Range;

builtin builtin_read_phase_file 1 "read_phase_file" "popgen";
builtin remove_2nd_allele 1 "remove_2nd_allele" "popgen";
builtin allele_frequency_spectrum 1 "allele_frequency_spectrum" "popgen";
builtin ewens_sampling_group_probability 2 "ewens_sampling_group_probability" "popgen";
builtin ewens_sampling_probability 2 "ewens_sampling_probability" "popgen";
builtin builtin_ewens_sampling_mixture_probability 3 "ewens_sampling_mixture_probability" "popgen";
builtin builtin_ewens_diploid_probability 3 "ewens_diploid_probability" "popgen";
builtin builtin_selfing_coalescence_probability 3 "selfing_coalescence_probability" "popgen";
builtin builtin_sum_out_coals 3 "sum_out_coals" "sum_out_coals";


sum_out_coals x y pr = IOAction3 builtin_sum_out_coals x y pr;

read_phase_file = builtin_read_phase_file . listToString;

ewens_sampling_mixture_probability (thetas,ps) x = builtin_ewens_sampling_mixture_probability (listToVectorDouble thetas) (listToVectorDouble ps) x;

ewens_diploid_probability (theta,i) x = builtin_ewens_diploid_probability theta (list_to_vector i) (list_to_vector x);

selfing_coalescence_probability (n_loci,s) i = builtin_selfing_coalescence_probability n_loci s (list_to_vector i);

afs args = ProbDensity (ewens_sampling_probability args) (error "afs has no quantile") () ();

afsGroup args = ProbDensity (ewens_sampling_group_probability args) (error "afs has no quantile") () ();

afsMixture args = ProbDensity (ewens_sampling_mixture_probability args) (error "afs has no quantile") () ();

afs2 args  = ProbDensity (ewens_diploid_probability args) (error "afs has no quantile") () ();

selfing_coalescence (n_loci,s) = ProbDensity (selfing_coalescence_probability (n_loci,s)) (error "selfing_coalescence has no quantile") (replicate n_loci False) (ListRange (replicate n_loci TrueFalseRange));
}