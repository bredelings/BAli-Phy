module PopGen where
{
import Distributions;
import Range;

builtin builtin_read_phase_file 1 "read_phase_file" "PopGen";
builtin remove_2nd_allele 1 "remove_2nd_allele" "PopGen";
builtin allele_frequency_spectrum 1 "allele_frequency_spectrum" "PopGen";
builtin ewens_sampling_group_probability 2 "ewens_sampling_group_probability" "PopGen";
builtin ewens_sampling_probability 2 "ewens_sampling_probability" "PopGen";
builtin builtin_ewens_sampling_mixture_probability 3 "ewens_sampling_mixture_probability" "PopGen";
builtin builtin_selfing_coalescence_probability 3 "selfing_coalescence_probability" "PopGen";

read_phase_file filename = map list_from_vector $ list_from_vector $ builtin_read_phase_file $ listToString filename;

ewens_sampling_mixture_probability thetas ps x = builtin_ewens_sampling_mixture_probability (listToVectorDouble thetas) (listToVectorDouble ps) x;

selfing_coalescence_probability n_loci s i = builtin_selfing_coalescence_probability n_loci s (list_to_vector i);

afs args = ProbDensity (ewens_sampling_probability args) (error "afs has no quantile") () ();

afsGroup args = ProbDensity (ewens_sampling_group_probability args) (error "afsGroup has no quantile") () ();

afsMixture thetas ps = ProbDensity (ewens_sampling_mixture_probability thetas ps) (error "afsMixture has no quantile") () ();

selfing_coalescence n_loci s = ProbDensity (selfing_coalescence_probability n_loci s) (error "selfing_coalescence has no quantile") (replicate n_loci False) (ListRange (replicate n_loci TrueFalseRange));

}
