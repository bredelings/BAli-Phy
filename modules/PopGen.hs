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
builtin builtin_ewens_diploid_probability 3 "ewens_diploid_probability" "PopGen";
builtin builtin_selfing_coalescence_probability 3 "selfing_coalescence_probability" "PopGen";
builtin builtin_sum_out_coals 3 "sum_out_coals" "MCMC";


sum_out_coals x y c = IOAction3 builtin_sum_out_coals x y c;

read_phase_file filename = map list_from_vector $ list_from_vector $ builtin_read_phase_file $ listToString filename;

ewens_sampling_mixture_probability thetas ps x = builtin_ewens_sampling_mixture_probability (listToVectorDouble thetas) (listToVectorDouble ps) x;

ewens_diploid_probability theta i x = builtin_ewens_diploid_probability theta (list_to_vector i) (list_to_vector x);

selfing_coalescence_probability n_loci s i = builtin_selfing_coalescence_probability n_loci s (list_to_vector i);

afs args = ProbDensity (ewens_sampling_probability args) (error "afs has no quantile") () ();

afsGroup args = ProbDensity (ewens_sampling_group_probability args) (error "afsGroup has no quantile") () ();

afsMixture thetas ps = ProbDensity (ewens_sampling_mixture_probability thetas ps) (error "afsMixture has no quantile") () ();

afs2 thetas ps = ProbDensity (ewens_diploid_probability thetas ps) (error "afs2 has no quantile") () ();

selfing_coalescence n_loci s = ProbDensity (selfing_coalescence_probability n_loci s) (error "selfing_coalescence has no quantile") (replicate n_loci False) (ListRange (replicate n_loci TrueFalseRange));

diploid_afs_modified n_individuals n_loci s theta_effective = Prefix "DiploidAFS" $ do 
  { 
--     This MAY have mixing issues.
--          f' <- beta 1.0 3.0;
--          has_biparental_inbreeding <- bernoulli 0.5;
--          Log "biparental_inbreeding" has_biparental_inbreeding;
--          let {f = if (has_biparental_inbreeding == 1) then f' else 0.0};
--          Log "f'" f';

       f <- beta 1.0 5.0;
       Log "f" f;

       t <- iid n_individuals (rgeometric s);
       Log "t" t;

       i <- plate n_individuals (\k->iid n_loci (rbernoulli (0.5**t!!k*(1.0-f))) );
--       Log "i" i;

       AddMove (\c -> mapM_ (\k-> sum_out_coals (t!!k) (i!!k) c) [0..n_individuals-1]);

       return $ plate n_loci (\l -> afs2 (theta_effective!!l) (map (!!l) i));
  };

diploid_afs n_individuals n_loci s theta_effective = Prefix "DiploidAFS" $ do 
  { 
       t <- iid n_individuals (rgeometric s);
       Log "t" t;

       i <- plate n_individuals (\k->iid n_loci (rbernoulli (0.5**t!!k)) );
--       Log "i" i;

       AddMove (\c -> mapM_ (\k-> sum_out_coals (t!!k) (i!!k) c) [0..n_individuals-1]);

       return $ plate n_loci (\l -> afs2 (theta_effective!!l) (map (!!l) i));
  };
}
