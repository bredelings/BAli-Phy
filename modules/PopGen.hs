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

robust_diploid_afs n_individuals n_loci s f theta_effective = Prefix "DiploidAFS" $ do 
  { 
--     extra_pop_structure -- 
--     This has mixing issues in the case where s ~ 0.95, and we estimate f as like uniform [0,0.75], with a bump at 0.6.
--      * ACT ~ 45
--      * It seems that f and s are not very correlated when f is in [0, 0.4], but starts making a different when is in [0.4,0.7].
--      * Perhaps we should co-propose f and s, since both affect the coalescence probability.
--      * However, perhaps the I_{lk} has a bigger effect, since f affects how individual-specific Pr(I_{lk}) is.
--
--     This has mixing issues in the case where s ~ 0.33, and we estimate f ~ 0.045 (0.022,0.067).
--      * ACT ~ \infty ?
--      * We take 500-1000 generations to accept extra_pop_structure=1. (Why? This might be the real problem.)
--      * We never propose moving back to extra_pop_structure=0.
--        + Well, of course!  If f=0 and T_k=0, then coalescence is absolutely not allowed.
--        + If we set f=0, then we need to set I_lk=0 for every individual with no selfing.
--        + I guess we would like to (a) choose a random order for the individuals.
--                                   (b) resample I_lk for individuals k = 1, 2, 3, ... , n.
--      * It seems that f and s are not very correlated.
--     
--       extra_pop_structure <- bernoulli 0.5;
--       Log "extra-pop-structure" extra_pop_structure;
--       f <- beta 1.0 5.0;
--       let {f = if (extra_pop_structure == 1) then f' else 0.0};
--       Log "f'" f';
--       Log "f" f;

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
