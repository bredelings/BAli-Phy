module SModel (module SModel,
               module SModel.Nucleotides,
               module SModel.Doublets,
               module SModel.Codons,
               module SModel.ReversibleMarkov,
               module SModel.Likelihood,
               module SModel.Simple,
               module SModel.Rate,
               module SModel.MixtureModel,
               module SModel.MixtureModels,
               module SModel.Empirical,
               frequencies_from_dict) where

import Probability
import Bio.Alphabet
import Bio.Sequence
import Tree
import Parameters

import SModel.Nucleotides
import SModel.Doublets
import SModel.Codons
import SModel.ReversibleMarkov
import SModel.Likelihood
import SModel.Simple
import SModel.Rate
import SModel.MixtureModel
import SModel.MixtureModels
import SModel.Empirical

import Data.Matrix

foreign import bpcall "SModel:" mut_sel_q :: Matrix Double -> EVector Double -> Matrix Double
foreign import bpcall "SModel:" mut_sel_pi :: EVector Double -> EVector Double -> EVector Double
foreign import bpcall "SModel:modulated_markov_rates" builtin_modulated_markov_rates :: EVector (Matrix Double) -> Matrix Double -> Matrix Double
foreign import bpcall "SModel:modulated_markov_pi" builtin_modulated_markov_pi :: EVector (EVector Double) -> EVector Double -> EVector Double
foreign import bpcall "SModel:modulated_markov_smap" builtin_modulated_markov_smap :: EVector (EVector Int) -> EVector Int

data F81 = F81 Alphabet (EVector Int) () (EVector Double)

-- We need to combine branch lengths and rate matrices to get transition probability matrices.
-- We need to combine mixtures of rate matrices.
-- We need to combine mixtures of transition probability matrices.
-- Should we combine mixture only at one of the levels?
-- Should we select branch-specific models at the level of rate matrices, or the level of transition probability matrices, or both?

--
m1a_omega_dist f1 w1 = [(f1,w1), (1.0-f1,1.0)]

m2a_omega_dist f1 w1 posP posW = extendDiscreteDistribution (m1a_omega_dist f1 w1) posP posW

m2a_test_omega_dist f1 w1 posP posW 0 = m2a_omega_dist f1 w1 posP 1.0
m2a_test_omega_dist f1 w1 posP posW _ = m2a_omega_dist f1 w1 posP posW

m3_omega_dist ps omegas = zip' ps omegas

m3p_omega_dist ps omegas posP posW = extendDiscreteDistribution (m3_omega_dist ps omegas) posP posW

m3_test_omega_dist ps omegas posP posW 0 = m3p_omega_dist ps omegas posP 1.0
m3_test_omega_dist ps omegas posP posW _ = m3p_omega_dist ps omegas posP posW

-- The M7 is just a beta distribution
-- gamma' = var(x)/(mu*(1-mu)) = 1/(a+b+1) = 1/(n+1)
m7_omega_dist mu gamma n_bins = uniformDiscretize (beta a b) n_bins where cap = min (mu/(1.0+mu)) ((1.0-mu)/(2.0-mu))
                                                                          gamma' = gamma*cap
                                                                          n = (1.0/gamma')-1.0
                                                                          a = n*mu
                                                                          b = n*(1.0 - mu)

-- The M8 is a beta distribution, where a fraction posP of sites have omega posW
m8_omega_dist mu gamma n_bins posP posW = extendDiscreteDistribution (m7_omega_dist mu gamma n_bins) posP posW

m8a_omega_dist mu gamma n_bins posP = m8_omega_dist mu gamma n_bins posP 1.0

m8a_test_omega_dist mu gamma n_bins posP posW 0 = m8_omega_dist mu gamma n_bins posP 1.0
m8a_test_omega_dist mu gamma n_bins posP posW _ = m8_omega_dist mu gamma n_bins posP posW

--  w1 <- uniform 0.0 1.0
--  [f1, f2] <- symmetric_dirichlet 2 1.0
m1a w1 f1 model_func = parameter_mixture_unit (m1a_omega_dist f1 w1) model_func

m2a w1 f1 posP posW model_func = parameter_mixture_unit (m2a_omega_dist f1 w1 posP posW) model_func

m2a_test w1 f1 posP posW posSelection model_func = parameter_mixture_unit (m2a_test_omega_dist f1 w1 posP posW posSelection) model_func

m3 ps omegas model_func = parameter_mixture_unit (m3_omega_dist ps omegas) model_func

m3_test ps omegas posP posW posSelection model_func = parameter_mixture_unit (m3_test_omega_dist ps omegas posP posW posSelection) model_func

m7 mu gamma n_bins model_func =  parameter_mixture_unit (m7_omega_dist mu gamma n_bins) model_func

m8 mu gamma n_bins posP posW model_func = parameter_mixture_unit (m8_omega_dist mu gamma n_bins posP posW) model_func

m8a mu gamma n_bins posP model_func = parameter_mixture_unit  (m8a_omega_dist mu gamma n_bins posP) model_func

m8a_test mu gamma n_bins posP posW posSelection model_func = parameter_mixture_unit (m8a_test_omega_dist mu gamma n_bins posP posW posSelection) model_func

-- OK, so if I change this from [Mixture Omega] to Mixture [Omega] or Mixture (\Int -> Omega), how do I apply the function model_func to all the omegas?
branch_site fs ws posP posW branch_cats model_func = MixtureModels branch_cats [bg_mixture,fg_mixture]
-- background omega distribution -- where the last omega is 1.0 (neutral)
    where bg_dist = zip fs (ws ++ [1.0])
-- accelerated omega distribution -- posW for all categories
          accel_dist = zip fs (repeat posW)
-- background branches always use the background omega distribution              
          bg_mixture = parameter_mixture_unit (mix [1.0-posP, posP] [bg_dist, bg_dist]) model_func
-- foreground branches use the foreground omega distribution with probability posP
          fg_mixture = parameter_mixture_unit (mix [1.0-posP, posP] [bg_dist, accel_dist]) model_func

branch_site_test fs ws posP posW posSelection branch_cats model_func = branch_site fs ws posP posW' branch_cats model_func
    where posW' = if (posSelection == 1) then posW else 1.0

mut_sel w' (ReversibleMarkov a smap q0 pi0 _ _ _) = reversible_markov a smap q pi where
    w = list_to_vector w'
    q = mut_sel_q q0 w
    pi = mut_sel_pi pi0 w

mut_sel' w' q0 = mut_sel w q0 where
    w = get_ordered_elements (letters a) w' "fitnesses"
    a = getAlphabet q0

mut_sel_aa ws q@(ReversibleMarkov codon_a _ _ _ _ _ _) = mut_sel (aa_to_codon codon_a ws) q

mut_sel_aa' ws' q0 = mut_sel_aa ws q0 where
    ws = get_ordered_elements (letters amino_alphabet) ws' "fitnesses"
    codon_alphabet = getAlphabet q0
    amino_alphabet = getAminoAcids codon_alphabet

fMutSel codon_a codon_w omega nuc_model = nuc_model & x3 codon_a & dNdS omega & mut_sel codon_w

fMutSel' codon_a codon_ws' omega nuc_model = fMutSel codon_a codon_ws omega nuc_model
    where codon_ws = get_ordered_elements (letters codon_a) codon_ws' "fitnesses"

aa_to_codon codon_a xs = [xs_array!aa | codon <- codons, let aa = translate codon_a codon]
    where xs_array = listArray' xs
          codons = take n_letters [0..]
          n_letters = alphabetSize codon_a

-- \#1->let {w' = listAray' #1} in \#2 #3->fMutSel #0 codon_w #2 #3
-- The whole body of the function is let-floated up in round 2, and w' is eliminated.
fMutSel0 codon_a aa_w omega nuc_q  = fMutSel codon_a codon_w omega nuc_q
    where codon_w = aa_to_codon codon_a aa_w

fMutSel0' codon_a amino_ws' omega nuc_model = fMutSel0 codon_a amino_ws omega nuc_model
                                               where amino_ws = get_ordered_elements (letters amino_a) amino_ws' "fitnesses"
                                                     amino_a = getAminoAcids codon_a

-- Issue: bad mixing on fMutSel model


modulated_markov_rates qs rates_between = builtin_modulated_markov_rates (list_to_vector qs) rates_between

modulated_markov_pi pis level_probs = builtin_modulated_markov_pi (list_to_vector pis) (list_to_vector level_probs)

modulated_markov_smap smaps = builtin_modulated_markov_smap (list_to_vector smaps)

-- This could get renamed, after I look at the paper that uses the term "modulated markov"
modulated_markov models rates_between level_probs = reversible_markov a smap q pi where
    a = getAlphabet $ head models
    qs = map get_q models
    pis = map get_pi models
    smaps = map get_smap models
    q = modulated_markov_rates qs rates_between
    pi = modulated_markov_pi pis level_probs
    smap = modulated_markov_smap smaps

markov_modulate_mixture nu (MixtureModel dist) = modulated_markov models rates_between level_probs where
    (level_probs,models) = unzip dist
    rates_between = generic_equ (length models) nu

-- We need to rescale submodels to have substitution rate `1.0`.
-- Otherwise class-switching rates are not relative to the substitution rate.

tuffley_steel_98_unscaled s01 s10 q = modulated_markov [scale 0.0 q, q] rates_between level_probs where
    level_probs = [s10/total, s01/total] where total = s10 + s01
    rates_between = fromLists [[-s01,s01],[s10,-s10]]

tuffley_steel_98 s01 s10 q = tuffley_steel_98_unscaled s01 s10 (rescale 1.0 q)

huelsenbeck_02 s01 s10 model = MixtureModel [(p, tuffley_steel_98_unscaled s01 s10 q) | (p,q) <- dist] where
    MixtureModel dist = rescale 1.0 model

galtier_01_ssrv nu model = modulated_markov models rates_between level_probs where
    MixtureModel dist = rescale 1.0 model
    level_probs = map fst dist
    models = map snd dist
    n_levels = length dist
    -- This is really a generic gtr...  We should be able to get this with f81
    rates_between = (generic_equ n_levels nu) %*% (plus_f_matrix $ list_to_vector level_probs)

galtier_01 nu pi model = parameter_mixture_unit [(1.0-pi, 0.0), (pi, nu)] (\nu' -> galtier_01_ssrv nu' model)

wssr07_ssrv s01 s10 nu model = tuffley_steel_98 s01 s10 $ galtier_01_ssrv nu model

wssr07 s01 s10 nu pi model = parameter_mixture_unit [(1.0-pi, 0.0), (pi, nu)] (\nu' -> wssr07_ssrv s01 s10 nu' model)

gamma_rates_dist alpha = gamma alpha (1.0/alpha)

gamma_rates alpha n base = rate_mixture_unif_bins base (gamma_rates_dist alpha) n

log_normal_rates_dist sigmaOverMu = log_normal lmu lsigma where x = log(1.0+sigmaOverMu^2)
                                                                lmu = -0.5*x
                                                                lsigma = sqrt x

log_normal_rates sigmaOverMu n base = rate_mixture_unif_bins base (log_normal_rates_dist sigmaOverMu) n

--dp base rates fraction = rate_mixture base dist where dist = zip fraction rates
free_rates rates fractions base = scaled_mixture (replicate (length fractions) base) rates fractions

transition_p_index smodel_on_tree = mkArray n_branches (list_to_vector . branch_transition_p smodel_on_tree) where tree = get_tree' smodel_on_tree
                                                                                                                   n_branches = numBranches tree

-- * OK... so a mixture of rate matrices is NOT the same as a mixture of exponentiated matrices, because the rate matrices are scaled relative to each other.
--   ** Hmm... THAT might explain why the mixtures aren't working well!  We need to scale each of THOSE components separately.

-- * In theory, we should allow each mixture component to have a different number of states.  This would require
--   that we either split the condition likelihoods into per-component objects, or reserve sum(i,smap(i)) spots per cell.
--   Probably the latter one would be fine.

-- * The model from Sergei Kosakovsky-Pond is a SModelOnTreeMixture, since it is a mixture at the matrix level.
-- * The MBR models are also SModelOnTree Mixtures, since they are also mixtures at the matrix level.
--   + We should be able to get them by combining SingleBranchLengthModels.

-- OK... so a mixture of rate matrices is NOT the same as a mixture of exponentiated matrices, because the rate matrices are scale with respect to each other.
-- So, we can have
--   ReversibleMarkov                          -- rate matrix
--   MixtureModel ReversibleMarkov             -- mixture of rate matrices
--   MixtureModels branch_cats MixtureModel    -- per-branch mixture of rate matrices, where component i always has the same frequencies.

-- We can construct mixtures of these things with e.g. gamma rate models.
--   Gamma rate models SHOULD be able to construct unit_mixtures WITHOUT the use of mmm or unit_mixture now.
--   We should also be able to constructing mixtures of mixtures of rate matrices -> mixtures of rate matrices.  This sounds like the join operation.

-- class SModelOnTree a where
--   branch_transition_p       :: (SingleBranchLengthModel a) Int -> EVector (Matrix Double)
--   distribution              :: a -> [Double]
--   weighted_frequency_matrix :: a -> Matrix Double
--   weighted_matrix           :: a -> Matrix Double
--   nBaseModels               :: a -> Int
--   stateLetters              :: a -> EVector
--   getAlphabet               :: a -> b
--   componentFrequencies      :: a -> Int -> EVector

-- How about
--   scale :: a -> a ?
--   qExp  :: a -> Matrix Double?
-- What kind of things can be scaled?  Things built on rate matrices, I guess?

-- If a mixture of mixture can be flattened, Mixture (Mixture) -> Mixture, isn't that like the monadic operation "join"?

-- Instances:
--   * ReversibleMarkovModel
--   * MixtureModel
--   * MixtureModels
--   * SModelOnTree a => SModelOnTreeMixture [(Double,a)]

-- So, the question is, can we avoid distribution on things like mixtures of Rates.

-- So, how are we going to handle rate scaling?  That should be part of the model!
