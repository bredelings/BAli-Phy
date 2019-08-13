module SModel (module SModel,
               module SModel.Nucleotides,
               module SModel.Doublets,
               module SModel.Codons,
               module SModel.ReversibleMarkov,
               module SModel.Likelihood) where 
import Probability
import Alphabet
import Tree
import Parameters

import SModel.Nucleotides
import SModel.Doublets
import SModel.Codons
import SModel.ReversibleMarkov
import SModel.Likelihood

builtin builtin_average_frequency 1 "average_frequency" "SModel"
builtin builtin_empirical 2 "empirical" "SModel"
builtin pam 1 "pam" "SModel"
builtin jtt 1 "jtt" "SModel"
builtin wag 1 "wag" "SModel"
builtin builtin_wag_frequencies 1 "wag_frequencies" "SModel"
builtin builtin_lg_frequencies 1 "lg_frequencies" "SModel"
builtin lg 1 "lg" "SModel"
builtin builtin_weighted_frequency_matrix 2 "weighted_frequency_matrix" "SModel"
builtin builtin_frequency_matrix 1 "frequency_matrix" "SModel"
builtin mut_sel_q 2 "mut_sel_q" "SModel"
builtin mut_sel_pi 2 "mut_sel_pi" "SModel"

data F81 = F81 a b c d
data MixtureModel a = MixtureModel [(Double,a)]
-- Currently we are weirdly duplicating the mixture probabilities for each component.
-- Probably the actual data-type is something like [(Double,\Int->a)] or [(Double,[a])] where all the [a] should have the same length.
-- This would be a branch-dependent mixture
data MixtureModels a = MixtureModels [Int] [MixtureModel a]

branch_categories (MixtureModels categories _) = categories

-- We need to combine branch lengths and rate matrices to get transition probability matrices.
-- We need to combine mixtures of rate matrices.
-- We need to combine mixtures of transition probability matrices.
-- Should we combine mixture only at one of the levels?
-- Should we select branch-specific models at the level of rate matrices, or the level of transition probability matrices, or both?


-- Probably we should define `scale` to work on (ReversibleMarkov, MixtureModel, MixtureModels)
-- Probably we should also define some kind of generic mixture thing, instead just mixMM.
scaleMM x (MixtureModel dist            ) = MixtureModel [(p, scale x m) | (p, m) <- dist]

mixMM fs ms = MixtureModel $ mix fs [m | MixtureModel m <- ms]
scale_MMs rs ms = [scaleMM r m | (r,m) <- zip' rs ms]
scaled_mixture ms rs fs = mixMM fs (scale_MMs rs ms)

multiParameter model_fn values = MixtureModel [ (f*p, m) |(p,x) <- values, let dist = case model_fn x of MixtureModel d -> d, (f,m) <- dist]
multiParameter_unit model_fn values = multiParameter (\x -> unit_mixture $ model_fn x) values

multi_rate m d = multiParameter (\x->scaleMM x m) d

average_frequency (MixtureModel ms) = list_from_vector $ builtin_average_frequency $ weighted_frequency_matrix $ MixtureModel ms

extend_mixture (MixtureModel ms) (p,x) = MixtureModel $ mix [p, 1.0-p] [certainly x, ms]

plus_inv mm p_inv = extend_mixture mm (p_inv, scale 0.0 $ f81 pi a)
    where a  = getAlphabet mm
          pi = average_frequency mm

multi_rate_unif_bins base dist n_bins = multi_rate base $ uniformDiscretize dist n_bins

rate (ReversibleMarkov a s q pi l t r) = r
rate (MixtureModel d) = average [(p,rate m) | (p,m) <- d]

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
baseModel (MixtureModel l) i = snd (l !! i)

nStates m = vector_size (stateLetters m)
  
frequencies (ReversibleMarkov _ _ _ pi _ _ _) = pi
frequencies (F81 _ _ _ pi) = pi

unwrapMM (MixtureModel dd) = dd

mixMixtureModels l dd = MixtureModel (mix l (map unwrapMM dd))

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
--  [f1, f2] <- dirichlet' 2 1.0
m1a model_func w1 f1 = multiParameter_unit model_func (m1a_omega_dist f1 w1)

m2a model_func w1 f1 posP posW = multiParameter_unit model_func (m2a_omega_dist f1 w1 posP posW)

m2a_test model_func w1 f1 posP posW posSelection = multiParameter_unit model_func (m2a_test_omega_dist f1 w1 posP posW posSelection)

m3 model_func ps omegas = multiParameter_unit model_func (m3_omega_dist ps omegas)

m3_test model_func ps omegas posP posW posSelection = multiParameter_unit model_func (m3_test_omega_dist ps omegas posP posW posSelection)

m7 model_func mu gamma n_bins =  multiParameter_unit model_func (m7_omega_dist mu gamma n_bins)

m8 model_func mu gamma n_bins posP posW = multiParameter_unit model_func (m8_omega_dist mu gamma n_bins posP posW)

m8a model_func mu gamma n_bins posP = multiParameter_unit model_func (m8a_omega_dist mu gamma n_bins posP)

m8a_test model_func mu gamma n_bins posP posW posSelection = multiParameter_unit model_func (m8a_test_omega_dist mu gamma n_bins posP posW posSelection)

-- OK, so if I change this from [Mixture Omega] to Mixture [Omega] or Mixture (\Int -> Omega), how do I apply the function model_func to all the omegas?
branch_site model_func fs ws posP posW branch_cats = MixtureModels branch_cats [bg_mixture,fg_mixture]
-- background omega distribution -- where the last omega is 1.0 (neutral)
    where bg_dist = zip fs (ws ++ [1.0])
-- accelerated omega distribution -- posW for all categories
          accel_dist = zip fs (repeat posW)
-- background branches always use the background omega distribution              
          bg_mixture = multiParameter_unit model_func (mix [1.0-posP, posP] [bg_dist, bg_dist])
-- foreground branches use the foreground omega distribution with probability posP
          fg_mixture = multiParameter_unit model_func (mix [1.0-posP, posP] [bg_dist, accel_dist])

branch_site_test model_func fs ws posP posW posSelection = branch_site model_func fs ws posP posW'
    where posW' = if (posSelection == 1) then posW else 1.0

mut_sel w' (ReversibleMarkov a smap q0 pi0 _ _ _) = reversible_markov a smap q pi where
    w = list_to_vector w'
    q = mut_sel_q q0 w
    pi = mut_sel_pi pi0 w

mut_sel' w' q0 = mut_sel w q0 where
    w = get_ordered_elements (alphabet_letters a) w' "fitnesses"
    a = getAlphabet q0

mut_sel_aa ws q@(ReversibleMarkov codon_a _ _ _ _ _ _) = mut_sel (aa_to_codon codon_a ws) q

mut_sel_aa' ws' q0 = mut_sel_aa ws q0 where
    ws = get_ordered_elements (alphabet_letters amino_alphabet) ws' "fitnesses"
    codon_alphabet = getAlphabet q0
    amino_alphabet = getAminoAcids codon_alphabet

fMutSel codon_a codon_w omega nuc_model = nuc_model & x3 codon_a & dNdS omega & mut_sel codon_w

fMutSel' codon_a codon_ws' omega nuc_model = fMutSel codon_a codon_ws omega nuc_model
    where codon_ws = get_ordered_elements (alphabet_letters codon_a) codon_ws' "fitnesses"

aa_to_codon codon_a xs = [xs_array!aa | codon <- codons, let aa = translate codon_a codon]
    where xs_array = listArray' xs
          codons = take n_letters [0..]
          n_letters = alphabetSize codon_a

-- \#1->let {w' = listAray' #1} in \#2 #3->fMutSel #0 codon_w #2 #3
-- The whole body of the function is let-floated up in round 2, and w' is eliminated.
fMutSel0 codon_a aa_w omega nuc_q  = fMutSel codon_a codon_w omega nuc_q
    where codon_w = aa_to_codon codon_a aa_w

fMutSel0' codon_a amino_ws' omega nuc_model = fMutSel0 codon_a amino_ws omega nuc_model
                                               where amino_ws = get_ordered_elements (alphabet_letters amino_a) amino_ws' "fitnesses"
                                                     amino_a = getAminoAcids codon_a

-- Issue: bad mixing on fMutSel model

gamma_rates_dist alpha = gamma alpha (1.0/alpha)

gamma_rates base alpha n = multi_rate_unif_bins base (gamma_rates_dist alpha) n

log_normal_rates_dist sigmaOverMu = log_normal lmu lsigma where x = log(1.0+sigmaOverMu^2)
                                                                lmu = -0.5*x
                                                                lsigma = sqrt x

log_normal_rates base sigmaOverMu n = multi_rate_unif_bins base (log_normal_rates_dist sigmaOverMu) n

--dp base rates fraction = multi_rate base dist where dist = zip fraction rates
free_rates base rates fraction = scaled_mixture (replicate (length fraction) base) rates fraction

transition_p_index tree smodel ds = mkArray (numBranches tree) (list_to_vector . branch_transition_p tree smodel ds)
-- * OK... so a mixture of rate matrices is NOT the same as a mixture of exponentiated matrices, because the rate matrices are scaled relative to each other.
--   ** Hmm... THAT might explain why the mixtures aren't working well!  We need to scale each of THOSE components separately.

-- * In theory, we should allow each mixture component to have a different number of states.  This would require
--   that we either split the condition likelihoods into per-component objects, or reserve sum(i,smap(i)) spots per cell.
--   Probably the latter one would be fine.

-- * The model from Sergei Kosakovsky-Pond is a SModelOnTreeMixture, since it is a mixture at the matrix level.

-- OK... so a mixture of rate matrices is NOT the same as a mixture of exponentiated matrices, because the rate matrices are scale with respect to each other.
-- So, we can have
--   ReversibleMarkov                          -- rate matrix
--   MixtureModel ReversibleMarkov             -- mixture of rate matrices
--   MixtureModels branch_cats MixtureModel    -- per-branch mixture of rate matrices, where component i always has the same frequencies.

-- We can construct mixtures of these things with e.g. gamma rate models.
--   Gamma rate models SHOULD be able to construct unit_mixtures WITHOUT the use of mmm or unit_mixture now.
--   We should also be able to constructing mixtures of mixtures of rate matrices -> mixtures of rate matrices.  This sounds like the join operation.

-- Then we have MBR models.  We should be able to get them by mixing together SmodelOnTrees for SingleBranchModels
--   This suggests an instance of SModelOnTree

-- class SModelOnTree a where
--   branch_transition_p       :: (SingleBranchLengthModel a) Int -> EVector<Matrix>
--   distribution              :: a -> [Double]
--   weighted_frequency_matrix :: a -> Matrix
--   weighted_matrix           :: a -> Matrix
--   nBaseModels               :: a -> Int
--   stateLetters              :: a -> EVector
--   getAlphabet               :: a -> b
--   componentFrequencies      :: a -> Int -> EVector

-- How about
-- scale :: a -> a ?
-- qExp  :: a -> Matrix?

-- Instances:
--   * ReversibleMarkovModel
--   * MixtureModel
--   * MixtureModels
--   * SModelOnTree a => SModelOnTreeMixture [(Double,a)]

-- So, the question is, can we avoid distribution on things like mixtures of Rates.

-- So, how are we going to handle rate scaling?  That should be part of the model!

-- branch_transition_p :: Tree -> a -> Array Int Double -> Int -> EVector
branch_transition_p tree smodel@(MixtureModels branch_cat_list mms) ds b = branch_transition_p tree mx ds b                        where mx = mms!!(branch_cat_list!!b)
branch_transition_p tree smodel@(MixtureModel cs                  ) ds b = [qExp $ scale (ds!b/r) component | (_,component) <- cs] where r = rate smodel
branch_transition_p tree smodel@(ReversibleMarkov _ _ _ _ _ _ _   ) ds b = [qExp $ scale (ds!b/r) smodel]                          where r = rate smodel

-- distribution :: a -> [Double]
distribution (MixtureModel l) = map fst l
distribution (MixtureModels _ (m:ms))                  = distribution m
distribution (ReversibleMarkov _ _ _ _ _ _ _) = [1.0]

-- weighted_frequency_matrix :: a -> Matrix
weighted_frequency_matrix (MixtureModels _ (m:ms)) = weighted_frequency_matrix m
weighted_frequency_matrix (MixtureModel d) = let model = MixtureModel d
                                                 dist = list_to_vector $ distribution model
                                                 freqs = list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1]
                                             in builtin_weighted_frequency_matrix dist freqs
weighted_frequency_matrix smodel@(ReversibleMarkov _ _ _ pi _ _ _) = builtin_weighted_frequency_matrix (list_to_vector [1.0]) pi

-- frequency_matrix :: a -> Matrix
frequency_matrix (MixtureModels _ (m:ms)) = frequency_matrix m
frequency_matrix (MixtureModel d) = let model = MixtureModel d
                                    in  builtin_frequency_matrix $ list_to_vector $ map (componentFrequencies model) [0..nBaseModels model-1]
frequency_matrix smodel@(ReversibleMarkov _ _ _ pi _ _ _) = builtin_frequency_matrix (list_to_vector [pi])

-- nBaseModels :: a -> Int
nBaseModels (MixtureModel l) = length l
nBaseModels (MixtureModels _ (m:ms)) = nBaseModels m
nBaseModels (ReversibleMarkov _ _ _ _ _ _ _) = 1

-- stateLetters :: a -> EVector
stateLetters (ReversibleMarkov _ smap _ _ _ _ _) = smap
stateLetters (F81 _ smap _ _ ) = smap
stateLetters (MixtureModel l) = stateLetters (baseModel (MixtureModel l) 0)
stateLetters (MixtureModels _ (m:ms)) = stateLetters m

-- getAlphabet :: a -> Alphabet
getAlphabet (ReversibleMarkov a _ _ _ _ _ _) = a
getAlphabet (F81 a _ _ _) = a
getAlphabet (MixtureModel l) = getAlphabet (baseModel (MixtureModel l) 0)
getAlphabet (MixtureModels _ (m:ms)) = getAlphabet m

-- componentFrequencies :: a -> Int -> EVector
componentFrequencies smodel@(ReversibleMarkov _ _ _ _ _ _ _) i = [frequencies smodel]!!i
componentFrequencies        (MixtureModel d)                 i = frequencies (baseModel (MixtureModel d) i)
componentFrequencies        (MixtureModels _ (m:ms))         i = componentFrequencies m i

---

unit_mixture m = MixtureModel (certainly m)

mmm m branch_cats = MixtureModels branch_cats [m]

empirical a filename = builtin_empirical a (listToString filename)

wag_frequencies a = zip (alphabet_letters a) (list_from_vector $ builtin_wag_frequencies a)
lg_frequencies a = zip (alphabet_letters a) (list_from_vector $ builtin_lg_frequencies a)

-- FIXME: need polymorphism.
--        This needs to be after weighted_frequency_matrix.
--        Because we have no polymorphism, wfm needs to be after MixtureModel and MixtureModels.
subst_like_on_tree topology root as alphabet smodel ts scale seqs = substitution_likelihood topology root seqs' as alphabet ps f
    where f = weighted_frequency_matrix smodel
          ps = transition_p_index topology smodel ds
          ds = listArray' $ map (scale*) ts
          seqs' = listArray' seqs

ctmc_on_tree topology root as alphabet smodel ts scale =
    Distribution (\seqs -> [subst_like_on_tree topology root as alphabet smodel ts scale seqs]) (no_quantile "ctmc_on_tree") () ()
