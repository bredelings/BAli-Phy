module SModel.ReversibleMarkov (module SModel.ReversibleMarkov, module SModel.Frequency, module Data.Matrix) where

import qualified Markov
import           Markov (CTMC, getQ, getPi, qExp)
import           SModel.Simple
import           SModel.Rate
import           SModel.Frequency
import           Bio.Alphabet
import           Data.Matrix
import           Tree
import           SModel.EigenExp

foreign import bpcall "SModel:get_equilibrium_rate" get_equilibrium_rate :: Alphabet -> EVector Int -> Matrix Double -> EVector Double -> Double

-- This takes the rate matrix q and adds:
-- * pi -> a cached version of the equilibrium frequencies
-- * t -> a scaling factor
-- * r -> a cached version of the rate at equilibrium
--
-- * the observation model
--   + a -> an alphabet
--   + smap -> mapping from markov states -> alphabet states

-- Now, all of these except (a,smap) are just caches for things that we can compute from q.
-- It is the (a,smap) parts that make this into a SUBSTITUTION model.

-- What the LIKELIHOOD wants is just:
-- * component probabilities
--   - number of components = component_probabilities.size()
-- * mapping from states -> observed letters
--   - number of states = smap(m).size()
-- * for each branch, the probability of transitioning from i->j in component m.
--   That is, transition_p(tree, branch, mixture) = matrix(i,j)
-- * for each component, the root frequencies -> root_frequencies(m)
-- I guess this is a "LikelihoodMixtureModel"
-- Ideally, it would also specify if it is reversible
--   that is, all transition_p matricies in component m satisfy detailed balance with respect to the root frequences in component m
--

data ReversibleMarkov = ReversibleMarkov Alphabet (EVector Int) Markov.ReversibleMarkov Double

-- This is used both for observations, and also to determine which states are the same for computing rates.
instance HasSMap ReversibleMarkov where
    get_smap (ReversibleMarkov _ s _ _) = s

instance CTMC ReversibleMarkov where
    qExp (ReversibleMarkov _ _ m _) = qExp m
    getPi (ReversibleMarkov _ _ m _) = getPi m
    getQ (ReversibleMarkov _ _ m  _) = getQ m

data Markov = Markov Alphabet (EVector Int) Markov.Markov Double

-- This is used both for observations, and also to determine which states are the same for computing rates.
instance HasSMap Markov where
    get_smap (Markov _ s _ _) = s

instance CTMC Markov where
    qExp (Markov _ _ m _) = qExp m
    getPi (Markov _ _ m _) = getPi m
    getQ (Markov _ _ m  _) = getQ m

frequencies = getPi

simple_smap a = list_to_vector [0..(alphabetSize a)-1]

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
reversibleMarkov a smap q pi = ReversibleMarkov a smap rm rate where
    rm = Markov.reversibleMarkov q pi
    rate = get_equilibrium_rate a smap (getQ rm) pi

equ a = Markov.equ (alphabetSize a) 1.0

gtr_sym exchange a = Markov.gtr_sym (alphabetSize a) exchange 

gtr a s pi = reversibleMarkov a (simple_smap a) (s %*% plus_f_matrix pi') pi' where pi' = list_to_vector pi

f81     pi a = gtr a (equ a) pi
jukes_cantor a = gtr a (equ a) (uniform_frequencies a)

gtr' s'    pi a = gtr a (gtr_sym' s'    a) (frequencies_from_dict a pi)

-- es' is a [(String,Double)] here
letter_pair_names a = [l1++l2|(l1,l2) <- Markov.all_pairs (letters a)]

-- factor out code to get gtr exch list
-- maybe put ReversibleFrequency into this file.
-- clean up f1x4 and f3x4?
gtr_sym' es' a = gtr_sym es a where lpairs = Markov.all_pairs (letters a)
                                    es :: [Double]
                                    es = if length lpairs == length es' then
                                             [Markov.get_element_exchange es' (l1++l2) (l2++l1)| (l1,l2) <- lpairs]
                                         else
                                             error $ "Expected "++show (length lpairs)++" exchangeabilities but got "++ show (length es')++"!"

plus_f   a pi s   = gtr a s pi
plus_fe  a s      = plus_f a (uniform_frequencies a) s
plus_gwf a pi f s = reversibleMarkov a (simple_smap a) (s %*% plus_gwf_matrix pi' f) pi' where pi' = list_to_vector pi

plus_f'  a pi s   = plus_f a (frequencies_from_dict a pi) s
plus_gwf'  a pi f s = plus_gwf a (frequencies_from_dict a pi) f s

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
markov a smap q pi = Markov a smap rm rate where
    rm = Markov.markov q pi
    rate = get_equilibrium_rate a smap (getQ rm) pi

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
markov' a smap q = Markov a smap rm rate where
    rm = Markov.markov' q
    rate = get_equilibrium_rate a smap (getQ rm) (getPi rm)

instance HasAlphabet ReversibleMarkov where
    getAlphabet (ReversibleMarkov a _ _ _) = a

instance HasAlphabet Markov where
    getAlphabet (Markov a _ _ _) = a

instance SimpleSModel ReversibleMarkov where
    type instance IsReversible ReversibleMarkov = Reversible
    branch_transition_p (SingleBranchLengthModel tree smodel factor) b = [qExp $ scale (branch_length tree b * factor / r) smodel]
        where r = rate smodel
    distribution _ = [1.0]
    nBaseModels _ = 1
    stateLetters rm = get_smap rm
    componentFrequencies smodel i = [frequencies smodel]!!i

instance SimpleSModel Markov where
    type instance IsReversible Markov = NonReversible
    branch_transition_p (SingleBranchLengthModel tree smodel factor) b = [qExp $ scale (branch_length tree b * factor / r) smodel]
        where r = rate smodel
    distribution _ = [1.0]
    nBaseModels _ = 1
    stateLetters rm = get_smap rm
    componentFrequencies smodel i = [frequencies smodel]!!i

instance Scalable ReversibleMarkov where
    scale x (ReversibleMarkov a s rm r) = ReversibleMarkov a s (scale x rm) (x*r)

instance Scalable Markov where
    scale x (Markov a s rm r) = Markov a s (scale x rm) (x*r)

instance RateModel ReversibleMarkov where
    rate (ReversibleMarkov _ _ _ r) = r

instance RateModel Markov where
    rate (Markov _ _ _ r) = r

-- A markov model needs a map from state -> letter in order to have a rate!
-- For codon models, we basically use smap = id for nucleotides (then divide by three)
--   and amino acids.
-- If we had a covarion model on codons, then we'd need to first collaps the state to
-- a codon, and then collapse the codons to either (i) amino acids or (ii) codons, and then divide by three.

instance Show ReversibleMarkov where
    show q = show $ getQ q

instance Show Markov where
    show q = show $ getQ q
