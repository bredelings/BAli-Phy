module SModel.Markov (module SModel.Markov,
                      module SModel.Frequency,
                      module Data.Matrix,
                      module Markov
                     ) where

import qualified Markov
import           Markov (CTMC(..),
                         CheckReversible(..),
                         checkReversible,
                         checkStationary,
                         reversible,
                         equilibriumReversible)
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

-- Fields are: alphabet, smap, q, and cached rate.

data Markov = Markov Alphabet (EVector Int) Markov.Markov Double

-- This is used both for observations, and also to determine which states are the same for computing rates.
instance HasSMap Markov where
    get_smap (Markov _ s _ _) = s

instance CTMC Markov where
    qExp (Markov _ _ m _) = qExp m
    getStartFreqs (Markov _ _ m _) = getStartFreqs m
    getEqFreqs (Markov _ _ m _) = getEqFreqs m
    getQ (Markov _ _ m  _) = getQ m

instance CheckReversible Markov where
    isReversible m = checkReversible (getQ m) (getEqFreqs m)
    isStationary m = checkStationary (getQ m) (getStartFreqs m)

simple_smap a = list_to_vector [0..(alphabetSize a)-1]

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
markov a smap q pi = Markov a smap rm rate where
    rm = Markov.markov q pi
    rate = get_equilibrium_rate a smap (getQ rm) pi

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
markov' a smap q = Markov a smap rm rate where
    rm = Markov.markov' q
    rate = get_equilibrium_rate a smap (getQ rm) (getEqFreqs rm)

instance HasAlphabet Markov where
    getAlphabet (Markov a _ _ _) = a

instance SimpleSModel Markov where
    branch_transition_p (SingleBranchLengthModel tree smodel factor) b = [qExp $ scale (branchLength tree b * factor / r) smodel]
        where r = rate smodel
    distribution _ = [1.0]
    nBaseModels _ = 1
    stateLetters rm = get_smap rm
    componentFrequencies smodel i = [getStartFreqs smodel]!!i

instance Scalable Markov where
    scale x (Markov a s rm r) = Markov a s (scale x rm) (x*r)

instance RateModel Markov where
    rate (Markov _ _ _ r) = r

-- A markov model needs a map from state -> letter in order to have a rate!
-- For codon models, we basically use smap = id for nucleotides (then divide by three)
--   and amino acids.
-- If we had a covarion model on codons, then we'd need to first collaps the state to
-- a codon, and then collapse the codons to either (i) amino acids or (ii) codons, and then divide by three.

instance Show Markov where
    show q = show $ getQ q

nonEq a rates pi = markov a smap q pi
    where smap = simple_smap a
          n = length $ letters a
          q = Markov.non_rev_from_list n rates

pairNames ls = [l1 ++ l2 | (l1,l2) <- ls]

allOrderedPairs l = [(x,y) | x <- l, y <- l, x /= y]

orderedLetterPairNames a = pairNames $ allOrderedPairs (letters a)

nonEq' a rates' pi' = nonEq a rs pi
    where lPairs = allOrderedPairs (letters a)
          rs = if length lPairs == length rates' then
                   [ Markov.getElement rates' (l1++l2) | (l1,l2) <- lPairs]
               else
                   error $ "Expected "++show (length lPairs)++" rates but got "++ show (length rates')++"!"
          pi = list_to_vector $ frequencies_from_dict a pi'

labelledEqFrequencies m = zip (letters a) frequencies
    where frequencies = list_from_vector $ getEqFreqs m
          a = getAlphabet m

labelledStartFrequencies m = zip (letters a) frequencies
    where frequencies = list_from_vector $ getStartFreqs m
          a = getAlphabet m
