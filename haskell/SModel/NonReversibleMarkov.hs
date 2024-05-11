module SModel.NonReversibleMarkov (module SModel.NonReversibleMarkov,
                                   module SModel.Markov,
                                   MkEquilibrium(..),
                                   equilibrium,
                                   nonequilibrium)
    where

import           Bio.Alphabet
import           Markov (MkEquilibrium(..), equilibrium, nonequilibrium, CTMC(..))
import qualified Markov
import           SModel.Markov
import           SModel.Simple
import           SModel.Rate
import           Tree (branch_length)

-- The only guarantee is that we are at equilibrium
type NonReversibleMarkov = MkEquilibrium Markov

-- This is used both for observations, and also to determine which states are the same for computing rates.
instance HasSMap m => HasSMap (MkEquilibrium m) where
    get_smap (Equilibrium m) = get_smap m

instance HasAlphabet m => HasAlphabet (MkEquilibrium m) where
    getAlphabet (Equilibrium m) = getAlphabet m

instance SimpleSModel (MkEquilibrium Markov) where
    type instance IsReversible (MkEquilibrium Markov) = EquilibriumNonReversible
    branch_transition_p (SingleBranchLengthModel tree smodel factor) b = [qExp $ scale (branch_length tree b * factor / r) smodel]
        where r = rate smodel
    distribution _ = [1.0]
    nBaseModels _ = 1
    stateLetters rm = get_smap rm
    componentFrequencies smodel i = [getStartFreqs smodel]!!i

instance RateModel NonReversibleMarkov where
    rate (Equilibrium m) = rate m

nonRev a rates = Markov.Equilibrium $ markov' a smap q
    where smap = simple_smap a
          n = length $ letters a
          q = Markov.non_rev_from_list n rates

nonRev' a rates' = nonRev a rs
    where lPairs = allOrderedPairs (letters a)
          rs = if length lPairs == length rates' then
                   [ Markov.getElement rates' (l1++l2) | (l1,l2) <- lPairs]
               else
                   error $ "Expected "++show (length lPairs)++" rates but got "++ show (length rates')++"!"


