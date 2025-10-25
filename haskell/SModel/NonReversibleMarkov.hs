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
import           Tree (HasBranchLengths(..))

-- The only guarantee is that we are at equilibrium
type NonReversibleMarkov = MkEquilibrium Markov

-- This is used both for observations, and also to determine which states are the same for computing rates.
instance HasSMap m => HasSMap (MkEquilibrium m) where
    getSMap (Equilibrium m) = getSMap m

instance HasAlphabet m => HasAlphabet (MkEquilibrium m) where
    getAlphabet (Equilibrium m) = getAlphabet m

instance HasBranchLengths t => SimpleSModel t (MkEquilibrium Markov) where
    branchTransitionP (SModelOnTree tree smodel) b = [qExp $ scaleBy (branchLength tree b) smodel]
    stateLetters (SModelOnTree _ model) = getSMap model
    componentFrequencies (SModelOnTree _ smodel) = [getStartFreqs smodel]

instance RateModel NonReversibleMarkov where
    rate (Equilibrium m) = rate m

nonRev a rates = scaleTo 1 $ Markov.Equilibrium $ markov' a smap q
    where smap = simpleSMap a
          n = length $ getLetters a
          q = Markov.non_rev_from_list n rates

nonRev' a rates' = nonRev a rs
    where lPairs = allOrderedPairs (getLetters a)
          rs = if length lPairs == length rates' then
                   [ Markov.getElement rates' (l1++l2) | (l1,l2) <- lPairs]
               else
                   error $ "Expected "++show (length lPairs)++" rates but got "++ show (length rates')++"!"


