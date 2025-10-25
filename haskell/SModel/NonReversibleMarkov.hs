module SModel.NonReversibleMarkov (module SModel.NonReversibleMarkov,
                                   module SModel.Markov)
    where

import           Bio.Alphabet
import           Markov (CTMC(..))
import qualified Markov
import           SModel.Markov
import           SModel.Simple
import           SModel.Rate
import           Tree (HasBranchLengths(..))

nonRev a rates = scaleTo 1 $ markov' a smap q
    where smap = simpleSMap a
          n = length $ getLetters a
          q = Markov.non_rev_from_list n rates

nonRev' a rates' = nonRev a rs
    where lPairs = allOrderedPairs (getLetters a)
          rs = if length lPairs == length rates' then
                   [ Markov.getElement rates' (l1++l2) | (l1,l2) <- lPairs]
               else
                   error $ "Expected "++show (length lPairs)++" rates but got "++ show (length rates')++"!"


