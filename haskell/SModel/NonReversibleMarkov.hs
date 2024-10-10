module SModel.NonReversibleMarkov (module SModel.NonReversibleMarkov,
                                   module SModel.Markov)
    where
import           Bio.Alphabet
import qualified Markov
import           SModel.Markov
import           SModel.Simple
import           SModel.Rate
import           Tree (branchLength)

nonRev a rates = markov' a smap q
    where smap = simple_smap a
          n = length $ letters a
          q = Markov.non_rev_from_list n rates

nonRev' a rates' = nonRev a rs
    where lPairs = allOrderedPairs (letters a)
          rs = if length lPairs == length rates' then
                   [ Markov.getElement rates' (l1++l2) | (l1,l2) <- lPairs]
               else
                   error $ "Expected "++show (length lPairs)++" rates but got "++ show (length rates')++"!"


