module SModel.ReversibleMarkov (module SModel.ReversibleMarkov,
                                module SModel.Markov,
                                module Reversible)
    where

import           Reversible
import           Bio.Alphabet
import           Markov (CTMC(..))
import qualified Markov
import           SModel.Markov
import           SModel.Simple
import           SModel.Rate
import           Tree (HasBranchLengths(..))
import           Reversible    

equ a = Markov.equ (alphabetSize a) 1.0

gtr_sym exchange a = Markov.gtr_sym (alphabetSize a) exchange 

gtr a s pi = scaleTo 1 $ setReversibility EqRev $ markov a (simpleSMap a) (s %*% plus_f_matrix pi') pi' where pi' = toVector pi

f81     pi a = gtr a (equ a) pi
jukes_cantor a = gtr a (equ a) (uniform_frequencies a)

gtr' s'    pi a = gtr a (gtr_sym' s'    a) (frequenciesFromDict a pi)

letter_pair_names a = pairNames $ Markov.all_pairs (getLetters a)

-- factor out code to get gtr exch list
-- maybe put ReversibleFrequency into this file.
-- clean up f1x4 and f3x4?
gtr_sym' es' a = gtr_sym es a where lpairs = Markov.all_pairs (getLetters a)
                                    es :: [Double]
                                    es = if length lpairs == length es' then
                                             [Markov.get_element_exchange es' (l1++l2) (l2++l1)| (l1,l2) <- lpairs]
                                         else
                                             error $ "Expected "++show (length lpairs)++" exchangeabilities but got "++ show (length es')++"!"

plus_f   a pi s   = gtr a s pi
plus_fe  a s      = plus_f a (uniform_frequencies a) s
plus_gwf a pi f s = setReversibility EqRev $ markov a (simpleSMap a) (s %*% plus_gwf_matrix pi' f) pi' where pi' = toVector pi

plus_f'  a pi s   = plus_f a (frequenciesFromDict a pi) s
plus_gwf'  a pi f s = plus_gwf a (frequenciesFromDict a pi) f s
