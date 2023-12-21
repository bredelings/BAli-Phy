module SModel.ReversibleMarkov (module SModel.ReversibleMarkov, module SModel.Markov) where

import           Bio.Alphabet
import           Markov (CTMC, getQ, getPi, qExp)
import qualified Markov
import           SModel.Markov
import           SModel.Simple
import           SModel.Rate
import           Tree (branch_length)

equ a = Markov.equ (alphabetSize a) 1.0

gtr_sym exchange a = Markov.gtr_sym (alphabetSize a) exchange 

gtr a s pi = reversible $ markov a (simple_smap a) (s %*% plus_f_matrix pi') pi' where pi' = list_to_vector pi

f81     pi a = gtr a (equ a) pi
jukes_cantor a = gtr a (equ a) (uniform_frequencies a)

gtr' s'    pi a = gtr a (gtr_sym' s'    a) (frequencies_from_dict a pi)

letter_pair_names a = pairNames $ Markov.all_pairs (letters a)

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
plus_gwf a pi f s = reversible $ markov a (simple_smap a) (s %*% plus_gwf_matrix pi' f) pi' where pi' = list_to_vector pi

plus_f'  a pi s   = plus_f a (frequencies_from_dict a pi) s
plus_gwf'  a pi f s = plus_gwf a (frequencies_from_dict a pi) f s

data ReversibleMarkov = Reversible Markov

reversible = Reversible

nonreversible (Reversible m) = m

-- This is used both for observations, and also to determine which states are the same for computing rates.
instance HasSMap ReversibleMarkov where
    get_smap (Reversible m) = get_smap m

instance CTMC ReversibleMarkov where
    qExp (Reversible m) = qExp m
    getPi (Reversible m) = getPi m
    getQ (Reversible m) = getQ m

instance HasAlphabet ReversibleMarkov where
    getAlphabet (Reversible m) = getAlphabet m

instance SimpleSModel ReversibleMarkov where
    type instance IsReversible ReversibleMarkov = Reversible
    branch_transition_p (SingleBranchLengthModel tree smodel factor) b = [qExp $ scale (branch_length tree b * factor / r) smodel]
        where r = rate smodel
    distribution _ = [1.0]
    nBaseModels _ = 1
    stateLetters rm = get_smap rm
    componentFrequencies smodel i = [frequencies smodel]!!i

instance Scalable ReversibleMarkov where
    scale x (Reversible m) = Reversible (scale x m)

instance RateModel ReversibleMarkov where
    rate (Reversible m) = rate m

instance Show ReversibleMarkov where
    show (Reversible m) = show m

