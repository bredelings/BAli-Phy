module SModel.ReversibleMarkov (module SModel.ReversibleMarkov,
                                module SModel.Markov,
                                MkReversible(..),
                                reversible,
                                nonreversible)
    where

import           Bio.Alphabet
import           Markov (MkReversible(..), reversible, nonreversible, CTMC(..))
import qualified Markov
import           SModel.Markov
import           SModel.Simple
import           SModel.Rate
import           Tree (branch_length)

data ExchangeModel a = ExchangeModel Alphabet a

instance Show a => Show (ExchangeModel a) where
    show (ExchangeModel _ x) = show x

equ a = ExchangeModel a (Markov.equ (alphabetSize a) 1)

gtr_sym exchange a = ExchangeModel a (Markov.gtr_sym (alphabetSize a) exchange)

instance HasAlphabet (ExchangeModel a) where
    getAlphabet (ExchangeModel a _) = a

gtr (ExchangeModel a s) pi = reversible $ markov a (simple_smap a) (s %*% plus_f_matrix pi') pi' where pi' = list_to_vector pi

f81     pi a = gtr (equ a) pi
jukes_cantor a = gtr (equ a) (uniform_frequencies a)

gtr' s'    pi a = gtr (gtr_sym' s'    a) (frequencies_from_dict a pi)

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

plus_f   pi s   = gtr s pi
plus_fe  s      = plus_f (uniform_frequencies a) s where a = getAlphabet s
plus_gwf pi f (ExchangeModel a s) = reversible $ markov a (simple_smap a) (s %*% plus_gwf_matrix pi' f) pi' where pi' = list_to_vector pi

plus_f'  pi s   = plus_f (frequencies_from_dict a pi) s where a = getAlphabet s
plus_gwf' pi f s = plus_gwf (frequencies_from_dict a pi) f s where a = getAlphabet s

type ReversibleMarkov = MkReversible Markov

-- This is used both for observations, and also to determine which states are the same for computing rates.
instance HasSMap m => HasSMap (MkReversible m) where
    get_smap (Reversible m) = get_smap m

instance HasAlphabet m => HasAlphabet (MkReversible m) where
    getAlphabet (Reversible m) = getAlphabet m

instance SimpleSModel (MkReversible Markov) where
    type instance IsReversible (MkReversible Markov) = EquilibriumReversible
    branch_transition_p (SingleBranchLengthModel tree smodel factor) b = [qExp $ scale (branch_length tree b * factor / r) smodel]
        where r = rate smodel
    distribution _ = [1.0]
    nBaseModels _ = 1
    stateLetters rm = get_smap rm
    componentFrequencies smodel i = [getStartFreqs smodel]!!i

instance RateModel ReversibleMarkov where
    rate (Reversible m) = rate m

