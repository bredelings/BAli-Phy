module SModel.ReversibleMarkov (module SModel.ReversibleMarkov, module SModel.Frequency) where

import SModel.Simple
import SModel.Rate
import SModel.Frequency
import Bio.Alphabet
import Data.Matrix
import Tree
import SModel.EigenExp

foreign import bpcall "SModel:get_equilibrium_rate" get_equilibrium_rate :: Alphabet -> EVector Int -> Matrix Double -> EVector Double -> Double
foreign import bpcall "SModel:MatrixExp" mexp :: Matrix Double -> Double -> Matrix Double
foreign import bpcall "SModel:gtr_sym" builtin_gtr_sym :: EVector Double -> Int -> Matrix Double
foreign import bpcall "SModel:fixup_diagonal_rates" fixup_diagonal_rates :: Matrix Double -> Matrix Double

data ReversibleMarkov = ReversibleMarkov Alphabet (EVector Int) (Matrix Double) (EVector Double) Double Double

qExp (ReversibleMarkov a s q pi t r) = mexp q t

get_smap' (ReversibleMarkov a s q pi t r) = s

get_alphabet (ReversibleMarkov a s q pi t r) = a

get_q (ReversibleMarkov _ _ q _ t _) = scaleMatrix t q

get_pi (ReversibleMarkov _ _ _ pi _ _) = pi

frequencies (ReversibleMarkov _ _ _ pi _ _) = pi

simple_smap a = list_to_vector [0..(alphabetSize a)-1]

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
reversible_markov a smap q pi = ReversibleMarkov a smap q2 pi 1.0 (get_equilibrium_rate a smap q2 pi) where q2 = fixup_diagonal_rates q

generic_equ n x = generic_gtr_sym (replicate n_elements x) n
    where n_elements = n*(n-1) `div` 2

equ a = generic_equ (alphabetSize a) 1.0

generic_gtr_sym exchange n = builtin_gtr_sym (list_to_vector exchange) n
gtr_sym exchange a = generic_gtr_sym exchange (alphabetSize a)

gtr a s pi = reversible_markov a (simple_smap a) (s %*% plus_f_matrix pi') pi' where pi' = list_to_vector pi

f81     pi a = gtr a (equ a) pi
jukes_cantor a = gtr a (equ a) (uniform_frequencies a)

gtr' s'    pi a = gtr a (gtr_sym' s'    a) (frequencies_from_dict a pi)

-- es' is a [(String,Double)] here
all_pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

letter_pair_names a = [l1++l2|(l1,l2) <- all_pairs (letters a)]

get_element_exchange []                 x y = error ("No exchangeability specified for '" ++ x ++ "'")
get_element_exchange ((key,value):rest) x y = if key == x || key == y then value else get_element_exchange rest x y

-- factor out code to get gtr exch list
-- maybe put ReversibleFrequency into this file.
-- clean up f1x4 and f3x4?
gtr_sym' es' a = gtr_sym es a where lpairs = all_pairs (letters a)
                                    es :: [Double]
                                    es = if length lpairs == length es' then
                                             [get_element_exchange es' (l1++l2) (l2++l1)| (l1,l2) <- lpairs]
                                         else
                                             error $ "Expected "++show (length lpairs)++" exchangeabilities but got "++ show (length es')++"!"

plus_f   a pi s   = gtr a s pi
plus_fe  a s      = plus_f a (uniform_frequencies a) s
plus_gwf a pi f s = reversible_markov a (simple_smap a) (s %*% plus_gwf_matrix pi' f) pi' where pi' = list_to_vector pi

plus_f'  a pi s   = plus_f a (frequencies_from_dict a pi) s
plus_gwf'  a pi f s = plus_gwf a (frequencies_from_dict a pi) f s

instance SimpleSModel ReversibleMarkov where
    get_smap (ReversibleMarkov _ s _ _ _ _) = s
    branch_transition_p (SingleBranchLengthModel tree smodel@(ReversibleMarkov _ _ _ _ _ _   )) b = [qExp $ scale (branch_length tree b/r) smodel]
        where r = rate smodel
    distribution _ = [1.0]
    weighted_frequency_matrix smodel@(ReversibleMarkov _ _ _ pi _ _) = builtin_weighted_frequency_matrix (list_to_vector [1.0]) (list_to_vector [pi])
    frequency_matrix smodel@(ReversibleMarkov _ _ _ pi _ _) = builtin_frequency_matrix (list_to_vector [pi])
    nBaseModels _ = 1
    stateLetters (ReversibleMarkov _ smap _ _ _ _) = smap
    getAlphabet (ReversibleMarkov a _ _ _ _ _) = a
    componentFrequencies smodel@(ReversibleMarkov _ _ _ _ _ _) i = [frequencies smodel]!!i

instance RateModel ReversibleMarkov where
    rate (ReversibleMarkov a s q pi t r) = r
    scale x (ReversibleMarkov a s q pi t r) = ReversibleMarkov a s q pi (x*t) (x*r)

instance Show ReversibleMarkov where
    show q = show $ get_q q

