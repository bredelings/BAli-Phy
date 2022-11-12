module Markov where

import Data.Matrix

foreign import bpcall "SModel:gtr_sym" builtin_gtr_sym :: EVector Double -> Int -> Matrix Double
foreign import bpcall "SModel:fixup_diagonal_rates" fixup_diagonal_rates :: Matrix Double -> Matrix Double
foreign import bpcall "SModel:plus_gwf_matrix" plus_gwf_matrix :: EVector Double -> Double -> Matrix Double

-- For functions like equ, f81, and gtr, maybe I also need versions that just construct the matrix?
data ReversibleMarkov = ReversibleMarkov (Matrix Double) (EVector Double) Double

plus_f_matrix pi = plus_gwf_matrix pi 1.0

get_q (ReversibleMarkov q _ scale) = scaleMatrix scale q

get_pi (ReversibleMarkov _ pi _) = pi

-- In theory we could take just (a,q) since we could compute smap from a (if states are simple) and pi from q.
reversible_markov q pi = ReversibleMarkov (fixup_diagonal_rates q) pi 1.0

gtr_sym n exchange = builtin_gtr_sym (list_to_vector exchange) n

gtr er pi = reversible_markov (er %*% plus_f_matrix pi') pi' where pi' = list_to_vector pi

-- Probabily we should make a builtin for this
equ n x = gtr_sym n (replicate n_elements x)
    where n_elements = n*(n-1) `div` 2

