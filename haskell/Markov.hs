module Markov where

import Data.Matrix
import SModel.Rate

foreign import bpcall "SModel:gtr_sym" builtin_gtr_sym :: EVector Double -> Int -> Matrix Double
foreign import bpcall "SModel:" non_rev_from_vec :: Int -> EVector Double -> Matrix Double
foreign import bpcall "SModel:fixup_diagonal_rates" fixup_diagonal_rates :: Matrix Double -> Matrix Double
foreign import bpcall "SModel:plus_gwf_matrix" plus_gwf_matrix :: EVector Double -> Double -> Matrix Double
foreign import bpcall "SModel:MatrixExp" mexp :: Matrix Double -> Double -> Matrix Double
foreign import bpcall "SModel:compute_check_stationary_freqs" builtin_get_check_pi :: Matrix Double -> EVector Double -> EVector Double
foreign import bpcall "SModel:compute_stationary_freqs" builtin_get_pi :: Matrix Double -> EVector Double

-- We don't have rates here, because rates require a concept of states being "equal".
-- For cases like markov modulated models, the rate we care about is the rate of switching letter,
--   not the rate of switching states.
-- For codon models, we could care about the rate of switching codons, nucleotides, or amino acids.
--   -- we prefer to scale branch lengths in terms of nucleotide changes.
-- We also only care about the rate at equilibrium.
-- Its possible to run nonreversible models from a non-equilibrium rate, but also we probably sometimes
--  want to use the equilibrium rate.

-- For functions like equ, f81, and gtr, maybe I also need versions that just construct the matrix?

-- Storing the rate separately means that we don't need to recompute the equilibrium frequencies
--    when we are just rescaling.
-- Originally, it probably was a way to avoid recomputing the eigensystem when rescaling.

class Scalable c => CTMC c where
    get_q :: c -> Matrix Double
    get_pi :: c -> EVector Double
    qExp :: c -> Matrix Double

    get_pi m = builtin_get_pi (get_q m)
    qExp m = mexp (get_q m) 1.0

-- should I add a field for the stationary freqs?
-- I could allow specifying it, or alternatively computing it and caching it.
data Markov = Markov (Matrix Double) Double

non_reversible_markov q = Markov (fixup_diagonal_rates q) 1.0

non_rev_from_list n rates = non_rev_from_vec n (list_to_vector rates)

instance Scalable Markov where
    scale f (Markov q s) = Markov q (s*f)

instance CTMC Markov where
    get_q  (Markov q scale) = scaleMatrix scale q
    qExp   (Markov q scale) = mexp q scale

data ReversibleMarkov = ReversibleMarkov (Matrix Double) (EVector Double) Double

instance Scalable ReversibleMarkov where
    scale f (ReversibleMarkov q pi s) = ReversibleMarkov q pi (s*f)

instance CTMC ReversibleMarkov where
    get_q  (ReversibleMarkov q _  scale) = scaleMatrix scale q
    get_pi (ReversibleMarkov _ pi scale) = pi
    qExp   (ReversibleMarkov q _  scale) = mexp q scale

plus_f_matrix pi = plus_gwf_matrix pi 1.0

reversible_markov q pi = ReversibleMarkov (fixup_diagonal_rates q) pi 1.0

gtr_sym n exchange = builtin_gtr_sym (list_to_vector exchange) n

gtr er pi = reversible_markov (er %*% plus_f_matrix pi') pi' where pi' = list_to_vector pi

-- Probabily we should make a builtin for this
equ n x = gtr_sym n (replicate n_elements x)
    where n_elements = n*(n-1) `div` 2

f81 pi = gtr (equ n 1.0) pi where n = length pi

uniform_frequencies n = replicate n $ 1.0/(intToDouble n)

jukes_cantor n = gtr (equ n 1.0) (uniform_frequencies n)

