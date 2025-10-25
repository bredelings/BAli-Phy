module Markov where

import Data.Matrix
import SModel.Rate
import EigenExp
import Reversible

foreign import bpcall "SModel:gtr_sym" builtin_gtr_sym :: EVector Double -> Int -> Matrix Double
foreign import bpcall "SModel:" non_rev_from_vec :: Int -> EVector Double -> Matrix Double
foreign import bpcall "SModel:" fixupDiagonalRates :: Matrix Double -> Matrix Double
foreign import bpcall "SModel:plus_gwf_matrix" plus_gwf_matrix :: EVector Double -> Double -> Matrix Double
foreign import bpcall "Matrix:MatrixExp" mexp :: Matrix Double -> Double -> Matrix Double
foreign import bpcall "SModel:compute_check_stationary_freqs" builtin_get_check_pi :: Matrix Double -> EVector Double -> EVector Double
foreign import bpcall "SModel:" equilibriumLimit :: EVector Double -> Matrix Double -> EVector Double
foreign import bpcall "SModel:compute_stationary_freqs" builtin_getEqFreqs :: Matrix Double -> EVector Double
foreign import bpcall "SModel:" checkReversible :: Matrix Double -> EVector Double -> Bool
foreign import bpcall "SModel:" checkStationary :: Matrix Double -> EVector Double -> Bool

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
    getQ :: c -> Matrix Double
    getStartFreqs :: c -> EVector Double
    getEqFreqs :: c -> EVector Double
    qExp :: c -> Matrix Double

    getEqFreqs m = equilibriumLimit (getStartFreqs m) (getQ m) 
    qExp m = mexp (getQ m) 1

-- Should I add gtr, equ n x, and f81 to this class? Probably...

-- Should I add SModel.ReversibleMarkov to this class?

-- Can I make an SModel.Markov?

instance Scalable (Matrix Double) where
    scaleBy f m = scaleMatrix f m

instance CheckReversible (Matrix Double) where
    getReversibility m | stat && rev = EqRev
                       | stat        = EqNonRev
                       | otherwise   = NonEq
                       where rev = checkReversible (getQ m) (getEqFreqs m)
                             stat = checkStationary (getQ m) (getStartFreqs m)

instance CTMC (Matrix Double) where
    getQ m = m
    getStartFreqs = error "No start freqs for Matrix Double"

-- SHould I rename this to ctmc?
-- can I hide the constructor, to guarantee that rows sum to zero, and frequencies sum to 1?

-- Fields are: q, start frequencies, rate, possibly eigendecomposition, reversibility
data Markov = Markov (Matrix Double) (EVector Double) Double MatDecomp Reversibility

-- can I hide the Markov constructor?
-- should I rename this function to ctmc?
markov q pi = Markov qFixed pi 1 (NoDecomp Nothing) NonEq where
    qFixed = fixupDiagonalRates q

-- If we're starting from the equilibrium, then I guess we're EqNonRev?
markov' q = Markov qFixed (builtin_getEqFreqs qFixed) 1 (NoDecomp Nothing) EqNonRev where
    qFixed = fixupDiagonalRates q

non_rev_from_list n rates = non_rev_from_vec n (toVector rates)

instance Scalable Markov where
    scaleBy f (Markov q pi s decomp rev) = Markov q pi (s*f) decomp rev

instance CheckReversible Markov where
    getReversibility (Markov _ _ _ _ r) = r

instance CanMakeReversible Markov where
--    setReversibility r1  m@(Markov q f s e r2) | r1 == r2  = m

    setReversibility EqRev (Markov q pi s _ _ ) = Markov q pi s decomp EqRev
        where decomp = case getEigensystem q pi of Just e -> RealEigenDecomp e
                                                   Nothing -> NoDecomp (Just NoDiagReason)
    
    setReversibility r      (Markov q f s _ _ ) = Markov q f s (NoDecomp Nothing) r
                                        
                                          
instance CTMC Markov where
    getQ  (Markov q _  factor _ _) = scaleMatrix factor q
    getStartFreqs (Markov _ pi _ _ _) = pi
    qExp   (Markov q _  factor (NoDecomp _) _) = mexp q factor
    qExp   (Markov q pi  factor (RealEigenDecomp eigensys) _) =
        case lExp eigensys pi factor of Just mat -> mat
                                        Nothing -> mexp q factor

-- Wrapper class to mark things reversible AND at equilibrium.
-- Used for both Markov.Markov and SModel.Markov.
-- Which is why it takes any type m.

instance Show Markov where
    show m@(Markov _ _ _ decomp _) = "Markov " ++ show decomp ++ "\n" ++ show (getQ m)

-- Wrapper class to mark things reversible AND at equilibrium.
-- Used for both Markov.Markov and SModel.Markov.
-- Which is why it takes any type m.
----------------------------

plus_f_matrix pi = plus_gwf_matrix pi 1

gtr_sym n exchange = builtin_gtr_sym (toVector exchange) n

gtr er pi = setReversibility EqRev $ markov (er %*% plus_f_matrix pi') pi' where pi' = toVector pi

-- Probabily we should make a builtin for this
equ n x = gtr_sym n (replicate n_elements x)
    where n_elements = n*(n-1) `div` 2

f81 pi = gtr (equ n 1.0) pi where n = length pi

uniform_frequencies n = replicate n $ 1/fromIntegral n

jukes_cantor n = gtr (equ n 1.0) (uniform_frequencies n)

all_pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

number_pairs n = [ show x ++ "|" ++ show y | (x,y) <- all_pairs [1..n]]

get_element_exchange []                 x y = error ("No exchangeability specified for '" ++ x ++ "'")
get_element_exchange ((key,value):rest) x y = if key == x || key == y then value else get_element_exchange rest x y

getElement []                 x  = error ("No exchangeability specified for '" ++ x ++ "'")
getElement ((key,value):rest) x  = if key == x then value else getElement rest x

gtr_sym_from_numbers n es' = gtr_sym n es where
    npairs = all_pairs [1..n]
    es :: [Double]
    es = if length es' == length npairs then
             [get_element_exchange es' (show l1 ++ "|" ++ show l2) (show l2 ++ "|" ++ show l1)| (l1,l2) <- npairs]
         else
             error $ "Expected "++show (length npairs)++" exchangeabilities but got "++ show (length es')++"!"
