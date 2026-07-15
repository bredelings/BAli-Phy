module Markov where

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import SModel.Rate
import EigenExp
import Reversible

foreign import trcall "SModel:gtr_sym" gtrSymNative :: Vector Double -> Int -> Matrix Double
foreign import trcall "SModel:non_rev_from_vec" nonRevNative :: Int -> Vector Double -> Matrix Double
foreign import trcall "SModel:fixupDiagonalRates" fixupDiagonalNative :: Matrix Double -> Matrix Double
foreign import trcall "SModel:plus_gwf_matrix" plusGwfNative :: Vector Double -> Double -> Matrix Double
foreign import trcall "Matrix:MatrixExp" matrixExpNative :: Matrix Double -> Double -> Matrix Double
foreign import trcall "SModel:equilibriumLimit" equilibriumLimitNative :: Vector Double -> Matrix Double -> Vector Double
foreign import trcall "SModel:checkReversible" checkReversibleNative :: Matrix Double -> Vector Double -> Bool
foreign import trcall "SModel:checkStationary" checkStationaryNative :: Matrix Double -> Vector Double -> Bool
foreign import trcall "SModel:flow" flowNative :: Vector Double -> Matrix Double -> Matrix Double

builtin_gtr_sym exchange dimension = overrideMatrixDims dimension dimension
    (gtrSymNative exchange dimension)

non_rev_from_vec dimension rates = overrideMatrixDims dimension dimension
    (nonRevNative dimension rates)

fixupDiagonalRates matrix = overrideMatrixDims (rows matrix) (cols matrix)
    (fixupDiagonalNative matrix)

plus_gwf_matrix frequencies weight = overrideMatrixDims dimension dimension
    (plusGwfNative frequencies weight)
  where dimension = vectorSize frequencies

mexp matrix time = overrideMatrixDims (rows matrix) (cols matrix)
    (matrixExpNative matrix time)

equilibriumLimit frequencies matrix = overrideVectorSize (vectorSize frequencies)
    (equilibriumLimitNative frequencies matrix)

checkReversible matrix frequencies =
    checkReversibleNative matrix frequencies

checkStationary matrix frequencies =
    checkStationaryNative matrix frequencies

flow frequencies matrix = overrideMatrixDims (rows matrix) (cols matrix)
    (flowNative frequencies matrix)

flux pi q = f - tr f
    where f = flow pi q

relativeFlux pi q = (1/2) * sumElements (abs flux') / (sumElements flow' + 1.0e-12) where
    flow' = flow pi q
    flux' = flow' - tr flow'

-- NOTE: Rates
-- We don't have rates here, because rates require a concept of states being "equal".
-- For cases like markov modulated models, the rate we care about is the rate of switching letters,
--   not the rate of switching states.
-- For codon models, we could care about the rate of switching codons, nucleotides, or amino acids.
--   -- we prefer to scale branch lengths in terms of nucleotide changes.
-- We also only care about the rate at equilibrium.
-- For non-equilibrium models, we only care about the rate at equilibrium.

-- Storing the rate separately means that we don't need to recompute
-- (i) the equilibrium frequencies or (ii) the eigensystem when we are just rescaling.

class Scalable c => CTMC c where
    getQ :: c -> Matrix Double
    getStartFreqs :: c -> Vector Double
    getEqFreqs :: c -> Vector Double
    qExp :: c -> Matrix Double

    getEqFreqs m = equilibriumLimit (getStartFreqs m) (getQ m) 
    qExp m = mexp (getQ m) 1

getNStates m = rows (getQ m)

eqFlow m = flow (getEqFreqs m) (getQ m)

eqFlux :: Markov -> Matrix Double
eqFlux m = flux (getEqFreqs m) (getQ m)           

eqRelFlux m = relativeFlux (getEqFreqs m) (getQ m)           

-- TODO: I should probably hide the constructor to guarantee that rows sum to zero, and frequencies sum to 1.
-- TODO: Should I rename Markov -> CTMC?

-- Fields are: q, start frequencies, rate, possibly eigendecomposition, reversibility
data Markov = Markov (Matrix Double) (Vector Double) Double MatDecomp Reversibility

markov q pi = Markov qFixed pi 1 (NoDecomp Nothing) NonEq where
    qFixed = fixupDiagonalRates q

uniformEquilibriumLimit q = equilibriumLimit pi0 q where
    pi0 = fromList $ replicate n (1/fromIntegral n)
    n = rows q

eqMarkov q = setReversibility EqNonRev $ markov q (uniformEquilibriumLimit q)

non_rev_from_list n rates = non_rev_from_vec n (fromList rates)

instance Scalable Markov where
    scaleBy f (Markov q pi s decomp rev) = Markov q pi (s*f) decomp rev

instance CheckReversible Markov where
    getReversibility (Markov _ _ _ _ r) = r

instance CanMakeReversible Markov where
    setReversibility r1  m@(Markov q f s e r2) | r1 == r2  = m

    setReversibility EqRev (Markov q pi s _ _ ) = Markov q pi s decomp EqRev
        where decomp = case getEigensystem q pi of Just e -> RealEigenDecomp e
                                                   Nothing -> NoDecomp (Just NoDiagReason)
    
    setReversibility r      (Markov q f s _ _ ) = Markov q f s (NoDecomp Nothing) r


instance CTMC Markov where
    getQ  (Markov q _  factor _ _) = scale factor q
    getStartFreqs (Markov _ pi _ _ _) = pi
    qExp   (Markov q _  factor (NoDecomp _) _) = mexp q factor
    qExp   (Markov q pi  factor (RealEigenDecomp eigensys) _) =
        case lExp eigensys pi factor of Just mat -> mat
                                        Nothing -> mexp q factor

    getEqFreqs m | isStationary m = getStartFreqs m
                 | otherwise      = equilibriumLimit (getStartFreqs m) (getQ m)

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

gtr_sym n exchange = builtin_gtr_sym (fromList exchange) n

gtr er pi = setReversibility EqRev $ markov (er * plus_f_matrix pi') pi' where pi' = fromList pi

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
