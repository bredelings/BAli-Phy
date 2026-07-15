module SModel.Doublets where

import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.Nucleotides
import qualified Markov
import Markov (getQ, getEqFreqs)
import Reversible
import Numeric.LinearAlgebra.Data

type DoubletAlphabet = Alphabet

foreign import trcall "SModel:f2x4_frequencies" f2x4Native :: DoubletAlphabet -> Vector Double -> Vector Double -> Vector Double
foreign import trcall "SModel:singlet_to_doublet_rates" doubletRatesNative :: DoubletAlphabet -> Matrix Double -> Matrix Double -> Matrix Double

f2x4_frequencies_builtin alphabet pi1 pi2 = overrideVectorSize (alphabetSize alphabet)
    (f2x4Native alphabet pi1 pi2)

singlet_to_doublet_rates alphabet rates1 rates2 = overrideMatrixDims dimension dimension
    (doubletRatesNative alphabet rates1 rates2)
  where dimension = alphabetSize alphabet

x2x2 a m1 m2 = setReversibility rv $ markov a smap q pi where
    rv = minimum $ fmap getReversibility [m1,m2]
    smap = simpleSMap a
    q = singlet_to_doublet_rates a (getQ m1) (getQ m2)
    pi = f2x4_frequencies_builtin a (getEqFreqs m1) (getEqFreqs m2)

x2_sym a s = singlet_to_doublet_rates a s s
x2 a q = x2x2 a q q

foreign import trcall "SModel:rna_16a_exchange" rnaStemNative :: DoubletAlphabet -> Double -> Double -> Double -> Double -> Double -> Matrix Double

rna_stem_16a_exchange alphabet aS aD b g e = overrideMatrixDims dimension dimension
    (rnaStemNative alphabet aS aD b g e)
  where dimension = alphabetSize alphabet

rna_stem_16a a aS aD b g e pi = gtr a (rna_stem_16a_exchange a aS aD b g e) pi
