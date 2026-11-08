module SModel.RNAEdit where

import Bio.Alphabet
import qualified Data.Map as Map
import SModel.ReversibleMarkov
import SModel.Nucleotides
import qualified Markov
import Markov (getQ, getStartFreqs)
import Probability.Distribution.Mixture
import SModel.Doublets
import Reversible    
import Numeric.LinearAlgebra.Data

type RNAEditAlphabet = Alphabet

foreign import trcall "SModel:rna_editting_rates" rnaEditingRatesNative :: RNAEditAlphabet -> Matrix Double -> EVector (EPair Int Int) -> Double -> Matrix Double
foreign import trcall "SModel:rna_editting_pi" rnaEditingPiNative :: RNAEditAlphabet -> Vector Double -> EVector (EPair Int Int) -> Vector Double

rna_editting_rates alphabet rates edits rate = overrideMatrixDims dimension dimension
    (rnaEditingRatesNative alphabet rates edits rate)
  where dimension = alphabetSize alphabet

rna_editting_pi alphabet frequencies edits = overrideVectorSize (alphabetSize alphabet)
    (rnaEditingPiNative alphabet frequencies edits)

-- Map root frequencies to the editing states. Symmetric rate changes preserve detailed
-- balance, but can change a non-reversible equilibrium. Seed its recalculation with the
-- mapped frequencies to retain the appropriate class masses when the rates are reducible.
siteEdit alphabet nucModel rnaRate edits =
    case rv of
        EqNonRev -> eqMarkovFrom alphabet smap (getLettersVector alphabet) q pi
        _        -> setReversibility rv $ markov alphabet smap (getLettersVector alphabet) q pi
    where rv = getReversibility nucModel
          smap = simpleSMap alphabet
          nucs = getNucleotides alphabet
          qNuc = getQ nucModel
          piNuc = getStartFreqs nucModel
          q = rna_editting_rates alphabet qNuc edits' rnaRate
          pi = rna_editting_pi alphabet piNuc edits'
          edits' = toVector [ c_pair (findLetter nucs i) (findLetter nucs j) | (i,j) <- Map.toAscList edits]

siteNone a nucModel rnaRate = siteEdit a nucModel rnaRate Map.empty
siteC2U  a nucModel rnaRate = siteEdit a nucModel rnaRate (Map.singleton "C" "U")   -- forward
siteU2C  a nucModel rnaRate = siteEdit a nucModel rnaRate (Map.singleton "U" "C")   -- reverse

both a nucModel rnaRate = [ siteNone a nucModel rnaRate,
                            siteC2U  a nucModel rnaRate,
                            siteU2C a nucModel rnaRate]

bothMixture a nucModel rnaRate ps = mixture ps $ both a nucModel rnaRate

--rna_edit_both'' a nuc_model rnaRate sym = 
