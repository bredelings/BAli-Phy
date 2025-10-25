module SModel.RNAEdit where

import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.Nucleotides
import qualified Markov
import Markov (getQ, getEqFreqs)
import Probability.Distribution.Mixture
import SModel.Doublets
import Reversible    

type RNAEditAlphabet = Alphabet

foreign import bpcall "SModel:" rna_editting_rates :: RNAEditAlphabet -> Matrix Double -> EVector (EPair Int Int) -> Double -> Matrix Double
foreign import bpcall "SModel:" rna_editting_pi :: RNAEditAlphabet -> EVector Double -> EVector (EPair Int Int) -> EVector Double

siteEdit alphabet nucModel rnaRate edits = setReversibility EqRev $ markov alphabet smap q pi
    where smap = simpleSMap alphabet
          nucs = getNucleotides alphabet
          qNuc = getQ nucModel
          piNuc = getEqFreqs nucModel
          q = rna_editting_rates alphabet qNuc edits' rnaRate
          pi = rna_editting_pi alphabet piNuc edits'
          edits' = toVector [ c_pair (findLetter nucs i) (findLetter nucs j) | (i,j) <- edits]

siteNone a nucModel rnaRate = siteEdit a nucModel rnaRate []
siteC2U  a nucModel rnaRate = siteEdit a nucModel rnaRate [("C","U")]   -- forward
siteU2C  a nucModel rnaRate = siteEdit a nucModel rnaRate [("U","C")]   -- reverse

both a nucModel rnaRate = [ siteNone a nucModel rnaRate,
                            siteC2U  a nucModel rnaRate,
                            siteU2C a nucModel rnaRate]

bothMixture a nucModel rnaRate ps = mixture ps $ both a nucModel rnaRate

--rna_edit_both'' a nuc_model rnaRate sym = 
