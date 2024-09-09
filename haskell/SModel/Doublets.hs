module SModel.Doublets where

import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.Nucleotides
import qualified Markov
import Markov (getQ, getEqFreqs)

type DoubletAlphabet = Alphabet

foreign import bpcall "SModel:f2x4_frequencies" f2x4_frequencies_builtin :: DoubletAlphabet -> EVector Double -> EVector Double -> EVector Double
foreign import bpcall "SModel:singlet_to_doublet_rates" singlet_to_doublet_rates :: DoubletAlphabet -> Matrix Double -> Matrix Double -> Matrix Double

x2x2 m1 m2 = reversible $ markov a smap q pi where
    a = mkDoublets (getAlphabet m1)
    smap = simple_smap a
    q = singlet_to_doublet_rates a (getQ m1) (getQ m2)
    pi = f2x4_frequencies_builtin a (getEqFreqs m1) (getEqFreqs m2)

x2_sym (ExchangeModel nucA s) = ExchangeModel a (singlet_to_doublet_rates a s s)
    where a = mkDoublets nucA

x2 q = x2x2 q q

foreign import bpcall "SModel:rna_16a_exchange" rna_stem_16a_exchange :: DoubletAlphabet -> Double -> Double -> Double -> Double -> Double -> Matrix Double

rna_stem_16a a aS aD b g e pi = gtr (ExchangeModel a (rna_stem_16a_exchange a aS aD b g e)) pi
