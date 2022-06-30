module SModel.Doublets where

import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.Nucleotides

type DoubletAlphabet = Alphabet

foreign import bpcall "SModel:f2x4_frequencies" f2x4_frequencies_builtin :: DoubletAlphabet -> EVector Double -> EVector Double -> EVector Double
foreign import bpcall "SModel:singlet_to_doublet_rates" singlet_to_doublet_rates :: DoubletAlphabet -> Matrix Double -> Matrix Double -> Matrix Double

x2x2 a (ReversibleMarkov _ _ q_1 pi_1 _ _ _) (ReversibleMarkov _ _ q_2 pi_2 _ _ _) =
    let smap = simple_smap a
        q = singlet_to_doublet_rates a q_1 q_2
        pi = f2x4_frequencies_builtin a pi_1 pi_2
    in reversible_markov a smap q pi

x2_sym a s = singlet_to_doublet_rates a s s
x2 a q = x2x2 a q q

foreign import bpcall "SModel:rna_16a_exchange" rna_stem_16a_exchange :: DoubletAlphabet -> Double -> Double -> Double -> Double -> Double -> Matrix Double

rna_stem_16a a aS aD b g e pi = gtr a (rna_stem_16a_exchange a aS aD b g e) pi

foreign import bpcall "SModel:" rna_editting_rates :: DoubletAlphabet -> Matrix Double -> EVector (EPair Int Int) -> Matrix Double
foreign import bpcall "SModel:" rna_editting_pi :: DoubletAlphabet -> EVector Double -> EVector (EPair Int Int) -> EVector Double

rna_editting a (ReversibleMarkov _ _ q_nuc pi_nuc _ _ _) edits = reversible_markov a smap q pi 
    where smap = simple_smap a
          nuc_a = getNucleotides a
          q = rna_editting_rates a q_nuc edits'
          pi = rna_editting_pi a pi_nuc edits'
          edits' = list_to_vector [ c_pair (find_letter nuc_a i) (find_letter nuc_a j) | (i,j) <- edits]
