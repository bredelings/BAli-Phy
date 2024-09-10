module SModel.Codons (module SModel.Codons) where 

import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.Nucleotides
import Data.Matrix
import qualified Markov
import Markov (getQ, getEqFreqs)

type TripletAlphabet = Alphabet
type CodonAlphabet = TripletAlphabet

foreign import bpcall "SModel:" m0 :: CodonAlphabet -> Matrix Double -> Double -> Matrix Double
foreign import bpcall "SModel:f3x4_frequencies" f3x4_frequencies_builtin :: TripletAlphabet -> EVector Double -> EVector Double -> EVector Double -> EVector Double
foreign import bpcall "SModel:" singlet_to_triplet_rates :: TripletAlphabet -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
foreign import bpcall "SModel:" dNdS_matrix :: CodonAlphabet -> Double -> Matrix Double

f3x4_frequencies a pi1 pi2 pi3 = let pi1' = list_to_vector pi1
                                     pi2' = list_to_vector pi2
                                     pi3' = list_to_vector pi3
                                  in list_from_vector $ f3x4_frequencies_builtin a pi1' pi2' pi3'

f3x4'_frequencies a pi1 pi2 pi3 = f3x4_frequencies a pi1' pi2' pi3'
    where pi1' = get_ordered_elements nuc_letters pi1 "frequencies"
          pi2' = get_ordered_elements nuc_letters pi2 "frequencies"
          pi3' = get_ordered_elements nuc_letters pi3 "frequencies"
          nuc_letters = letters a_nuc
          a_nuc = getNucleotides a

plus_f3x4 pi1 pi2 pi3 sym = gtr sym (f3x4'_frequencies a pi1 pi2 pi3) where a = getAlphabet sym

f1x4_frequencies a pi = f3x4_frequencies a pi pi pi

f1x4'_frequencies a pi = f3x4'_frequencies a pi pi pi

plus_f1x4 pi sym = gtr sym (f1x4'_frequencies a pi) where a = getAlphabet sym

gy94 (ExchangeModel nucA nucSym) w code = ExchangeModel codonA (m0 codonA nucSym w)
    where codonA = mkCodons nucA code

mg94_ext w code q = q & x3c code & dNdS w
mg94k a k pi w code = hky85 nuc_a k pi & mg94_ext w code where nuc_a = getNucleotides a
mg94  a   pi w code = f81     pi nuc_a & mg94_ext w code where nuc_a = getNucleotides a

x3x3 m1 m2 m3 = reversible $ markov a smap q pi where
    a = mkTriplets (getAlphabet m1)
    smap = simple_smap a
    q = singlet_to_triplet_rates a (getQ m1) (getQ m2) (getQ m3)
    pi = f3x4_frequencies_builtin a (getEqFreqs m1) (getEqFreqs m2) (getEqFreqs m3)

x3x3c code m1 m2 m3 = reversible $ markov a smap q pi where
    a = mkCodons (getAlphabet m1) code
    smap = simple_smap a
    q = singlet_to_triplet_rates a (getQ m1) (getQ m2) (getQ m3)
    pi = f3x4_frequencies_builtin a (getEqFreqs m1) (getEqFreqs m2) (getEqFreqs m3)

x3_sym (ExchangeModel nucA s) = ExchangeModel a (singlet_to_triplet_rates a s s s)
    where a = mkTriplets nucA

x3c_sym code (ExchangeModel nucA s) = ExchangeModel a (singlet_to_triplet_rates a s s s)
    where a = mkCodons nucA code

x3 q = x3x3 q q q
x3c code q = x3x3c code q q q

-- maybe this should be t*(q %*% dNdS_matrix) in order to avoid losing scaling factors?  Probably this doesn't matter at the moment.
dNdS omega m@(Reversible (Markov a s _ r)) = reversible $ markov a s q pi where
    pi = getEqFreqs m
    q = (getQ m) %*% dNdS_matrix a omega
