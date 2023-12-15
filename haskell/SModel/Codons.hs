module SModel.Codons (module SModel.Codons) where 

import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.Nucleotides
import Data.Matrix
import qualified Markov
import Markov (getQ, getPi)

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

f3x4'_frequencies a pi1 pi2 pi3 = zip (letters a) (f3x4_frequencies a pi1' pi2' pi3')
    where pi1' = get_ordered_elements nuc_letters pi1 "frequencies"
          pi2' = get_ordered_elements nuc_letters pi2 "frequencies"
          pi3' = get_ordered_elements nuc_letters pi3 "frequencies"
          nuc_letters = letters a_nuc
          a_nuc = getNucleotides a

f1x4_frequencies a pi = f3x4_frequencies a pi pi pi

f1x4'_frequencies a pi = f3x4'_frequencies a pi pi pi

gy94_ext  sym w pi a = gtr a (m0 a sym w) pi

gy94  k w pi a = gy94_ext  sym w pi a where sym = hky85_sym (getNucleotides a) k

mg94_ext a w q = q & x3 a & dNdS w
mg94k a k pi w  = hky85 nuc_a k pi & mg94_ext a w where nuc_a = getNucleotides a
mg94  a   pi w  = f81     pi nuc_a & mg94_ext a w where nuc_a = getNucleotides a

x3x3 a m1 m2 m3 = reversibleMarkov a smap q pi where
    smap = simple_smap a
    q = singlet_to_triplet_rates a (getQ m1) (getQ m2) (getQ m3)
    pi = f3x4_frequencies_builtin a (getPi m1) (getPi m2) (getPi m3)

x3_sym a s = singlet_to_triplet_rates a s s s
x3 a q = x3x3 a q q q

-- maybe this should be t*(q %*% dNdS_matrix) in order to avoid losing scaling factors?  Probably this doesn't matter at the moment.
dNdS omega m@(ReversibleMarkov a s _ r) = reversibleMarkov a s q pi where
    pi = getPi m
    q = (getQ m) %*% dNdS_matrix a omega
