module SModel.Codons (module SModel.Codons) where 

import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.Nucleotides
import Data.Matrix
import qualified Markov
import Markov (CTMC(..))

type TripletAlphabet = Alphabet
type CodonAlphabet = TripletAlphabet

foreign import bpcall "SModel:" singletToTripletSym :: TripletAlphabet -> Matrix Double -> Matrix Double
foreign import bpcall "SModel:f3x4_frequencies" f3x4_frequencies_builtin :: TripletAlphabet -> EVector Double -> EVector Double -> EVector Double -> EVector Double
foreign import bpcall "SModel:" singlet_to_triplet_rates :: TripletAlphabet -> Matrix Double -> Matrix Double -> Matrix Double -> Matrix Double
foreign import bpcall "SModel:" multiNucleotideMutationRates :: TripletAlphabet -> Double -> Double -> Matrix Double -> EVector Double -> Matrix Double
foreign import bpcall "SModel:" dNdS_matrix :: CodonAlphabet -> Double -> Matrix Double

f3x4_frequencies a pi1 pi2 pi3 = let pi1' = toVector pi1
                                     pi2' = toVector pi2
                                     pi3' = toVector pi3
                                  in vectorToList $ f3x4_frequencies_builtin a pi1' pi2' pi3'

f3x4'_frequencies a pi1 pi2 pi3 = zip (getLetters a) (f3x4_frequencies a pi1' pi2' pi3')
    where pi1' = get_ordered_elements nucLetters pi1 "frequencies"
          pi2' = get_ordered_elements nucLetters pi2 "frequencies"
          pi3' = get_ordered_elements nucLetters pi3 "frequencies"
          nucLetters = getLetters (getNucleotides a)

f1x4_frequencies a pi = f3x4_frequencies a pi pi pi

f1x4'_frequencies a pi = f3x4'_frequencies a pi pi pi

gy94_ext sym w pi a = gtr a (singletToTripletSym a sym) pi & dNdS w

gy94 k w pi a = gy94_ext  sym w pi a where sym = hky85_sym (getNucleotides a) k

mg94_ext a w q = q & x3 a & dNdS w
mg94k a k pi w  = hky85 nuc_a k pi & mg94_ext a w where nuc_a = getNucleotides a
mg94  a   pi w  = f81     pi nuc_a & mg94_ext a w where nuc_a = getNucleotides a

x3x3 a m1 m2 m3 = reversible $ markov a smap q pi where
    smap = simpleSMap a
    q = singlet_to_triplet_rates a (getQ m1) (getQ m2) (getQ m3)
    pi = f3x4_frequencies_builtin a (getEqFreqs m1) (getEqFreqs m2) (getEqFreqs m3)

x3_sym a s = singlet_to_triplet_rates a s s s
x3 a q = x3x3 a q q q

mnm :: CTMC m => TripletAlphabet -> Double -> Double -> m -> Markov
mnm a v2 v3 model = reversible $ markov a smap q pi where
    smap = simpleSMap a
    q = multiNucleotideMutationRates a v2 v3 (getQ model) (getEqFreqs model)
    pi' = getEqFreqs model
    pi = f3x4_frequencies_builtin a pi' pi' pi'

-- maybe this should be t*(q %*% dNdS_matrix) in order to avoid losing scaling factors?  Probably this doesn't matter at the moment.
dNdS omega m@(Markov a s _ r) = reversible $ markov a s q pi where
    pi = getEqFreqs m
    q = (getQ m) %*% dNdS_matrix a omega
