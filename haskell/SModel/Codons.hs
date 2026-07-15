module SModel.Codons (module SModel.Codons) where 

import Bio.Alphabet
import SModel.ReversibleMarkov
import SModel.Property
import SModel.Nucleotides
import Numeric.LinearAlgebra
import qualified Markov
import Markov (CTMC(..))
import Reversible
import Numeric.LinearAlgebra.Data

type TripletAlphabet = Alphabet
type CodonAlphabet = TripletAlphabet

foreign import bpcall "SModel:singletToTripletSym" singletToTripletNative :: TripletAlphabet -> NativeMatrix Double -> NativeMatrix Double
foreign import trcall "SModel:f3x4_frequencies" f3x4Native :: TripletAlphabet -> Vector Double -> Vector Double -> Vector Double -> Vector Double
foreign import bpcall "SModel:singlet_to_triplet_rates" tripletRatesNative :: TripletAlphabet -> NativeMatrix Double -> NativeMatrix Double -> NativeMatrix Double -> NativeMatrix Double
foreign import trcall "SModel:multiNucleotideMutationRates" multiNucleotideNative :: TripletAlphabet -> Double -> Double -> Matrix Double -> Vector Double -> Matrix Double
foreign import bpcall "SModel:dNdS_matrix" dNdSNative :: CodonAlphabet -> Double -> NativeMatrix Double

singletToTripletSym alphabet rates = squareFor alphabet
    (singletToTripletNative alphabet (nativeMatrix rates))

f3x4_frequencies_builtin alphabet pi1 pi2 pi3 = overrideVectorSize (alphabetSize alphabet)
    (f3x4Native alphabet pi1 pi2 pi3)

singlet_to_triplet_rates alphabet rates1 rates2 rates3 = squareFor alphabet
    (tripletRatesNative alphabet (nativeMatrix rates1) (nativeMatrix rates2)
        (nativeMatrix rates3))

multiNucleotideMutationRates alphabet v2 v3 rates frequencies =
    overrideMatrixDims dimension dimension
        (multiNucleotideNative alphabet v2 v3 rates frequencies)
  where dimension = alphabetSize alphabet

dNdS_matrix alphabet omega = squareFor alphabet (dNdSNative alphabet omega)

squareFor alphabet = matrixFromNative dimension dimension
  where dimension = alphabetSize alphabet

f3x4_frequencies a pi1 pi2 pi3 = let pi1' = fromList pi1
                                     pi2' = fromList pi2
                                     pi3' = fromList pi3
                                  in toList $ f3x4_frequencies_builtin a pi1' pi2' pi3'

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

x3x3 a m1 m2 m3 = setReversibility rv $ markov a smap q pi where
    rv = minimum $ fmap getReversibility [m1,m2,m3]
    smap = simpleSMap a
    q = singlet_to_triplet_rates a (getQ m1) (getQ m2) (getQ m3)
    pi = f3x4_frequencies_builtin a (getStartFreqs m1) (getStartFreqs m2) (getStartFreqs m3)

x3_sym a s = singlet_to_triplet_rates a s s s
x3 a q = x3x3 a q q q

mnm a v2 v3 nucModel = setReversibility rv $ markov a smap q pi where
    rv = getReversibility nucModel
    smap = simpleSMap a
    q = multiNucleotideMutationRates a v2 v3 (getQ nucModel) (getEqFreqs nucModel)
    pi' = getStartFreqs nucModel
    pi = f3x4_frequencies_builtin a pi' pi' pi'

-- Add codon dN/dS effects to the rate matrix and tag every codon state with
-- the chosen omega value and whether it represents positive selection.
-- NOTE: maybe this should be t*(q * dNdS_matrix) in order to avoid losing scaling factors?  Probably this doesn't matter at the moment.
dNdS omega m@(Markov a s _ _ _) =
    setConstantStateProperty posSelectionPropertyName posSelection $
    setConstantStateProperty dNdSPropertyName omega $
    setReversibility rv $ markov a s q pi
  where
    rv = getReversibility m
    pi = getStartFreqs m
    q = (getQ m) * dNdS_matrix a omega
    posSelection = if omega > 1 then 1 else 0
