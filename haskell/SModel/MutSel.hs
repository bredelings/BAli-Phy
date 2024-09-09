module SModel.MutSel where

import Data.Array
import Foreign.Vector
import SModel.ReversibleMarkov
import SModel.Codons
import SModel.Frequency -- for get_ordered_elements
import Bio.Alphabet
import qualified Markov
import Markov (getQ, getEqFreqs)

foreign import bpcall "SModel:" mut_sel_q :: Matrix Double -> EVector Double -> Matrix Double
foreign import bpcall "SModel:" mut_sel_pi :: EVector Double -> EVector Double -> EVector Double

mut_sel ws' m0@(Reversible (Markov a smap _ _)) = reversible $ markov a smap q pi where
    q0 = getQ m0
    pi0 = getEqFreqs m0
    ws = list_to_vector ws'
    q = mut_sel_q q0 ws
    pi = mut_sel_pi pi0 ws

mut_sel' w' q0 = mut_sel w q0 where
    w = get_ordered_elements (letters a) w' "fitnesses"
    a = getAlphabet q0

mut_sel_aa ws q@(Reversible (Markov codonA _ _ _)) = mut_sel (aaToCodon codonA ws) q

mut_sel_aa' ws' q0 = mut_sel_aa ws q0 where
    ws = get_ordered_elements (letters aminoA) ws' "fitnesses"
    codonA = getAlphabet q0
    aminoA = getAminoAcids codonA

fMutSel fitnesses omega code nucModel = nucModel & x3c code & dNdS omega & mut_sel fitnesses

fMutSel' fitnesses' omega code nucModel = fMutSel fitnesses omega code nucModel
    where fitnesses = get_ordered_elements (letters codonA) fitnesses' "fitnesses"
          codonA = mkCodons (getAlphabet nucModel) code

aaToCodon codonA xs = [xsArray!aa | codon <- codons, let aa = translate codonA codon]
    where xsArray = listArray' xs
          codons = take (alphabetSize codonA) [0..]

-- \#1->let {w' = listAray' #1} in \#2 #3->fMutSel #0 codon_w #2 #3
-- The whole body of the function is let-floated up in round 2, and w' is eliminated.
fMutSel0 aaFitnesses omega code nucQ = fMutSel fitnesses omega code nucQ
    where fitnesses = aaToCodon codonA aaFitnesses
          codonA = mkCodons (getAlphabet nucQ) code

fMutSel0' aaFitnesses' omega code nucQ = fMutSel0 aaFitnesses omega code nucQ
    where aaFitnesses = get_ordered_elements (letters aminoA) aaFitnesses' "fitnesses"
          aminoA = getAminoAcids codonA
          codonA = mkCodons (getAlphabet nucQ) code

-- Issue: bad mixing on fMutSel model

    
