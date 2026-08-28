module SModel.MutSel where

import qualified Data.Vector as V
import qualified Data.Map as Map
import Foreign.Vector
import SModel.ReversibleMarkov
import SModel.Codons
import SModel.Frequency -- for get_ordered_elements
import Bio.Alphabet
import qualified Markov
import Markov (getQ, getEqFreqs)
import Reversible    
import Numeric.LinearAlgebra.Data

foreign import trcall "SModel:mut_sel_q" mutSelQNative :: Matrix Double -> Vector Double -> Matrix Double
foreign import trcall "SModel:mut_sel_pi" mutSelPiNative :: Vector Double -> Vector Double -> Vector Double

-- Subtract the unweighted mean to choose a symmetric representative; the
-- common shift cancels from every pairwise fitness difference.
centerFitnesses :: Map.Map String Double -> Map.Map String Double
centerFitnesses fitnesses
    | Map.null fitnesses = Map.empty
    | otherwise = Map.map (\fitness -> fitness - meanFitness) fitnesses
    where
        meanFitness = Map.foldl (+) 0 fitnesses / fromIntegral (Map.size fitnesses)

-- Apply mutation-selection weights while preserving the rate-matrix shape.
mut_sel_q rates fitness =
    overrideMatrixDims (rows rates) (cols rates) (mutSelQNative rates fitness)

-- Apply mutation-selection weights while preserving the frequency-vector size.
mut_sel_pi frequencies fitness =
    overrideVectorSize (vectorSize frequencies)
        (mutSelPiNative frequencies fitness)

-- MutSel changes Q and pi without changing components or state identities, so
-- carry construction annotations onto the reconstructed numerical model.
mut_sel ws' m0@(Markov a smap _ _ annotations) =
    case setReversibility rv $ markov a smap q pi of
      Markov a' smap' process modelRate _ -> Markov a' smap' process modelRate annotations
  where
    rv = getReversibility m0
    q0 = getQ m0
    pi0 = getEqFreqs m0
    ws = fromList ws'
    q = mut_sel_q q0 ws
    pi = mut_sel_pi pi0 ws

mut_sel' w' q0 = mut_sel w q0 where
    w = get_ordered_elements (getLetters a) w' "fitnesses"
    a = getAlphabet q0

mut_sel_aa ws q@(Markov codon_a _ _ _ _) = mut_sel (aa_to_codon codon_a ws) q

mut_sel_aa' ws' q0 = mut_sel_aa ws q0 where
    ws = get_ordered_elements (getLetters amino_alphabet) ws' "fitnesses"
    codon_alphabet = getAlphabet q0
    amino_alphabet = getAminoAcids codon_alphabet

fMutSel codon_a codon_w omega nuc_model = nuc_model & x3 codon_a & dNdS omega & mut_sel codon_w

fMutSel' codon_a codon_ws' omega nuc_model = fMutSel codon_a codon_ws omega nuc_model
    where codon_ws = get_ordered_elements (getLetters codon_a) codon_ws' "fitnesses"

aa_to_codon codon_a xs = [xs_array V.! aa | codon <- codons, let aa = translate codon_a codon]
    where xs_array = V.fromList xs
          codons = take n_letters [0..]
          n_letters = alphabetSize codon_a

-- \#1->let {w' = listAray' #1} in \#2 #3->fMutSel #0 codon_w #2 #3
-- The whole body of the function is let-floated up in round 2, and w' is eliminated.
fMutSel0 codon_a aa_w omega nuc_q  = fMutSel codon_a codon_w omega nuc_q
    where codon_w = aa_to_codon codon_a aa_w

fMutSel0' codon_a amino_ws' omega nuc_model = fMutSel0 codon_a amino_ws omega nuc_model
                                               where amino_ws = get_ordered_elements (getLetters amino_a) amino_ws' "fitnesses"
                                                     amino_a = getAminoAcids codon_a

-- Issue: bad mixing on fMutSel model

    
